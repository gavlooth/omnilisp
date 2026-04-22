  auto enable = [&](const std::vector<DECMode>& parameters) {
    std::cout << Set(parameters);
    on_exit_functions.emplace([=] { std::cout << Reset(parameters); });
  };

  auto disable = [&](const std::vector<DECMode>& parameters) {
    std::cout << Reset(parameters);
    on_exit_functions.emplace([=] { std::cout << Set(parameters); });
  };

  if (use_alternative_screen_) {
    enable({
        DECMode::kAlternateScreen,
    });
  }

  disable({
      // DECMode::kCursor,
      DECMode::kLineWrap,
  });

  if (track_mouse_) {
    enable({DECMode::kMouseVt200});
    enable({DECMode::kMouseAnyEvent});
    enable({DECMode::kMouseUrxvtMode});
    enable({DECMode::kMouseSgrExtMode});
  }

  // After installing the new configuration, flush it to the terminal to
  // ensure it is fully applied:
  Flush();

  quit_ = false;
  task_sender_ = task_receiver_->MakeSender();
  event_listener_ =
      std::thread(&EventListener, &quit_, task_receiver_->MakeSender());
  animation_listener_ =
      std::thread(&AnimationListener, &quit_, task_receiver_->MakeSender());
}

// private
void ScreenInteractive::Uninstall() {
  ExitNow();
  event_listener_.join();
  animation_listener_.join();
  OnExit();
}

// private
// NOLINTNEXTLINE
void ScreenInteractive::RunOnceBlocking(Component component) {
  ExecuteSignalHandlers();
  Task task;
  if (task_receiver_->Receive(&task)) {
    HandleTask(component, task);
  }
  RunOnce(component);
}

// private
void ScreenInteractive::RunOnce(Component component) {
  Task task;
  while (task_receiver_->ReceiveNonBlocking(&task)) {
    HandleTask(component, task);
    ExecuteSignalHandlers();
  }
  Draw(std::move(component));

  if (selection_data_previous_ != selection_data_) {
    selection_data_previous_ = selection_data_;
    if (selection_on_change_) {
      selection_on_change_();
      Post(Event::Custom);
    }
  }
}

// private
// NOLINTNEXTLINE
void ScreenInteractive::HandleTask(Component component, Task& task) {
  std::visit(
      [&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;

        // clang-format off
    // Handle Event.
    if constexpr (std::is_same_v<T, Event>) {
      if (arg.is_cursor_position()) {
        cursor_x_ = arg.cursor_x();
        cursor_y_ = arg.cursor_y();
        return;
      }

      if (arg.is_cursor_shape()) {
        cursor_reset_shape_= arg.cursor_shape();
        return;
      }

      if (arg.is_mouse()) {
        arg.mouse().x -= cursor_x_;
        arg.mouse().y -= cursor_y_;
      }

      arg.screen_ = this;

      bool handled = component->OnEvent(arg);

      handled = HandleSelection(handled, arg);

      if (arg == Event::CtrlC && (!handled || force_handle_ctrl_c_)) {
        RecordSignal(SIGABRT);
      }

#if !defined(_WIN32)
      if (arg == Event::CtrlZ && (!handled || force_handle_ctrl_z_)) {
        RecordSignal(SIGTSTP);
      }
#endif

      frame_valid_ = false;
      return;
    }

    // Handle callback
    if constexpr (std::is_same_v<T, Closure>) {
      arg();
      return;
    }

    // Handle Animation
    if constexpr (std::is_same_v<T, AnimationTask>) {
      if (!animation_requested_) {
        return;
      }

      animation_requested_ = false;
      const animation::TimePoint now = animation::Clock::now();
      const animation::Duration delta = now - previous_animation_time_;
      previous_animation_time_ = now;

      animation::Params params(delta);
      component->OnAnimation(params);
      frame_valid_ = false;
      return;
    }
  },
  task);
  // clang-format on
}

// private
bool ScreenInteractive::HandleSelection(bool handled, Event event) {
  if (handled) {
    selection_pending_ = nullptr;
    selection_data_.empty = true;
    selection_ = nullptr;
    return true;
  }

  if (!event.is_mouse()) {
    return false;
  }

  auto& mouse = event.mouse();
  if (mouse.button != Mouse::Left) {
    return false;
  }

  if (mouse.motion == Mouse::Pressed) {
    selection_pending_ = CaptureMouse();
    selection_data_.start_x = mouse.x;
    selection_data_.start_y = mouse.y;
    selection_data_.end_x = mouse.x;
    selection_data_.end_y = mouse.y;
    return false;
  }

  if (!selection_pending_) {
    return false;
  }

  if (mouse.motion == Mouse::Moved) {
    if ((mouse.x != selection_data_.end_x) ||
        (mouse.y != selection_data_.end_y)) {
      selection_data_.end_x = mouse.x;
      selection_data_.end_y = mouse.y;
      selection_data_.empty = false;
    }

    return true;
  }

  if (mouse.motion == Mouse::Released) {
    selection_pending_ = nullptr;
    selection_data_.end_x = mouse.x;
    selection_data_.end_y = mouse.y;
    selection_data_.empty = false;
    return true;
  }

  return false;
}

// private
// NOLINTNEXTLINE
void ScreenInteractive::Draw(Component component) {
  if (frame_valid_) {
    return;
  }
  auto document = component->Render();
  int dimx = 0;
  int dimy = 0;
  auto terminal = Terminal::Size();
  document->ComputeRequirement();
  switch (dimension_) {
    case Dimension::Fixed:
      dimx = dimx_;
      dimy = dimy_;
      break;
    case Dimension::TerminalOutput:
      dimx = terminal.dimx;
      dimy = util::clamp(document->requirement().min_y, 0, terminal.dimy);
      break;
    case Dimension::Fullscreen:
      dimx = terminal.dimx;
      dimy = terminal.dimy;
      break;
    case Dimension::FitComponent:
      dimx = util::clamp(document->requirement().min_x, 0, terminal.dimx);
      dimy = util::clamp(document->requirement().min_y, 0, terminal.dimy);
      break;
  }

  const bool resized = (dimx != dimx_) || (dimy != dimy_);
  ResetCursorPosition();
  std::cout << ResetPosition(/*clear=*/resized);

  // If the terminal width decrease, the terminal emulator will start wrapping
  // lines and make the display dirty. We should clear it completely.
  if ((dimx < dimx_) && !use_alternative_screen_) {
    std::cout << "\033[J";  // clear terminal output
    std::cout << "\033[H";  // move cursor to home position
  }

  // Resize the screen if needed.
  if (resized) {
    dimx_ = dimx;
    dimy_ = dimy;
    pixels_ = std::vector<std::vector<Pixel>>(dimy, std::vector<Pixel>(dimx));
    cursor_.x = dimx_ - 1;
    cursor_.y = dimy_ - 1;
  }

  // Periodically request the terminal emulator the frame position relative to
  // the screen. This is useful for converting mouse position reported in
  // screen's coordinates to frame's coordinates.
#if defined(FTXUI_MICROSOFT_TERMINAL_FALLBACK)
  // Microsoft's terminal suffers from a [bug]. When reporting the cursor
  // position, several output sequences are mixed together into garbage.
  // This causes FTXUI user to see some "1;1;R" sequences into the Input
  // component. See [issue]. Solution is to request cursor position less
  // often. [bug]: https://github.com/microsoft/terminal/pull/7583 [issue]:
  // https://github.com/ArthurSonzogni/FTXUI/issues/136
  static int i = -3;
  ++i;
  if (!use_alternative_screen_ && (i % 150 == 0)) {  // NOLINT
    std::cout << DeviceStatusReport(DSRMode::kCursor);
  }
#else
  static int i = -3;
  ++i;
  if (!use_alternative_screen_ &&
      (previous_frame_resized_ || i % 40 == 0)) {  // NOLINT
    std::cout << DeviceStatusReport(DSRMode::kCursor);
  }
#endif
  previous_frame_resized_ = resized;

  selection_ = selection_data_.empty
                   ? std::make_unique<Selection>()
                   : std::make_unique<Selection>(
                         selection_data_.start_x, selection_data_.start_y,  //
                         selection_data_.end_x, selection_data_.end_y);
  Render(*this, document.get(), *selection_);

  // Set cursor position for user using tools to insert CJK characters.
  {
    const int dx = dimx_ - 1 - cursor_.x + int(dimx_ != terminal.dimx);
    const int dy = dimy_ - 1 - cursor_.y;

    set_cursor_position.clear();
    reset_cursor_position.clear();

    if (dy != 0) {
      set_cursor_position += "\x1B[" + std::to_string(dy) + "A";
      reset_cursor_position += "\x1B[" + std::to_string(dy) + "B";
    }

    if (dx != 0) {
      set_cursor_position += "\x1B[" + std::to_string(dx) + "D";
      reset_cursor_position += "\x1B[" + std::to_string(dx) + "C";
    }

    if (cursor_.shape == Cursor::Hidden) {
      set_cursor_position += "\033[?25l";
    } else {
      set_cursor_position += "\033[?25h";
      set_cursor_position +=
          "\033[" + std::to_string(int(cursor_.shape)) + " q";
    }
  }

  std::cout << ToString() << set_cursor_position;
  Flush();
  Clear();
  frame_valid_ = true;
}

// private
void ScreenInteractive::ResetCursorPosition() {
  std::cout << reset_cursor_position;
  reset_cursor_position = "";
}

/// @brief Return a function to exit the main loop.
/// @ingroup component
Closure ScreenInteractive::ExitLoopClosure() {
  return [this] { Exit(); };
}

/// @brief Exit the main loop.
/// @ingroup component
void ScreenInteractive::Exit() {
  Post([this] { ExitNow(); });
}

// private:
void ScreenInteractive::ExitNow() {
  quit_ = true;
  task_sender_.reset();
}

// private:
void ScreenInteractive::Signal(int signal) {
  if (signal == SIGABRT) {
    Exit();
    return;
  }

// Windows do no support SIGTSTP / SIGWINCH
#if !defined(_WIN32)
  if (signal == SIGTSTP) {
    Post([&] {
      ResetCursorPosition();
      std::cout << ResetPosition(/*clear*/ true);  // Cursor to the beginning
      Uninstall();
      dimx_ = 0;
      dimy_ = 0;
      Flush();
      std::ignore = std::raise(SIGTSTP);
      Install();
    });
    return;
  }

  if (signal == SIGWINCH) {
    Post(Event::Special({0}));
    return;
  }
#endif
}

bool ScreenInteractive::SelectionData::operator==(
    const ScreenInteractive::SelectionData& other) const {
  if (empty && other.empty) {
    return true;
  }
  if (empty || other.empty) {
    return false;
  }
  return start_x == other.start_x && start_y == other.start_y &&
         end_x == other.end_x && end_y == other.end_y;
}

bool ScreenInteractive::SelectionData::operator!=(
    const ScreenInteractive::SelectionData& other) const {
  return !(*this == other);
}
