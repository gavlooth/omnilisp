  input->OnEvent(Event::Character("测"));
  input->OnEvent(Event::Character("试"));
  input->OnEvent(Event::Character("a⃒"));
  input->OnEvent(Event::Character("ā"));
  input->OnEvent(Event::Return);
  input->OnEvent(Event::Character("测"));
  input->OnEvent(Event::Character("试"));
  input->OnEvent(Event::Character("a⃒"));
  input->OnEvent(Event::Character("ā"));

  EXPECT_EQ(cursor_position, 27u);

  auto render = [&] {
    auto document = input->Render();
    auto screen = Screen::Create(Dimension::Fixed(100), Dimension::Fixed(4));
    Render(screen, document);
  };
  render();

  Mouse mouse;
  mouse.button = Mouse::Button::Left;
  mouse.motion = Mouse::Motion::Pressed;
  mouse.shift = false;
  mouse.meta = false;
  mouse.control = false;

  mouse.x = 0;
  mouse.y = 0;
  EXPECT_TRUE(input->OnEvent(Event::Mouse("", mouse)));
  render();
  EXPECT_EQ(cursor_position, 0);

  mouse.x = 0;
  mouse.y = 1;
  EXPECT_TRUE(input->OnEvent(Event::Mouse("", mouse)));
  render();
  EXPECT_EQ(cursor_position, 14);

  mouse.x = 1;
  mouse.y = 0;
  EXPECT_TRUE(input->OnEvent(Event::Mouse("", mouse)));
  render();
  EXPECT_EQ(cursor_position, 3);

  mouse.x = 1;
  mouse.y = 1;
  EXPECT_TRUE(input->OnEvent(Event::Mouse("", mouse)));
  render();
  EXPECT_EQ(cursor_position, 17);
}

TEST(InputTest, OnEnter) {
  std::string content;
  auto option = InputOption();
  bool on_enter_called = false;
  option.on_enter = [&] { on_enter_called = true; };
  Component input = Input(&content, option);

  EXPECT_FALSE(on_enter_called);
  EXPECT_TRUE(input->OnEvent(Event::Return));
  EXPECT_TRUE(on_enter_called);
}

TEST(InputTest, InsertMode) {
  std::string content = "abc\nefg";
  bool insert = true;
  int cursor_position = 1;
  Component input = Input({
      .content = &content,
      .insert = &insert,
      .cursor_position = &cursor_position,
  });

  EXPECT_TRUE(insert);
  EXPECT_TRUE(input->OnEvent(Event::Insert));
  EXPECT_FALSE(insert);

  EXPECT_EQ(content, "abc\nefg");
  EXPECT_TRUE(input->OnEvent(Event::Character('x')));
  EXPECT_EQ(content, "axc\nefg");
  EXPECT_TRUE(input->OnEvent(Event::Character('y')));
  EXPECT_EQ(content, "axy\nefg");
  EXPECT_TRUE(input->OnEvent(Event::Character('z')));
  EXPECT_EQ(content, "axyz\nefg");

  EXPECT_TRUE(input->OnEvent(Event::ArrowDown));
  EXPECT_EQ(content, "axyz\nefg");
  EXPECT_TRUE(input->OnEvent(Event::Character('X')));
  EXPECT_EQ(content, "axyz\nefgX");
}
