  do {  // NOLINT
    DrawBlock(x1 - x, 2 * (y1 + y), true, s);
    DrawBlock(x1 + x, 2 * (y1 + y), true, s);
    DrawBlock(x1 + x, 2 * (y1 - y), true, s);
    DrawBlock(x1 - x, 2 * (y1 - y), true, s);
    e2 = 2 * err;
    if (e2 >= dx) {
      x++;
      err += dx += 2 * r2 * r2;
    }
    if (e2 <= dy) {
      y++;
      err += dy += 2 * r1 * r1;
    }
  } while (x <= 0);

  while (y++ < r2) {
    DrawBlock(x1, 2 * (y1 + y), true, s);
    DrawBlock(x1, 2 * (y1 - y), true, s);
  }
}

/// @brief Draw a filled ellipse made of block characters.
/// @param x the x coordinate of the center of the ellipse.
/// @param y the y coordinate of the center of the ellipse.
/// @param r1 the radius of the ellipse along the x axis.
/// @param r2 the radius of the ellipse along the y axis.
void Canvas::DrawBlockEllipseFilled(int x, int y, int r1, int r2) {
  DrawBlockEllipseFilled(x, y, r1, r2, nostyle);
}

/// @brief Draw a filled ellipse made of block characters.
/// @param x the x coordinate of the center of the ellipse.
/// @param y the y coordinate of the center of the ellipse.
/// @param r1 the radius of the ellipse along the x axis.
/// @param r2 the radius of the ellipse along the y axis.
/// @param color the color of the ellipse.
void Canvas::DrawBlockEllipseFilled(int x,
                                    int y,
                                    int r1,
                                    int r2,
                                    const Color& color) {
  DrawBlockEllipseFilled(x, y, r1, r2,
                         [color](Pixel& p) { p.foreground_color = color; });
}

/// @brief Draw a filled ellipse made of block characters.
/// @param x1 the x coordinate of the center of the ellipse.
/// @param y1 the y coordinate of the center of the ellipse.
/// @param r1 the radius of the ellipse along the x axis.
/// @param r2 the radius of the ellipse along the y axis.
/// @param s the style of the ellipse.
void Canvas::DrawBlockEllipseFilled(int x1,
                                    int y1,
                                    int r1,
                                    int r2,
                                    const Stylizer& s) {
  y1 /= 2;
  r2 /= 2;
  int x = -r1;
  int y = 0;
  int e2 = r2;
  int dx = (1 + 2 * x) * e2 * e2;
  int dy = x * x;
  int err = dx + dy;

  do {  // NOLINT
    for (int xx = x1 + x; xx <= x1 - x; ++xx) {
      DrawBlock(xx, 2 * (y1 + y), true, s);
      DrawBlock(xx, 2 * (y1 - y), true, s);
    }
    e2 = 2 * err;
    if (e2 >= dx) {
      x++;
      err += dx += 2 * r2 * r2;
    }
    if (e2 <= dy) {
      y++;
      err += dy += 2 * r1 * r1;
    }
  } while (x <= 0);

  while (y++ < r2) {
    for (int yy = y1 + y; yy <= y1 - y; ++yy) {
      DrawBlock(x1, 2 * yy, true, s);
    }
  }
}

/// @brief Draw a piece of text.
/// @param x the x coordinate of the text.
/// @param y the y coordinate of the text.
/// @param value the text to draw.
void Canvas::DrawText(int x, int y, const std::string& value) {
  DrawText(x, y, value, nostyle);
}

/// @brief Draw a piece of text.
/// @param x the x coordinate of the text.
/// @param y the y coordinate of the text.
/// @param value the text to draw.
/// @param color the color of the text.
void Canvas::DrawText(int x,
                      int y,
                      const std::string& value,
                      const Color& color) {
  DrawText(x, y, value, [color](Pixel& p) { p.foreground_color = color; });
}

/// @brief Draw a piece of text.
/// @param x the x coordinate of the text.
/// @param y the y coordinate of the text.
/// @param value the text to draw.
/// @param style the style of the text.
void Canvas::DrawText(int x,
                      int y,
                      const std::string& value,
                      const Stylizer& style) {
  for (const auto& it : Utf8ToGlyphs(value)) {
    if (!IsIn(x, y)) {
      x += 2;
      continue;
    }
    Cell& cell = storage_[XY{x / 2, y / 4}];
    cell.type = CellType::kCell;
    cell.content.character = it;
    style(cell.content);
    x += 2;
  }
}

/// @brief Directly draw a predefined pixel at the given coordinate
/// @param x the x coordinate of the pixel.
/// @param y the y coordinate of the pixel.
/// @param p the pixel to draw.
void Canvas::DrawPixel(int x, int y, const Pixel& p) {
  Cell& cell = storage_[XY{x / 2, y / 4}];
  cell.type = CellType::kCell;
  cell.content = p;
}

/// @brief Draw a predefined image, with top-left corner at the given coordinate
///   You can supply negative coordinates to align the image however you like -
///   only the 'visible' portion will be drawn
/// @param x the x coordinate corresponding to the top-left corner of the image.
/// @param y the y coordinate corresponding to the top-left corner of the image.
/// @param image the image to draw.
void Canvas::DrawImage(int x, int y, const Image& image) {
  x /= 2;
  y /= 4;
  const int dx_begin = std::max(0, -x);
  const int dy_begin = std::max(0, -y);
  const int dx_end = std::min(image.dimx(), width_ - x);
  const int dy_end = std::min(image.dimy(), height_ - y);

  for (int dy = dy_begin; dy < dy_end; ++dy) {
    for (int dx = dx_begin; dx < dx_end; ++dx) {
      Cell& cell = storage_[XY{
          x + dx,
          y + dy,
      }];
      cell.type = CellType::kCell;
      cell.content = image.PixelAt(dx, dy);
    }
  }
}

/// @brief Modify a pixel at a given location.
/// @param style a function that modifies the pixel.
void Canvas::Style(int x, int y, const Stylizer& style) {
  if (IsIn(x, y)) {
    style(storage_[XY{x / 2, y / 4}].content);
  }
}

namespace {

class CanvasNodeBase : public Node {
 public:
  CanvasNodeBase() = default;

  void Render(Screen& screen) override {
    const Canvas& c = canvas();
    const int y_max = std::min(c.height() / 4, box_.y_max - box_.y_min + 1);
    const int x_max = std::min(c.width() / 2, box_.x_max - box_.x_min + 1);
    for (int y = 0; y < y_max; ++y) {
      for (int x = 0; x < x_max; ++x) {
        screen.PixelAt(box_.x_min + x, box_.y_min + y) = c.GetPixel(x, y);
      }
    }
  }

  virtual const Canvas& canvas() = 0;
};

}  // namespace

/// @brief Produce an element from a Canvas, or a reference to a Canvas.
// NOLINTNEXTLINE
Element canvas(ConstRef<Canvas> canvas) {
  class Impl : public CanvasNodeBase {
   public:
    explicit Impl(ConstRef<Canvas> canvas) : canvas_(std::move(canvas)) {
      requirement_.min_x = (canvas_->width() + 1) / 2;
      requirement_.min_y = (canvas_->height() + 3) / 4;
    }
    const Canvas& canvas() final { return *canvas_; }
    ConstRef<Canvas> canvas_;
  };
  return std::make_shared<Impl>(canvas);
}

/// @brief Produce an element drawing a canvas of requested size.
/// @param width the width of the canvas.
/// @param height the height of the canvas.
/// @param fn a function drawing the canvas.
Element canvas(int width, int height, std::function<void(Canvas&)> fn) {
  class Impl : public CanvasNodeBase {
   public:
    Impl(int width, int height, std::function<void(Canvas&)> fn)
        : width_(width), height_(height), fn_(std::move(fn)) {}

    void ComputeRequirement() final {
      requirement_.min_x = (width_ + 1) / 2;
      requirement_.min_y = (height_ + 3) / 4;
    }

    void Render(Screen& screen) final {
      const int width = (box_.x_max - box_.x_min + 1) * 2;
      const int height = (box_.y_max - box_.y_min + 1) * 4;
      canvas_ = Canvas(width, height);
      fn_(canvas_);
      CanvasNodeBase::Render(screen);
    }

    const Canvas& canvas() final { return canvas_; }
    Canvas canvas_;
    int width_;
    int height_;
    std::function<void(Canvas&)> fn_;
  };
  return std::make_shared<Impl>(width, height, std::move(fn));
}

/// @brief Produce an element drawing a canvas.
/// @param fn a function drawing the canvas.
Element canvas(std::function<void(Canvas&)> fn) {
  const int default_dim = 12;
  return canvas(default_dim, default_dim, std::move(fn));
}
