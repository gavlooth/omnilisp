TEST(TableTest, RowFlexTwo) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectRow(1).Decorate(flex);
  table.SelectRow(3).Decorate(flex);
  Screen screen(10, 20);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬─┬─┬─┐ \r\n"
      "│a│b│c│d│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│e│f│g│h│ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│i│j│k│l│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│m│n│o│p│ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "└─┴─┴─┴─┘ ",
      screen.ToString());
}

TEST(TableTest, Merge) {
  auto table = Table({
      {"a", "b", "c"},
      {"d", "e", "f"},
      {"g", "h", "i"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectColumn(1).Border(HEAVY);
  table.SelectRow(1).Border(HEAVY);
  Screen screen(7, 7);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┲━┱─┐\r\n"
      "│a┃b┃c│\r\n"
      "┢━╋━╋━┪\r\n"
      "┃d┃e┃f┃\r\n"
      "┡━╋━╋━┩\r\n"
      "│g┃h┃i│\r\n"
      "└─┺━┹─┘",
      screen.ToString());
}

TEST(TableTest, Issue912) {
  Table({
      {"a"},
  });
  Table({
      {"a", "b"},
  });
  Table({
      {"a", "b", "c"},
  });
}
