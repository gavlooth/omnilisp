// Copyright 2021 Arthur Sonzogni. All rights reserved.
// Use of this source code is governed by the MIT license that can be found in
// the LICENSE file.
#include <gtest/gtest.h>

#include "ftxui/dom/elements.hpp"  // for LIGHT, flex, center, EMPTY, DOUBLE
#include "ftxui/dom/node.hpp"      // for Render
#include "ftxui/dom/table.hpp"
#include "ftxui/screen/screen.hpp"  // for Screen

// NOLINTBEGIN
namespace ftxui {

TEST(TableTest, Empty) {
  auto table = Table();
  Screen screen(5, 5);
  Render(screen, table.Render());
  EXPECT_EQ(
      "     \r\n"
      "     \r\n"
      "     \r\n"
      "     \r\n"
      "     ",
      screen.ToString());
}

TEST(TableTest, Basic) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "abcd      \r\n"
      "efgh      \r\n"
      "ijkl      \r\n"
      "mnop      \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorVerticalEmpty) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorVertical(EMPTY);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a b c d   \r\n"
      "e f g h   \r\n"
      "i j k l   \r\n"
      "m n o p   \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorHorizontalEmpty) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorHorizontal(EMPTY);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "abcd      \r\n"
      "          \r\n"
      "efgh      \r\n"
      "          \r\n"
      "ijkl      \r\n"
      "          \r\n"
      "mnop      \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorHorizontalLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorHorizontal(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "abcd      \r\n"
      "────      \r\n"
      "efgh      \r\n"
      "────      \r\n"
      "ijkl      \r\n"
      "────      \r\n"
      "mnop      \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorVerticalLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorVertical(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a│b│c│d   \r\n"
      "e│f│g│h   \r\n"
      "i│j│k│l   \r\n"
      "m│n│o│p   \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Separator(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a│b│c│d   \r\n"
      "─┼─┼─┼─   \r\n"
      "e│f│g│h   \r\n"
      "─┼─┼─┼─   \r\n"
      "i│j│k│l   \r\n"
      "─┼─┼─┼─   \r\n"
      "m│n│o│p   \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorVerticalHorizontalLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorVertical(LIGHT);
  table.SelectAll().SeparatorHorizontal(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a│b│c│d   \r\n"
      "─┼─┼─┼─   \r\n"
      "e│f│g│h   \r\n"
      "─┼─┼─┼─   \r\n"
      "i│j│k│l   \r\n"
      "─┼─┼─┼─   \r\n"
      "m│n│o│p   \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SeparatorHorizontalVerticalLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().SeparatorHorizontal(LIGHT);
  table.SelectAll().SeparatorVertical(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a│b│c│d   \r\n"
      "─┼─┼─┼─   \r\n"
      "e│f│g│h   \r\n"
      "─┼─┼─┼─   \r\n"
      "i│j│k│l   \r\n"
      "─┼─┼─┼─   \r\n"
      "m│n│o│p   \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, BorderLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌────┐    \r\n"
      "│abcd│    \r\n"
      "│efgh│    \r\n"
      "│ijkl│    \r\n"
      "│mnop│    \r\n"
      "└────┘    \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, BorderSeparatorLight) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬─┬─┬─┐ \r\n"
      "│a│b│c│d│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│e│f│g│h│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│i│j│k│l│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│m│n│o│p│ \r\n"
      "└─┴─┴─┴─┘ \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectRow) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRow(1).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " abcd     \r\n"
      "┌────┐    \r\n"
      "│efgh│    \r\n"
      "└────┘    \r\n"
      " ijkl     \r\n"
      " mnop     \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectRowNegative) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRow(-2).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " abcd     \r\n"
      " efgh     \r\n"
      "┌────┐    \r\n"
      "│ijkl│    \r\n"
      "└────┘    \r\n"
      " mnop     \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectColumn) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectColumn(1).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " ┌─┐      \r\n"
      "a│b│cd    \r\n"
      "e│f│gh    \r\n"
      "i│j│kl    \r\n"
      "m│n│op    \r\n"
      " └─┘      \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectColumnNegative) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectColumn(-2).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "  ┌─┐     \r\n"
      "ab│c│d    \r\n"
      "ef│g│h    \r\n"
      "ij│k│l    \r\n"
      "mn│o│p    \r\n"
      "  └─┘     \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, CrossingBorders) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRow(1).Border(LIGHT);
  table.SelectColumn(1).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "  ┌─┐     \r\n"
      " a│b│cd   \r\n"
      "┌─┼─┼──┐  \r\n"
      "│e│f│gh│  \r\n"
      "└─┼─┼──┘  \r\n"
      " i│j│kl   \r\n"
      " m│n│op   \r\n"
      "  └─┘     \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, CrossingBordersLightAndDouble) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRow(1).Border(LIGHT);
  table.SelectColumn(1).Border(DOUBLE);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "  ╔═╗     \r\n"
      " a║b║cd   \r\n"
      "┌─╫─╫──┐  \r\n"
      "│e║f║gh│  \r\n"
      "└─╫─╫──┘  \r\n"
      " i║j║kl   \r\n"
      " m║n║op   \r\n"
      "  ╚═╝     \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectColumns) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectColumns(1, 2).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " ┌──┐     \r\n"
      "a│bc│d    \r\n"
      "e│fg│h    \r\n"
      "i│jk│l    \r\n"
      "m│no│p    \r\n"
      " └──┘     \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectRows) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRows(1, 2).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " abcd     \r\n"
      "┌────┐    \r\n"
      "│efgh│    \r\n"
      "│ijkl│    \r\n"
      "└────┘    \r\n"
      " mnop     \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectRectangle) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectRectangle(1, 2, 1, 2).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "a bc d    \r\n"
      " ┌──┐     \r\n"
      "e│fg│h    \r\n"
      "i│jk│l    \r\n"
      " └──┘     \r\n"
      "m no p    \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectColumnsNegative) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectColumns(1, -1).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " ┌───┐    \r\n"
      "a│bcd│    \r\n"
      "e│fgh│    \r\n"
      "i│jkl│    \r\n"
      "m│nop│    \r\n"
      " └───┘    \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, SelectInverted) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectColumns(-1, 1).Border(LIGHT);
  Screen screen(10, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      " ┌───┐    \r\n"
      "a│bcd│    \r\n"
      "e│fgh│    \r\n"
      "i│jkl│    \r\n"
      "m│nop│    \r\n"
      " └───┘    \r\n"
      "          \r\n"
      "          \r\n"
      "          \r\n"
      "          ",
      screen.ToString());
}

TEST(TableTest, ColumnFlex) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectColumn(1).Decorate(flex);
  Screen screen(20, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬────────────┬─┬─┐\r\n"
      "│a│b           │c│d│\r\n"
      "├─┼────────────┼─┼─┤\r\n"
      "│e│f           │g│h│\r\n"
      "├─┼────────────┼─┼─┤\r\n"
      "│i│j           │k│l│\r\n"
      "├─┼────────────┼─┼─┤\r\n"
      "│m│n           │o│p│\r\n"
      "└─┴────────────┴─┴─┘\r\n"
      "                    ",
      screen.ToString());
}

TEST(TableTest, ColumnFlexCenter) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectColumn(1).Decorate(flex);
  table.SelectColumn(1).DecorateCells(center);
  Screen screen(20, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬─┬─┬─┐           \r\n"
      "│a│b│c│d│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│e│f│g│h│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│i│j│k│l│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│m│n│o│p│           \r\n"
      "└─┴─┴─┴─┘           \r\n"
      "                    ",
      screen.ToString());
}

TEST(TableTest, ColumnCenter) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectColumn(1).DecorateCells(center);
  Screen screen(20, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬─┬─┬─┐           \r\n"
      "│a│b│c│d│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│e│f│g│h│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│i│j│k│l│           \r\n"
      "├─┼─┼─┼─┤           \r\n"
      "│m│n│o│p│           \r\n"
      "└─┴─┴─┴─┘           \r\n"
      "                    ",
      screen.ToString());
}

TEST(TableTest, ColumnFlexTwo) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectColumn(1).Decorate(flex);
  table.SelectColumn(3).Decorate(flex);
  Screen screen(20, 10);
  Render(screen, table.Render());
  EXPECT_EQ(
      "┌─┬──────┬─┬───────┐\r\n"
      "│a│b     │c│d      │\r\n"
      "├─┼──────┼─┼───────┤\r\n"
      "│e│f     │g│h      │\r\n"
      "├─┼──────┼─┼───────┤\r\n"
      "│i│j     │k│l      │\r\n"
      "├─┼──────┼─┼───────┤\r\n"
      "│m│n     │o│p      │\r\n"
      "└─┴──────┴─┴───────┘\r\n"
      "                    ",
      screen.ToString());
}

TEST(TableTest, RowFlex) {
  auto table = Table({
      {"a", "b", "c", "d"},
      {"e", "f", "g", "h"},
      {"i", "j", "k", "l"},
      {"m", "n", "o", "p"},
  });
  table.SelectAll().Border(LIGHT);
  table.SelectAll().Separator(LIGHT);
  table.SelectRow(1).Decorate(flex);
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
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "│ │ │ │ │ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│i│j│k│l│ \r\n"
      "├─┼─┼─┼─┤ \r\n"
      "│m│n│o│p│ \r\n"
      "└─┴─┴─┴─┘ ",
      screen.ToString());
}
