\mainpage

# Introduction {#introduction}

Welcome to the FTXUI documentation!

This is a brief tutorial. You are also encouraged to self-learn by reading the
[examples](./examples.html).

@tableofcontents

**Short example**

To build a single frame, you need create an `ftxui::Element`, and display it on
a `ftxui::Screen`.

**main.cpp**
```cpp
#include <ftxui/dom/elements.hpp>
#include <ftxui/screen/screen.hpp>
#include <iostream>

int main(void) {
  using namespace ftxui;

  // Define the document
  Element document =
    hbox({
      text("left")   | border,
      text("middle") | border | flex,
      text("right")  | border,
    });

  auto screen = Screen::Create(
    Dimension::Full(),       // Width
    Dimension::Fit(document) // Height
  );
  Render(screen, document);
  screen.Print();

  return EXIT_SUCCESS;
}
```


**output**
```bash
┌────┐┌────────────────────────────────────┐┌─────┐
│left││middle                              ││right│
└────┘└────────────────────────────────────┘└─────┘
```

## Configure {#configure}
### Using CMake and find_package {#build-cmake-find-package}

Assuming FTXUI is available or installed on the system.

**CMakeLists.txt**
```cmake
cmake_minimum_required (VERSION 3.11)
find_package(ftxui 5 REQUIRED)
project(ftxui-starter LANGUAGES CXX VERSION 1.0.0)
add_executable(ftxui-starter src/main.cpp)
target_link_libraries(ftxui-starter
  PRIVATE ftxui::screen
  PRIVATE ftxui::dom
  PRIVATE ftxui::component # Not needed for this example.
)

```

### Using CMake and FetchContent {#build-cmake}

If you want to fetch FTXUI using cmake:

**CMakeLists.txt**
```cmake
cmake_minimum_required (VERSION 3.11)

include(FetchContent)
set(FETCHCONTENT_UPDATES_DISCONNECTED TRUE)
FetchContent_Declare(ftxui
  GIT_REPOSITORY https://github.com/ArthurSonzogni/ftxui
  GIT_TAG main # Important: Specify a version or a commit hash here.
)
FetchContent_MakeAvailable(ftxui)

project(ftxui-starter LANGUAGES CXX VERSION 1.0.0)
add_executable(ftxui-starter src/main.cpp)
target_link_libraries(ftxui-starter
  PRIVATE ftxui::screen
  PRIVATE ftxui::dom
  PRIVATE ftxui::component # Not needed for this example.
)
```

## Build

```bash
mkdir build && cd build
cmake ..
make
./main
```
