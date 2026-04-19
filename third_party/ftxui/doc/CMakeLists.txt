if(NOT FTXUI_BUILD_DOCS)
  return()
endif()

find_package(Doxygen)
if (NOT DOXYGEN_FOUND)
  message("Doxygen need to be installed to generate the doxygen documentation")
  return()
endif()

# Generate example list for documentation
set(EXAMPLE_LIST "${CMAKE_CURRENT_BINARY_DIR}/example_list.md")
file(WRITE ${EXAMPLE_LIST} "# Examples")
get_property(EXAMPLES GLOBAL PROPERTY FTXUI::EXAMPLES)
foreach(EXAMPLE IN LISTS EXAMPLES)
  file(APPEND ${EXAMPLE_LIST} "\n@example examples/${EXAMPLE}.cpp")
endforeach(EXAMPLE IN LISTS EXAMPLES)

set(DOXYFILE_PART_DIR "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile.parts")
file(MAKE_DIRECTORY ${DOXYFILE_PART_DIR})
foreach(DOXYFILE_PART IN ITEMS part-01.txt part-02.txt part-03.txt part-04.txt)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in.parts/${DOXYFILE_PART}"
    "${DOXYFILE_PART_DIR}/${DOXYFILE_PART}"
    @ONLY
  )
endforeach()

configure_file(Doxyfile.in Doxyfile @ONLY)

# note the option ALL which allows to build the docs together with the application
add_custom_target(doc
  COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API documentation with Doxygen"
  VERBATIM
  )
