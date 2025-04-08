cmake_minimum_required(VERSION 3.27.9)
message(STATUS "Using Cmake version: ${CMAKE_VERSION}")
project(NUM_METHODS VERSION 0.0)

add_subdirectory(labs)
message("Collected labs: ${LAB_LIST}")

foreach(LAB ${LAB_LIST})
  target_include_directories(${LAB} PRIVATE ${CMAKE_SOURCE_DIR}/include)
  target_include_directories(${LAB} PRIVATE ${CMAKE_SOURCE_DIR}/include/${LAB})
  target_compile_features(${LAB} PRIVATE cxx_std_20)
endforeach(LAB ${LAB_LIST})
