cmake_minimum_required(VERSION 2.6)
set(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE)

project("RSBIntegrationTest")

set(VERSION_MAJOR "0")
set(VERSION_MINOR "15")

# --- dependency handling ---

set(Boost_USE_VERSION 1.38 CACHE INTERNAL "Boost Version to use")
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_LIBS OFF)
add_definitions(-DBOOST_ALL_DYN_LINK)
if(BOOST_ROOT)
  set(Boost_NO_SYSTEM_PATHS ON)
endif()

find_package(RSC "${VERSION_MAJOR}.${VERSION_MINOR}" REQUIRED)

message(STATUS "RSC version: ${RSC_VERSION}")
message(STATUS "RSC libraries: ${RSC_LIBRARIES}")
message(STATUS "RSC runtime library dirs: ${RSC_RUNTIME_LIBRARY_DIRS}")
message(STATUS "RSC CMake module path: ${RSC_CMAKE_MODULE_PATH}")

# Use CMake modules in additional locations.
# Has to be done after RSC configuration, since RSC updates CMake's
# module path.
set(PROJECT_CMAKE_PATH ${CMAKE_INSTALL_PREFIX}/share/cmake/Modules)
set(CMAKE_MODULE_PATH "${PROJECT_CMAKE_PATH}" "${CMAKE_MODULE_PATH}" "${RSC_CMAKE_MODULE_PATH}")

# May depend on updated CMake module path.
find_package(Boost ${Boost_USE_VERSION} REQUIRED thread system filesystem)
find_package(BoostUUID REQUIRED)

find_package(ProtocolBuffers REQUIRED)

# Provided by RSC.
include(PedanticCompilerWarnings)

include_directories(BEFORE SYSTEM
                    ${Boost_INCLUDE_DIRS}
                    ${BOOSTUUID_INCLUDE_DIRS}
                    ${PROTOBUF_INCLUDE_DIRS}
                    ${RSC_INCLUDE_DIRS}
                    ${RSB_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

# --- source code ---

add_subdirectory(cpp)
add_subdirectory(python)
add_subdirectory(java)
add_subdirectory(lisp)
