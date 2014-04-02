find_package(RSB "${VERSION_MAJOR}.${VERSION_MINOR}" REQUIRED)

message(STATUS "RSB version: ${RSB_VERSION}")
message(STATUS "RSB libraries: ${RSB_LIBRARIES}")
message(STATUS "RSB runtime library dirs: ${RSB_RUNTIME_LIBRARY_DIRS}")

include_directories(${CMAKE_CURRENT_BINARY_DIR}
                    ${RSB_INCLUDE_DIRS})
protobuf_generate(CPP PROTO_SOURCE PROTO_HEADER
                  PROTOROOT  "${CMAKE_SOURCE_DIR}/data"
                  PROTOFILES "${CMAKE_SOURCE_DIR}/data/Image.proto")

set(PROGRAMS config.cpp event_id.cpp
             informer.cpp            listener.cpp
             server.cpp              client.cpp
             local-introspection.cpp             )

foreach(PROGRAM ${PROGRAMS})
    get_filename_component(BINARY "${PROGRAM}" NAME_WE)
    add_executable("${BINARY}" "${PROGRAM}" "${PROTO_SOURCE}")
    target_link_libraries("${BINARY}" ${Boost_LIBRARIES}
                                      ${PROTOBUF_LIBRARY}
                                      ${RSB_LIBRARIES})
endforeach()
