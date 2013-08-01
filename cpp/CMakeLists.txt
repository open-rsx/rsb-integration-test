FIND_PACKAGE(RSB REQUIRED)
MESSAGE(STATUS "RSB version: ${RSB_VERSION}")
MESSAGE(STATUS "RSB libraries: ${RSB_LIBRARIES}")
MESSAGE(STATUS "RSB runtime library dirs: ${RSB_RUNTIME_LIBRARY_DIRS}")

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}
                    ${RSB_INCLUDE_DIRS})
PROTOBUF_GENERATE(CPP PROTO_SOURCE PROTO_HEADER
                  PROTOROOT  "${CMAKE_SOURCE_DIR}/data"
                  PROTOFILES "${CMAKE_SOURCE_DIR}/data/Image.proto")

SET(PROGRAMS config.cpp event_id.cpp
             informer.cpp listener.cpp
             server.cpp   client.cpp)

FOREACH(PROGRAM ${PROGRAMS})
    GET_FILENAME_COMPONENT(BINARY "${PROGRAM}" NAME_WE)
    ADD_EXECUTABLE("${BINARY}" "${PROGRAM}" "${PROTO_SOURCE}")
    TARGET_LINK_LIBRARIES("${BINARY}" ${Boost_LIBRARIES}
                                      ${PROTOBUF_LIBRARY}
                                      ${RSB_LIBRARIES})
ENDFOREACH()
