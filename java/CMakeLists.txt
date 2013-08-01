set(JAR_FILE "${CMAKE_CURRENT_BINARY_DIR}/rsbintegration.jar")

find_program(ANT_EXECUTABLE ant)
if(NOT PROTOBUF_PROTOC_EXECUTABLE)
    message(FATAL_ERROR "protoc executable not found")
endif()
if(NOT PROTOBUF_JAVA_LIBRARY)
    message(FATAL_ERROR "protobuf java library not found")
endif()

# find rsb java file
find_file(RSB_JAVA_LIBRARY
          NAMES rsb.jar ${JAVA_NAME}
          HINTS ${JAVA_ROOT}
                "${CMAKE_INSTALL_PREFIX}/lib/java"
                "${CMAKE_INSTALL_PREFIX}/share/java"
                "/usr/share/java")
if(NOT RSB_JAVA_LIBRARY)
    message(SEND_ERROR "Could not find rsb.jar (or the value of JAVA_NAME). Please define JAVA_ROOT with a folder containing the library.")
endif()

protobuf_generate(JAVA JAVA_PROTO_SOURCE
                  PROTOROOT  "${CMAKE_SOURCE_DIR}/data"
                  PROTOFILES "${CMAKE_SOURCE_DIR}/data/Image.proto")

set(CONFIGURE_FILES build.xml
                    config.sh event_id.sh
                    listener.sh informer.sh
                    client.sh   server.sh)
foreach(FILE ${CONFIGURE_FILES})
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/${FILE}.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${FILE}"
                   @ONLY)
endforeach()

set(JAVA_FILES config.java event_id.java
               listener.java informer.java
               client.java   server.java)
add_custom_command(OUTPUT  ${JAR_FILE}
                   COMMAND ${ANT_EXECUTABLE} dist
                   DEPENDS "${JAVA_FILES}"
                           "${JAVA_PROTO_SOURCE}"
                           "${CMAKE_CURRENT_BINARY_DIR}/build.xml"
                   COMMENT "Creating java jar file")
add_custom_target(java ALL DEPENDS ${JAR_FILE})
