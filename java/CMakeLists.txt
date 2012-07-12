SET(JAR_FILE "${CMAKE_CURRENT_BINARY_DIR}/rsbintegration.jar")

FIND_PROGRAM(ANT_EXECUTABLE ant)
IF(NOT PROTOBUF_PROTOC_EXECUTABLE)
    MESSAGE(FATAL_ERROR "protoc executable not found")
ENDIF()
IF(NOT PROTOBUF_JAVA_LIBRARY)
    MESSAGE(FATAL_ERROR "protobuf java library not found")
ENDIF()

# find rsb java file
FIND_FILE(RSB_JAVA_LIBRARY
          NAMES RSBJava.jar ${JAVA_NAME}
          HINTS ${JAVA_ROOT}
                "${CMAKE_INSTALL_PREFIX}/lib/java"
                "${CMAKE_INSTALL_PREFIX}/share/java"
                "/usr/share/java")
IF(NOT RSB_JAVA_LIBRARY)
    MESSAGE(SEND_ERROR "Could not find RSBJava.jar. Please define JAVA_ROOT with a folder containing the library.")
ENDIF()

PROTOBUF_GENERATE(JAVA JAVA_PROTO_SOURCE
                  PROTOROOT  "${CMAKE_SOURCE_DIR}/data"
                  PROTOFILES "${CMAKE_SOURCE_DIR}/data/Image.proto")

SET(CONFIGURE_FILES build.xml
                    config.sh event_id.sh
                    listener.sh informer.sh
                    client.sh   server.sh)
FOREACH(FILE ${CONFIGURE_FILES})
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/${FILE}.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${FILE}"
                   @ONLY)
ENDFOREACH()

SET(JAVA_FILES config.java event_id.java
               listener.java informer.java
               client.java   server.java)
ADD_CUSTOM_COMMAND(OUTPUT  ${JAR_FILE}
                   COMMAND ${ANT_EXECUTABLE} dist
                   DEPENDS "${JAVA_FILES}"
                           "${JAVA_PROTO_SOURCE}"
                           "${CMAKE_CURRENT_BINARY_DIR}/build.xml"
                   COMMENT "Creating java jar file")
ADD_CUSTOM_TARGET(java ALL DEPENDS ${JAR_FILE})
