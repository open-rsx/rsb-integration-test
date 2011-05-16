
SET(JAR_FILE "${CMAKE_CURRENT_BINARY_DIR}/rsbintegration.jar")

FIND_PROGRAM(ANT_EXECUTABLE ant)
FIND_PACKAGE(ProtocolBuffers REQUIRED)
IF(NOT PROTOBUF_PROTOC_EXECUTABLE)
    MESSAGE(FATAL_ERROR "protoc executable not found")
ENDIF()
IF(NOT PROTOBUF_JAVA_LIBRARY)
    MESSAGE(FATAL_ERROR "protobuf java library not found")
ENDIF()

# find rsb java file
FIND_FILE(RSB_JAVA_LIBRARY
          NAMES RSBJava.jar
          HINTS ${JAVA_ROOT}
                "${CMAKE_INSTALL_PREFIX}/lib/java"
                "${CMAKE_INSTALL_PREFIX}/share/java"
                "/usr/share/java")
IF(NOT RSB_JAVA_LIBRARY)
    MESSAGE(SEND_ERROR "Could not find RSBJava.jar. Please define JAVA_ROOT with a folder containing the library.")
ENDIF()

CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/build.xml.in" "${CMAKE_CURRENT_BINARY_DIR}/build.xml" @ONLY)
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/listener.sh.in" "${CMAKE_CURRENT_BINARY_DIR}/listener.sh" @ONLY)
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/informer.sh.in" "${CMAKE_CURRENT_BINARY_DIR}/informer.sh" @ONLY)

ADD_CUSTOM_COMMAND(OUTPUT ${JAR_FILE}
                   COMMAND ${ANT_EXECUTABLE} dist
                   DEPENDS informer.java listener.java "${CMAKE_CURRENT_BINARY_DIR}/build.xml"
                   COMMENT "Creating java jar file")
ADD_CUSTOM_TARGET(java ALL DEPENDS ${JAR_FILE})
