set(JAR_FILE "${CMAKE_CURRENT_BINARY_DIR}/integration-test-1.0-SNAPSHOT-jar-with-dependencies.jar")

find_program(MAVEN_EXECUTABLE mvn)

file(GLOB CLASS_FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/main/java/rsb/integration/test/*.java)
set(JAVA_FILES ${CMAKE_CURRENT_SOURCE_DIR}/pom.xml ${CLASS_FILES})
add_custom_command(OUTPUT  ${JAR_FILE}
                   COMMAND ${MAVEN_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/pom.xml compile assembly:single
                   DEPENDS ${JAVA_FILES}
                   COMMENT "Creating java jar file")
add_custom_target(java ALL DEPENDS ${JAR_FILE})
