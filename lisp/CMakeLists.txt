FILE(GLOB SCRIPTS "*.lisp")

# Processor Architectur
EXECUTE_PROCESS(OUTPUT_VARIABLE SYSTEM_MACHINE
                COMMAND uname --machine
		OUTPUT_STRIP_TRAILING_WHITESPACE)

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(FATAL_ERROR "SBCL_HOME is not set. Set it.")
ENDIF()
FIND_PROGRAM(SBCL_EXECUTABLE sbcl
             PATHS "${SBCL_HOME}/../../bin"
	     NO_DEFAULT_PATH)
EXECUTE_PROCESS(OUTPUT_VARIABLE SBCL_VERSION
                COMMAND ${SBCL_EXECUTABLE} --version
		OUTPUT_STRIP_TRAILING_WHITESPACE)
STRING(REPLACE "SBCL " "" SBCL_VERSION ${SBCL_VERSION})

# Configure Lisp environment.
SET(LISP_INIT_FILE "" CACHE FILEPATH "File to load as Lisp init file.")
SET(LISP_INIT_CODE "" CACHE STRING "Lisp code to be evaulated during initialization.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()
IF(LISP_INIT_CODE)
    SET(LISP_INIT "${LISP_INIT} --eval '${LISP_INIT_CODE}'")
ENDIF()

SET(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t \\\"/tmp/fasl-cache-${SBCL_VERSION}-${SYSTEM_MACHINE}-rsb-integrationtest\\\") :ignore-inherited-configuration)")

# Create images.
SET(IMAGES)
FOREACH(SCRIPT ${SCRIPTS})
    STRING(REPLACE ".lisp" "" IMAGE ${SCRIPT})
    GET_FILENAME_COMPONENT(NAME ${IMAGE} NAME)
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
		   @ONLY)
    ADD_CUSTOM_COMMAND(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${IMAGE}"
                       COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                       DEPENDS ${SCRIPT}
                       COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
    LIST(APPEND IMAGES "${CMAKE_CURRENT_BINARY_DIR}/${IMAGE}")
ENDFOREACH()
ADD_CUSTOM_TARGET(scripts ALL DEPENDS ${IMAGES})
