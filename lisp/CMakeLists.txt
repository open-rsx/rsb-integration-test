SET(SCRIPTS "client.lisp"   "server.lisp"
            "informer.lisp" "listener.lisp"
	    "config.lisp"   "event_id.lisp")

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(SBCL_HOME)
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin"
	     NO_DEFAULT_PATH)

    # Configure ASDF.
    SET(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
    SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"/tmp/fasl-cache-rsb-integrationtest/\\\" :implementation)) :ignore-inherited-configuration)")

    # Create images.
    SET(IMAGES)
    FOREACH(SCRIPT ${SCRIPTS})
        SET(SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/${SCRIPT}")
        STRING(REPLACE ".lisp" "" IMAGE ${SCRIPT})
        GET_FILENAME_COMPONENT(NAME ${IMAGE} NAME)
        CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                       "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
		       @ONLY)
        ADD_CUSTOM_COMMAND(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${IMAGE}"
                           COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                           DEPENDS "${SCRIPT}"
                           COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
        LIST(APPEND IMAGES "${CMAKE_CURRENT_BINARY_DIR}/${IMAGE}")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(scripts ALL DEPENDS ${IMAGES})

ELSE()
    MESSAGE(STATUS "SBCL_HOME is not set. Common Lisp disabled for testing.")
ENDIF()
