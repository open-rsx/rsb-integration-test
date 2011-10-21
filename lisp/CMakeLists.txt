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

    # Create binary.
    SET(SCRIPT       "${CMAKE_CURRENT_SOURCE_DIR}/dump.lisp")
    SET(NAME         "test")
    SET(LOAD_SCRIPTS "")
    FOREACH(SCRIPT ${SCRIPTS})
        SET(LOAD_SCRIPTS "${LOAD_SCRIPTS} --load ${CMAKE_CURRENT_SOURCE_DIR}/${SCRIPT}")
    ENDFOREACH()
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    ADD_CUSTOM_COMMAND(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                       COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                       DEPENDS "${SCRIPT}"
                       COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
    ADD_CUSTOM_TARGET(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

    # Create symlinks.
    FOREACH(SCRIPT ${SCRIPTS})
        ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${SCRIPT}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "${CMAKE_CURRENT_BINARY_DIR}/${NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${SCRIPT}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                           COMMENT "Creating symlink ${TOOL} -> ${NAME}")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(links ALL DEPENDS ${SCRIPTS})

ELSE()
    MESSAGE(STATUS "SBCL_HOME is not set. Common Lisp disabled for testing.")
ENDIF()
