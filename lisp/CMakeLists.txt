set(SCRIPTS "config.lisp" "event_id.lisp"
            "informer.lisp"            "listener.lisp"
            "client.lisp"              "server.lisp"
            "local-introspection.lisp" "remote-introspection.lisp")

# Find and check SBCL installation.
set(SBCL_HOME $ENV{SBCL_HOME})
if(SBCL_HOME)
    find_program(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin"
                 NO_DEFAULT_PATH)

    # Configure Lisp environment.
    set(LISP_FASL_DIR  "/tmp/fasl-cache-rsb-integrationtest/"
                       CACHE STRING   "Directory in which compiled Lisp code should be placed.")
    set(LISP_INIT_FILE ""
                       CACHE FILEPATH "File to load as Lisp init file.")
    set(LISP_INIT_CODE ""
                       CACHE STRING   "Lisp code to be evaulated during initialization.")
    set(LISP_INIT "")
    if(LISP_INIT_FILE)
        set(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
    endif()
    if(LISP_INIT_CODE)
        set(LISP_INIT "${LISP_INIT} --eval '${LISP_INIT_CODE}'")
    endif()

    set(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
    set(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${LISP_FASL_DIR}\\\" :implementation)) :ignore-inherited-configuration)")

    # Create binary.
    set(SCRIPT       "${CMAKE_CURRENT_SOURCE_DIR}/dump.lisp")
    set(NAME         "test")
    set(LOAD_SCRIPTS "")
    foreach(SCRIPT ${SCRIPTS})
        set(LOAD_SCRIPTS "${LOAD_SCRIPTS} --eval \"(with-compilation-unit () (load \\\"${CMAKE_CURRENT_SOURCE_DIR}/${SCRIPT}\\\"))\"")
    endforeach()
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                       COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                       DEPENDS "${SCRIPT}"
                       COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
    add_custom_target(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")

    # Create symlinks.
    set(LINKS "")
    foreach(SCRIPT ${SCRIPTS})
        string(REPLACE ".lisp" "" LINK "${SCRIPT}")
        set(LINKS ${LINKS} "${CMAKE_CURRENT_BINARY_DIR}/${LINK}")
        add_custom_command(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${LINK}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "${CMAKE_CURRENT_BINARY_DIR}/${NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${LINK}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                           COMMENT "Creating symlink ${LINK} -> ${NAME}")
    endforeach()
    add_custom_target(links ALL DEPENDS ${LINKS})

else()
    message(STATUS "SBCL_HOME is not set. Common Lisp disabled for testing.")
endif()
