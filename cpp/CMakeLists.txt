SET(PROGRAMS config.cpp event_id.cpp
             informer.cpp listener.cpp
	     server.cpp   client.cpp)

FOREACH(PROGRAM ${PROGRAMS})
  GET_FILENAME_COMPONENT(BINARY "${PROGRAM}" NAME_WE)
  ADD_EXECUTABLE("${BINARY}" "${PROGRAM}")
  TARGET_LINK_LIBRARIES("${BINARY}" ${RSB_LIBRARIES} ${RSC_LIBRARIES})
ENDFOREACH()
