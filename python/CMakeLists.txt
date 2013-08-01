protobuf_generate(PYTHON PYTHON_PROTO_SOURCE
                  PROTOROOT  "${CMAKE_SOURCE_DIR}/data"
                  PROTOFILES "${CMAKE_SOURCE_DIR}/data/Image.proto")

add_custom_target(dummy ALL DEPENDS "${PYTHON_PROTO_SOURCE}")
