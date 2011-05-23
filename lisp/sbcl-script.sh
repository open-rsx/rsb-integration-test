#!/bin/bash

# Lisp Version
sbcl=${SBCL_HOME%/lib/sbcl*}/bin/sbcl
sbcl_version=$(${sbcl} --version | cut -c 6-)

# ASDF Setup
export CL_SOURCE_REGISTRY="${WORKSPACE}//:"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"${WORKSPACE}/../.fasl-cache-${sbcl_version}-${JOB_NAME}\") :ignore-inherited-configuration)"

# Set home and execute code
${sbcl} --script "$@"
