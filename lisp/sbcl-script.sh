#!/bin/bash

# Lisp Version
sbcl_version="1.0.47"
if [ $(uname -m) == "i686" ] ; then
  sbcl_home="/vol/cl/sbcl-${sbcl_version}/lib/sbcl"
  sbcl_bin="/vol/cl/sbcl-${sbcl_version}/bin/sbcl"
else
  echo "No appropriate SBCL installation for arch $(uname -m)"
  exit 1
fi

# ASDF Setup
export CL_SOURCE_REGISTRY="${WORKSPACE}//:"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"${WORKSPACE}/../.fasl-cache-${sbcl_version}-${JOB_NAME}\") :ignore-inherited-configuration)"

# Set home and execute code
export SBCL_HOME="${sbcl_home}"
"${sbcl_bin}" --script "$1"
