#!/bin/bash

set -eu

exercise_name="$1"
shift

case_files="$@"

for case_file in ${case_files[@]} ; do
  case_name="$(basename "$case_file" | cut -f 1 -d '.')"
  cat <<END

  itShouldFailForCaseWithMessage
    "$exercise_name"
    "$case_name"
    [error "TODO: specify expected error messages"]
END
done
