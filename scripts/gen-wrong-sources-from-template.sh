#!/bin/bash

set -eux

template_file="$1"
shift

error_names="$@"

template_basename="$(basename "$template_file" | cut -f 1 -d '.')"

for error_name in ${error_names[@]} ; do
  cp "$template_file" test/assets/"$template_basename"/"$error_name".hs
done
