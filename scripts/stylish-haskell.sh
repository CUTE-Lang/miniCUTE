#!/usr/bin/env bash

set -eu

declare -a HSFILES
mapfile -d '' -t HSFILES < <(find . -name "*.hs" -not -path "*.stack-work*" -print0)
stylish-haskell "${HSFILES[@]}" -i
