#!/usr/bin/env bash

set -eu

declare -a HSFILES
mapfile -t HSFILES < <(find . -name "*.hs" -not -path "*.stack-work*")
stylish-haskell "${HSFILES[@]}" -i
