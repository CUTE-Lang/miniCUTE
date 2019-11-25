#!/usr/bin/env bash

# Copyright: (c) 2019-present Junyoung Clare Jang
# License: BSD 3-Clause

set -eu

declare -a HSFILES
mapfile -d '' -t HSFILES < <(find . -name "*.hs" -not -path "*.stack-work*" -print0)
stylish-haskell "${HSFILES[@]}" -i
