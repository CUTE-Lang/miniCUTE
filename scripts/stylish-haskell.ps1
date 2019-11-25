# Copyright: (c) 2019-present Junyoung Clare Jang
# License: BSD 3-Clause

stylish-haskell (GCI -Path . -File -I *.hs -R | ? FullName -NotMatch ".stack-work").FullName -i
