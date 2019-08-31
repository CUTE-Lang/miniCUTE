@ECHO OFF
SETLOCAL EnableDelayedExpansion
FOR /F "delims=" %%F IN ('DIR /b /s "*.hs" ^| FINDSTR /v ".stack-work"') DO (
  SET HSFILES=!HSFILES! "%%F"
)
stylish-haskell !HSFILES! -i
