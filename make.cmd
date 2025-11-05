@echo off
setlocal
set path=%path%;"C:\Program Files\R\R-4.5.1\bin\x64";
set path=%path%;C:\tools\Rtools45;C:\tools\Rtools45\bin;
::set path=%path%;"C:\Program Files\MiKTeX 2.9\miktex\bin\x64";
R CMD build %1
endlocal

