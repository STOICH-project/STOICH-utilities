@echo off
setlocal
set path=%path%;"C:\Program Files\R\R-4.5.2\bin\x64";
::set path=%path%;"C:\Program Files\R\R-devel\bin\x64";
set path=%path%;C:\tools\Rtools45;C:\tools\Rtools45\bin;
::set path=%path%;"C:\Program Files\MiKTeX 2.9\miktex\bin\x64";
R CMD check --as-cran %1
endlocal

