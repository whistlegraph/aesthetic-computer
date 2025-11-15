@echo off

rem Simple batch file to assemble Game Boy ROMs
rem usage: build.bat <sourcefile>

setlocal
set _fn=%~n1

if exist %_fn%.gb del %_fn%.gb

echo Asset conversion...
rem Convert *-tilemap.png files to 2bpp format including a tilemap (remove duplicate tiles)
for %%f in (*-tilemap.png) do rgbgfx -u -o %%~nf.2bpp -t %%~nf.tilemap "%%f"

rem Convert *-ztiles.png files to 2bpp format column-by-column without a tilemap (keep duplicate tiles)
for %%f in (*-ztiles.png) do rgbgfx -Z -o %%~nf.2bpp "%%f"

rem Convert *-tiles.png files to 2bpp format row-by-row without a tilemap (keep duplicate tiles)
for %%f in (*-tiles.png) do rgbgfx -o %%~nf.2bpp "%%f"

echo Assembling...
rgbasm -I ..\..\inc -o%_fn%.o %1
if ERRORLEVEL 1 goto :error
echo Linking...
rgblink -n%_fn%.sym -m%_fn%.map -o %_fn%.gb %_fn%.o
if ERRORLEVEL 1 goto :error
echo Fixing...
rgbfix -p 255 -v %_fn%.gb
if ERRORLEVEL 1 goto :error

echo Created: %_fn%.gb
del *.o
goto :end

:error
echo Build failed.

:end
endlocal
