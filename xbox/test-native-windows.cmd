@echo off
setlocal enabledelayedexpansion

for /f "usebackq tokens=*" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -property installationPath`) do set "VSROOT=%%i"
if not defined VSROOT exit /b 2
call "%VSROOT%\VC\Auxiliary\Build\vcvars64.bat" >nul || exit /b 2

set "ROOT=%~dp0.."
set "BUILD=%TEMP%\ac-xbox-native-tests"
if exist "%BUILD%" rmdir /s /q "%BUILD%"
mkdir "%BUILD%" || exit /b 2

node --check "%ROOT%\xbox\live\controller-probe.js" || exit /b 1
node "%ROOT%\xbox\live\tests\controller-probe.test.mjs" || exit /b 1

cl /nologo /std:c++20 /EHsc /W4 ^
  /I"%ROOT%\xbox\runtime\include" ^
  "%ROOT%\xbox\runtime\tests\runtime_contract.cpp" ^
  /Fe:"%BUILD%\runtime-contract.exe" || exit /b 1
"%BUILD%\runtime-contract.exe" || exit /b 1

set "QJS=%ROOT%\xbox\native-bios\third_party\quickjs-ng"
for %%f in (quickjs dtoa libregexp libunicode) do (
  cl /nologo /c /O2 /TC /std:c17 /experimental:c11atomics ^
    /DQUICKJS_NG_BUILD /wd4146 /wd4703 /I"%QJS%" ^
    "%QJS%\%%f.c" /Fo:"%BUILD%\%%f.obj" || exit /b 1
)

cl /nologo /O2 /EHsc /std:c++17 /W4 /DQUICKJS_NG_BUILD ^
  /I"%ROOT%\xbox\native-bios" /I"%ROOT%\xbox\runtime\include" /I"%QJS%" ^
  "%ROOT%\xbox\native-bios\QuickJsEngine.cpp" ^
  "%ROOT%\xbox\native-bios\tests\quickjs_engine_smoke.cpp" ^
  "%BUILD%\quickjs.obj" "%BUILD%\dtoa.obj" ^
  "%BUILD%\libregexp.obj" "%BUILD%\libunicode.obj" ^
  /Fe:"%BUILD%\quickjs-smoke.exe" || exit /b 1
"%BUILD%\quickjs-smoke.exe" || exit /b 1

echo xbox native Windows preflight: all tests passed
exit /b 0
