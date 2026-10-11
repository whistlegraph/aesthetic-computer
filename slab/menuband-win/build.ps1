# build.ps1 — compile Menu Band for Windows with the VS 2022 Build Tools.
#
#   .\build.ps1            release build -> build\MenuBand.exe
#   .\build.ps1 -Run       build, then (re)launch it
#
# gm_synth.c/.h are the shared AC synthesis core. On the Mac they are
# symlinks into fedac/native/src; here deploy.sh copies them in beside this
# script, so the build never depends on symlink support in the checkout.

param([switch]$Run)
$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $here

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$vs = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
if (-not $vs) { throw "No Visual Studio C++ tools found" }
$vcvars = Join-Path $vs 'VC\Auxiliary\Build\vcvars64.bat'

New-Item -ItemType Directory -Force build | Out-Null
# The running strip holds its own exe open; the linker can't overwrite it.
Get-Process MenuBand -ErrorAction SilentlyContinue | Stop-Process -Force
$cmd = "call `"$vcvars`" >nul && cl /nologo /O2 /W3 /std:c17 " +
       "/Fo:build\ /Fe:build\MenuBand.exe menuband.c gm_synth.c " +
       "/link /SUBSYSTEM:WINDOWS /ENTRY:wWinMainCRTStartup user32.lib gdi32.lib shell32.lib ole32.lib avrt.lib"
cmd /c $cmd
if ($LASTEXITCODE -ne 0) { throw "build failed ($LASTEXITCODE)" }
Write-Host "built build\MenuBand.exe"

if ($Run) {
  Get-Process MenuBand -ErrorAction SilentlyContinue | Stop-Process -Force
  Start-Sleep -Milliseconds 300
  Start-Process (Join-Path $here 'build\MenuBand.exe')
  Write-Host "launched"
}
