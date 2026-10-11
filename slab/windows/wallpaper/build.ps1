# build.ps1 — compile the slab live wallpaper with the VS 2022 Build Tools.
#
#   .\build.ps1            release build -> build\SlabWallpaper.exe

$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $here

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$vs = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
if (-not $vs) { throw "No Visual Studio C++ tools found" }
$vcvars = Join-Path $vs 'VC\Auxiliary\Build\vcvars64.bat'

New-Item -ItemType Directory -Force build | Out-Null
# The running wallpaper holds its exe open; the linker can't overwrite it.
Get-Process SlabWallpaper -ErrorAction SilentlyContinue | Stop-Process -Force
$cmd = "call `"$vcvars`" >nul && cl /nologo /O2 /W3 /std:c17 " +
       "/Fo:build\ /Fe:build\SlabWallpaper.exe wallpaper.c " +
       "/link /SUBSYSTEM:WINDOWS /ENTRY:wWinMainCRTStartup /MANIFEST:EMBED /MANIFESTINPUT:app.manifest user32.lib gdi32.lib shell32.lib wtsapi32.lib shcore.lib"
cmd /c $cmd
if ($LASTEXITCODE -ne 0) { throw "build failed ($LASTEXITCODE)" }
Write-Host "built build\SlabWallpaper.exe"
