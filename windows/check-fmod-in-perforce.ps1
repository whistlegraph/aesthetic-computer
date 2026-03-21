# Check what FMOD files exist in Perforce
# Run from PowerShell

Write-Host "Searching for FMOD files in Perforce depot..." -ForegroundColor Cyan
Write-Host ""

# Search for FMOD DLL/LIB files
Write-Host "Searching for fmodL files:" -ForegroundColor Yellow
p4 files //SpiderLily/.../*fmodL*.dll //SpiderLily/.../*fmodL*.lib 2>&1

Write-Host ""
Write-Host "Searching for fmodstudioL files:" -ForegroundColor Yellow
p4 files //SpiderLily/.../*fmodstudio*.dll //SpiderLily/.../*fmodstudio*.lib 2>&1

Write-Host ""
Write-Host "Checking what's in the local FMODStudio plugin:" -ForegroundColor Yellow
Get-ChildItem "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio" -Recurse -Include *.dll,*.lib | Select-Object FullName

Write-Host ""
Write-Host "Checking Perforce workspace mapping:" -ForegroundColor Yellow
p4 where //SpiderLily/Plugins/FMODStudio/...

Write-Host ""
