# Clear all Unreal Engine build caches
Write-Host "Stopping Unreal processes..." -ForegroundColor Yellow
Get-Process | Where-Object {
    $_.ProcessName -like "*ShaderCompileWorker*" -or 
    $_.ProcessName -like "*UnrealEditor*" -or 
    $_.ProcessName -like "*UE5*" -or 
    $_.ProcessName -like "*UAT*"
} | Stop-Process -Force -ErrorAction SilentlyContinue

Write-Host "Clearing DDC cache..." -ForegroundColor Yellow
Remove-Item -Recurse -Force "$env:LOCALAPPDATA\UnrealEngine\Common\DerivedDataCache\*" -ErrorAction SilentlyContinue

Write-Host "Clearing cooked data..." -ForegroundColor Yellow
Remove-Item -Recurse -Force "C:\Perforce\SpiderLily\SL_main\Saved\Cooked\*" -ErrorAction SilentlyContinue

Write-Host "Clearing asset registry..." -ForegroundColor Yellow
Remove-Item -Force "C:\Perforce\SpiderLily\SL_main\Intermediate\CachedAssetRegistry*.bin" -ErrorAction SilentlyContinue

Write-Host "Clearing shader cache..." -ForegroundColor Yellow
Remove-Item -Recurse -Force "C:\Perforce\SpiderLily\SL_main\Intermediate\Shaders\*" -ErrorAction SilentlyContinue

Write-Host "Clearing build intermediates..." -ForegroundColor Yellow
Remove-Item -Recurse -Force "C:\Perforce\SpiderLily\SL_main\Intermediate\Build\*" -ErrorAction SilentlyContinue

Write-Host "`nAll caches cleared!" -ForegroundColor Green
Write-Host "`nReady to restart build with:" -ForegroundColor Cyan
Write-Host ".\build-false-work.ps1" -ForegroundColor White
