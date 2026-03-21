# Clean intermediate build files to fix file locking issues
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Cleaning Intermediate Build Files" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

$ProjectRoot = "C:\Perforce\SpiderLily\SL_main"

# Stop any UE5 processes that might be locking files
Write-Host "Stopping Unreal Engine processes..." -ForegroundColor Yellow
Get-Process | Where-Object {$_.ProcessName -like "*Unreal*"} | Stop-Process -Force -ErrorAction SilentlyContinue
Get-Process | Where-Object {$_.ProcessName -like "*UE5*"} | Stop-Process -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 2

# Clean intermediate directories
$IntermediateDirs = @(
    "$ProjectRoot\Intermediate",
    "$ProjectRoot\Plugins\FMODStudio\Intermediate",
    "$ProjectRoot\Plugins\FMODStudioNiagara\Intermediate"
)

foreach ($dir in $IntermediateDirs) {
    if (Test-Path $dir) {
        Write-Host "Deleting: $dir" -ForegroundColor Gray
        Remove-Item -Path $dir -Recurse -Force -ErrorAction SilentlyContinue
        
        if (Test-Path $dir) {
            Write-Host "[WARNING] Could not fully delete: $dir" -ForegroundColor Yellow
        } else {
            Write-Host "[OK] Deleted: $dir" -ForegroundColor Green
        }
    }
}

Write-Host ""
Write-Host "[OK] Clean complete!" -ForegroundColor Green
Write-Host ""
Write-Host "Next: Try building again" -ForegroundColor Cyan
Write-Host "  powershell.exe -ExecutionPolicy Bypass -File ""\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows\build-false-work.ps1""" -ForegroundColor Gray
Write-Host ""
