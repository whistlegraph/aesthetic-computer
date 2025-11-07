# Aggressively clean all generated files to fix corruption
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Deep Clean - All Generated Files" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

$ProjectRoot = "C:\Perforce\SpiderLily\SL_main"

# Stop ALL processes
Write-Host "Stopping all Unreal/FMOD processes..." -ForegroundColor Yellow
Get-Process | Where-Object {$_.ProcessName -match "Unreal|UE5|UE_|FMOD|dotnet"} | Stop-Process -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 3

# Delete ALL intermediate and generated files
$DirsToDelete = @(
    "$ProjectRoot\Intermediate",
    "$ProjectRoot\Binaries",
    "$ProjectRoot\Saved",
    "$ProjectRoot\Plugins\FMODStudio\Intermediate",
    "$ProjectRoot\Plugins\FMODStudio\Binaries", 
    "$ProjectRoot\Plugins\FMODStudioNiagara\Intermediate",
    "$ProjectRoot\Plugins\FMODStudioNiagara\Binaries"
)

foreach ($dir in $DirsToDelete) {
    if (Test-Path $dir) {
        Write-Host "Deleting: $dir" -ForegroundColor Gray
        
        # Try normal delete first
        Remove-Item -Path $dir -Recurse -Force -ErrorAction SilentlyContinue
        
        # If still exists, try robocopy method (faster for large dirs)
        if (Test-Path $dir) {
            Write-Host "  Using robocopy method..." -ForegroundColor DarkGray
            $emptyDir = "$env:TEMP\empty_$(Get-Random)"
            New-Item -ItemType Directory -Path $emptyDir -Force | Out-Null
            robocopy $emptyDir $dir /MIR /R:0 /W:0 /NP /NDL /NFL | Out-Null
            Remove-Item -Path $dir -Recurse -Force -ErrorAction SilentlyContinue
            Remove-Item -Path $emptyDir -Force -ErrorAction SilentlyContinue
        }
        
        if (Test-Path $dir) {
            Write-Host "[WARNING] Could not fully delete: $dir" -ForegroundColor Yellow
        } else {
            Write-Host "[OK] Deleted" -ForegroundColor Green
        }
    }
}

Write-Host ""
Write-Host "[OK] Deep clean complete!" -ForegroundColor Green
Write-Host ""
Write-Host "Next: Rebuild from scratch" -ForegroundColor Cyan
Write-Host "  powershell.exe -ExecutionPolicy Bypass -File ""\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows\build-false-work.ps1""" -ForegroundColor Gray
Write-Host ""
