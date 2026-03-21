# Unlock FMOD Generated Files
# Removes read-only attribute from UnrealHeaderTool generated files

Write-Host "Removing read-only attributes from FMOD generated files..." -ForegroundColor Cyan

$fmodPluginPath = "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate\Build\Win64"
$fmodNiagaraPath = "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudioNiagara\Intermediate\Build\Win64"

if (Test-Path $fmodPluginPath) {
    Get-ChildItem -Path $fmodPluginPath -Recurse -File | ForEach-Object {
        if ($_.IsReadOnly) {
            $_.IsReadOnly = $false
            Write-Host "  [UNLOCKED] $($_.Name)" -ForegroundColor Yellow
        }
    }
}

if (Test-Path $fmodNiagaraPath) {
    Get-ChildItem -Path $fmodNiagaraPath -Recurse -File | ForEach-Object {
        if ($_.IsReadOnly) {
            $_.IsReadOnly = $false
            Write-Host "  [UNLOCKED] $($_.Name)" -ForegroundColor Yellow
        }
    }
}

Write-Host "`nNow deleting Intermediate folders for clean rebuild..." -ForegroundColor Cyan

# Delete the Intermediate folders entirely
if (Test-Path "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate") {
    Remove-Item -Path "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate" -Recurse -Force
    Write-Host "  [DELETED] FMODStudio\Intermediate" -ForegroundColor Green
}

if (Test-Path "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudioNiagara\Intermediate") {
    Remove-Item -Path "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudioNiagara\Intermediate" -Recurse -Force
    Write-Host "  [DELETED] FMODStudioNiagara\Intermediate" -ForegroundColor Green
}

Write-Host "`nNow syncing FMOD binaries from Perforce..." -ForegroundColor Cyan
Set-Location "C:\Perforce\SpiderLily\SL_main"

# Force sync the entire FMOD plugin
& p4 sync -f "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\..."
& p4 sync -f "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudioNiagara\..."

Write-Host "`nVerifying FMOD library files..." -ForegroundColor Cyan
$fmodBinPath = "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Binaries\Win64"

$requiredFiles = @(
    "fmodL_vc.lib",
    "fmodstudioL_vc.lib",
    "fmodL.dll",
    "fmodstudioL.dll"
)

$allPresent = $true
foreach ($file in $requiredFiles) {
    $fullPath = Join-Path $fmodBinPath $file
    if (Test-Path $fullPath) {
        Write-Host "  [OK] $file" -ForegroundColor Green
    } else {
        Write-Host "  [MISSING] $file" -ForegroundColor Red
        $allPresent = $false
    }
}

if ($allPresent) {
    Write-Host "`nAll FMOD libraries present! Ready to build." -ForegroundColor Green
} else {
    Write-Host "`nSome FMOD libraries still missing. Check Perforce workspace mapping." -ForegroundColor Yellow
}
