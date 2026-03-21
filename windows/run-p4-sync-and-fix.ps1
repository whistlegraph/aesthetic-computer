# Run a Perforce sync and fix corrupted FMOD UHT include if present
# Run this on the Windows build machine as Administrator (elevated PowerShell)

param(
    [string]$PerforceRoot = "C:\Perforce\SpiderLily\SL_main",
    [string]$P4Command = "p4",
    [switch]$Force
)

function Require-Admin {
    $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
    if (-not $isAdmin) {
        Write-Host "This script must be run as Administrator. Re-run in an elevated PowerShell." -ForegroundColor Red
        exit 1
    }
}

Require-Admin

Write-Host "Perforce root: $PerforceRoot" -ForegroundColor Cyan

# Check p4 existence
try {
    $p4Info = & $P4Command -s info 2>&1
} catch {
    Write-Host "Error: 'p4' not found in PATH or failed to run. Ensure Perforce CLI is installed and accessible." -ForegroundColor Red
    exit 2
}

Write-Host "Running: $P4Command sync -f //SpiderLily/...#head" -ForegroundColor Yellow
$syncArgs = @("sync","-f","//SpiderLily/...#head")
if ($Force) { $syncArgs += "-q" }

$syncResult = & $P4Command @syncArgs 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "Perforce sync failed:" -ForegroundColor Red
    Write-Host $syncResult -ForegroundColor Red
    exit 3
}

Write-Host "Perforce sync finished." -ForegroundColor Green

# Paths to check/fix (same as other scripts)
$corruptedFile = Join-Path $PerforceRoot "Plugins\FMODStudio\Intermediate\Build\Win64\UnrealGame\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp"
$headerFile = Join-Path $PerforceRoot "Plugins\FMODStudio\Source\FMODStudio\Public\FMODCallbackHandler.h"

if (Test-Path $corruptedFile) {
    Write-Host "Checking generated file: $corruptedFile" -ForegroundColor Yellow
    $content = Get-Content -Path $corruptedFile -Raw

    if ($content -match 'allbackHandler\.h') {
        Write-Host "[FOUND] Corrupted include: 'allbackHandler.h'" -ForegroundColor Red
        $fixed = $content -replace '"allbackHandler\\.h"', '"FMODCallbackHandler.h"'
        # Write a backup and then replace
        $bak = "$corruptedFile.bak"
        Copy-Item -Path $corruptedFile -Destination $bak -Force
        Set-Content -Path $corruptedFile -Value $fixed -NoNewline
        Write-Host "[OK] Fixed include to: 'FMODCallbackHandler.h' (backup: $bak)" -ForegroundColor Green
    } else {
        Write-Host "[OK] No corrupted include found in generated file." -ForegroundColor Gray
    }
} else {
    Write-Host "Generated file not found: $corruptedFile" -ForegroundColor Yellow
}

# Verify source header exists
if (Test-Path $headerFile) {
    Write-Host "[OK] Source header exists: $headerFile" -ForegroundColor Green
} else {
    Write-Host "[ERROR] Source header missing: $headerFile" -ForegroundColor Red
    Write-Host "This indicates a Perforce sync issue or incorrect branch." -ForegroundColor Yellow
    Write-Host "You may need to run: p4 sync -f //SpiderLily/...#head or check Perforce client mappings." -ForegroundColor Cyan
    exit 4
}

Write-Host "All done. You can now delete the Intermediate folder and re-run the build." -ForegroundColor Cyan
Write-Host "  Remove-Item -LiteralPath '$PerforceRoot\Plugins\FMODStudio\Intermediate' -Recurse -Force" -ForegroundColor Gray

exit 0
