# Fix UE5 Build Dependencies

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "UE5 Build Dependencies Checker" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

Write-Host "Issues detected:" -ForegroundColor Yellow
Write-Host ""

Write-Host "1. MSVC Version Mismatch" -ForegroundColor Red
Write-Host "   Current: 14.43.34809" -ForegroundColor Gray
Write-Host "   Required: 14.38.33130" -ForegroundColor Gray
Write-Host ""
Write-Host "   Fix: Install Visual Studio 2022 with MSVC v143 - VS 2022 C++ x64/x86 build tools (v14.38)" -ForegroundColor Green
Write-Host "   URL: https://visualstudio.microsoft.com/downloads/" -ForegroundColor Cyan
Write-Host "   - Run Visual Studio Installer" -ForegroundColor Gray
Write-Host "   - Modify your VS 2022 installation" -ForegroundColor Gray
Write-Host "   - Go to Individual Components tab" -ForegroundColor Gray
Write-Host "   - Search for: MSVC v143 - VS 2022 C++ x64/x86 build tools (v14.38)" -ForegroundColor Gray
Write-Host "   - Check it and click Modify" -ForegroundColor Gray
Write-Host ""

Write-Host "2. Missing .NET Framework SDK" -ForegroundColor Red
Write-Host "   Required: .NET Framework SDK 4.6.0 or higher" -ForegroundColor Gray
Write-Host ""
Write-Host "   Fix: Install .NET Framework 4.8 Developer Pack" -ForegroundColor Green
Write-Host "   URL: https://dotnet.microsoft.com/download/dotnet-framework/net48" -ForegroundColor Cyan
Write-Host "   Download: .NET Framework 4.8 Developer Pack" -ForegroundColor Gray
Write-Host ""

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "After installing these, try building again with:" -ForegroundColor Yellow
Write-Host "  powershell.exe -ExecutionPolicy Bypass -File .\build-false-work.ps1" -ForegroundColor White
Write-Host ""

# Check if Visual Studio Installer is available
$vsInstaller = "C:\Program Files (x86)\Microsoft Visual Studio\Installer\vs_installer.exe"
if (Test-Path $vsInstaller) {
    Write-Host "Quick actions:" -ForegroundColor Cyan
    Write-Host ""
    $response = Read-Host "Open Visual Studio Installer now? (y/N)"
    if ($response -eq "y" -or $response -eq "Y") {
        Start-Process $vsInstaller
    }
}

Write-Host ""
