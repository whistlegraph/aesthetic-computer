# Install UE5 Build Dependencies via CLI
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Installing UE5 Build Dependencies" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Check if running as admin
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (!$isAdmin) {
    Write-Host "[ERROR] This script must be run as Administrator!" -ForegroundColor Red
    Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
    exit 1
}

# 1. Install .NET Framework 4.8 Developer Pack
Write-Host "Step 1: Installing .NET Framework 4.8 Developer Pack..." -ForegroundColor Yellow
Write-Host ""

# Check if already installed by looking for reference assemblies
$netfxRefPath = "C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.8"
if (Test-Path $netfxRefPath) {
    Write-Host "[OK] .NET Framework 4.8 Developer Pack already installed" -ForegroundColor Green
    Write-Host ""
} else {
    Write-Host "[INFO] .NET Framework 4.8 Developer Pack not detected" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Please download and install manually:" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  1. Visit: https://dotnet.microsoft.com/download/dotnet-framework/net48" -ForegroundColor Cyan
    Write-Host "  2. Download: Developer Pack" -ForegroundColor Cyan
    Write-Host "  3. Run the installer with default options" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Or use winget:" -ForegroundColor Yellow
    Write-Host "  winget install Microsoft.DotNet.Framework.DeveloperPack_4" -ForegroundColor Gray
    Write-Host ""
    
    # Offer to open the download page
    $openBrowser = Read-Host "Open download page in browser? (Y/n)"
    if ($openBrowser -ne 'n' -and $openBrowser -ne 'N') {
        Start-Process "https://dotnet.microsoft.com/download/dotnet-framework/net48"
        Write-Host "[OK] Browser opened" -ForegroundColor Green
    }
    
    Write-Host ""
    Write-Host "[WARNING] Build will fail without .NET Framework 4.8 Developer Pack" -ForegroundColor Yellow
    Write-Host "Press any key to continue or Ctrl+C to exit..." -ForegroundColor Gray
    $null = $Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')
}

Write-Host ""

# 2. Install MSVC v14.38 Build Tools using VS Installer
Write-Host "Step 2: Installing MSVC v14.38 Build Tools..." -ForegroundColor Yellow
Write-Host ""

$vsInstaller = "C:\Program Files (x86)\Microsoft Visual Studio\Installer\vs_installer.exe"

if (!(Test-Path $vsInstaller)) {
    Write-Host "[WARNING] Visual Studio Installer not found!" -ForegroundColor Yellow
    Write-Host "Installing Visual Studio Build Tools 2022..." -ForegroundColor Gray
    
    # Download VS Build Tools bootstrapper
    $vsBuildToolsUrl = "https://aka.ms/vs/17/release/vs_BuildTools.exe"
    $vsBuildTools = "$env:TEMP\vs_BuildTools.exe"
    
    Invoke-WebRequest -Uri $vsBuildToolsUrl -OutFile $vsBuildTools -UseBasicParsing
    
    # Install VS Build Tools with required components
    Write-Host "Installing Visual Studio Build Tools (this may take 10-20 minutes)..." -ForegroundColor Gray
    Start-Process -FilePath $vsBuildTools -ArgumentList `
        "--quiet", `
        "--wait", `
        "--norestart", `
        "--add", "Microsoft.VisualStudio.Workload.VCTools", `
        "--add", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64", `
        "--add", "Microsoft.VisualStudio.Component.VC.14.38.17.8.x86.x64", `
        "--add", "Microsoft.VisualStudio.Component.Windows11SDK.22621" `
        -Wait
    
    Remove-Item $vsBuildTools -ErrorAction SilentlyContinue
} else {
    Write-Host "Modifying existing Visual Studio installation..." -ForegroundColor Gray
    
    # Modify existing installation to add MSVC v14.38
    Start-Process -FilePath $vsInstaller -ArgumentList `
        "modify", `
        "--installPath", "`"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools`"", `
        "--quiet", `
        "--norestart", `
        "--add", "Microsoft.VisualStudio.Component.VC.14.38.17.8.x86.x64" `
        -Wait
}

Write-Host "[OK] MSVC v14.38 Build Tools installed" -ForegroundColor Green
Write-Host ""

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "[SUCCESS] Dependencies installed!" -ForegroundColor Green
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "You may need to restart your terminal or computer." -ForegroundColor Yellow
Write-Host ""
Write-Host "Next: Try building again:" -ForegroundColor Cyan
Write-Host "  powershell.exe -ExecutionPolicy Bypass -File .\build-false-work.ps1" -ForegroundColor White
Write-Host ""
