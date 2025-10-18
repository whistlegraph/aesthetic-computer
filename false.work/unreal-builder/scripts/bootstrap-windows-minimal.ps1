# Minimal Bootstrap Script for UE5 Builder
# Only installs what's actually needed - UE5 handles the rest!

$ErrorActionPreference = "Stop"

Write-Host "=== false.work UE5 Builder - Minimal Setup ===" -ForegroundColor Cyan
Write-Host ""
Write-Host "Note: Visual Studio NOT needed! UE5 includes build tools." -ForegroundColor Yellow
Write-Host ""

$LogFile = "C:\bootstrap-log.txt"
Start-Transcript -Path $LogFile

# Install Chocolatey
Write-Host "[1/4] Installing Chocolatey..." -ForegroundColor Yellow
if (!(Get-Command choco -ErrorAction SilentlyContinue)) {
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Chocolatey installed" -ForegroundColor Green
} else {
    Write-Host "✓ Already installed" -ForegroundColor Green
}

# Install Git
Write-Host ""
Write-Host "[2/4] Installing Git..." -ForegroundColor Yellow
if (!(Get-Command git -ErrorAction SilentlyContinue)) {
    choco install git -y --no-progress
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Git installed" -ForegroundColor Green
} else {
    Write-Host "✓ Already installed" -ForegroundColor Green
}

# Install Perforce CLI
Write-Host ""
Write-Host "[3/4] Installing Perforce CLI..." -ForegroundColor Yellow
$P4Url = "https://cdist2.perforce.com/perforce/r24.1/bin.ntx64/p4.exe"
$P4Path = "C:\Windows\System32\p4.exe"
if (!(Test-Path $P4Path)) {
    Invoke-WebRequest -Uri $P4Url -OutFile $P4Path
    Write-Host "✓ Perforce CLI installed" -ForegroundColor Green
} else {
    Write-Host "✓ Already installed" -ForegroundColor Green
}

# Setup directories and GitHub runner
Write-Host ""
Write-Host "[4/4] Setting up directories and GitHub Actions runner..." -ForegroundColor Yellow

# Create build directories
$Dirs = @("D:\Perforce", "D:\Builds", "D:\Builds\Logs")
foreach ($Dir in $Dirs) {
    if (!(Test-Path $Dir)) {
        New-Item -ItemType Directory -Path $Dir -Force | Out-Null
    }
}

# Download GitHub Actions runner
$RunnerUrl = "https://github.com/actions/runner/releases/download/v2.311.0/actions-runner-win-x64-2.311.0.zip"
$RunnerZip = "C:\actions-runner.zip"
if (!(Test-Path "C:\actions-runner")) {
    New-Item -ItemType Directory -Path "C:\actions-runner" -Force | Out-Null
    Invoke-WebRequest -Uri $RunnerUrl -OutFile $RunnerZip
    Expand-Archive -Path $RunnerZip -DestinationPath "C:\actions-runner" -Force
    Remove-Item $RunnerZip
}

# Download Epic Games Launcher
$EpicUrl = "https://launcher-public-service-prod06.ol.epicgames.com/launcher/api/installer/download/EpicGamesLauncherInstaller.msi"
$EpicInstaller = "C:\EpicGamesLauncherInstaller.msi"
if (!(Test-Path $EpicInstaller)) {
    Invoke-WebRequest -Uri $EpicUrl -OutFile $EpicInstaller
}

# Optimize Windows
Stop-Service WSearch -Force -ErrorAction SilentlyContinue
Set-Service WSearch -StartupType Disabled -ErrorAction SilentlyContinue
powercfg /setactive 8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c -ErrorAction SilentlyContinue

Write-Host "✓ Setup complete" -ForegroundColor Green

Stop-Transcript

Write-Host ""
Write-Host "=== Bootstrap Complete! ===" -ForegroundColor Green
Write-Host ""
Write-Host "✅ Installed:" -ForegroundColor Cyan
Write-Host "  - Git for Windows"
Write-Host "  - Perforce CLI (P4)"
Write-Host "  - GitHub Actions runner (ready to configure)"
Write-Host "  - Build directories created"
Write-Host ""
Write-Host "📥 Ready to install:" -ForegroundColor Cyan
Write-Host "  - Epic Games Launcher: C:\EpicGamesLauncherInstaller.msi"
Write-Host ""
Write-Host "⏭️  Next Steps:" -ForegroundColor Yellow
Write-Host ""
Write-Host "1. Install Epic Games Launcher and UE5 (~1-2 hours):"
Write-Host "   msiexec /i C:\EpicGamesLauncherInstaller.msi /quiet"
Write-Host "   Then sign in and install UE5 from the launcher"
Write-Host ""
Write-Host "2. Clone aesthetic-computer-vault to access secrets:"
Write-Host "   git clone https://github.com/whistlegraph/aesthetic-computer-vault.git C:\aesthetic-computer-vault"
Write-Host ""
Write-Host "3. Configure Perforce (using vault credentials):"
Write-Host "   # Load secrets"
Write-Host "   `$vaultEnv = 'C:\aesthetic-computer-vault\false.work\ue5-builder.env'"
Write-Host "   Get-Content `$vaultEnv | ForEach-Object { if (`$_ -match '^([^=]+)=(.*)$') { Set-Item -Path `"env:`$(`$matches[1])`" -Value `$matches[2] } }"
Write-Host "   # Configure P4"
Write-Host "   p4 set P4PORT=`$env:P4_SERVER"
Write-Host "   p4 set P4USER=`$env:P4_USER"
Write-Host "   p4 set P4PASSWD=`$env:P4_PASSWORD"
Write-Host "   p4 login && p4 sync"
Write-Host ""
Write-Host "4. Clone repo and configure runner:"
Write-Host "   git clone https://github.com/whistlegraph/aesthetic-computer.git C:\aesthetic-computer"
Write-Host "   cd C:\actions-runner"
Write-Host "   .\config.cmd --url https://github.com/whistlegraph/aesthetic-computer --token YOUR_TOKEN"
Write-Host "   .\svc.sh install && .\svc.sh start"
Write-Host ""
Write-Host "💡 No Visual Studio needed! UE5 includes UnrealBuildTool." -ForegroundColor Cyan
Write-Host ""
Write-Host "Log: $LogFile"
