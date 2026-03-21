# Windows VM Bootstrap Script for UE5 Builder
# This script installs all dependencies automatically on first boot
# Run as Administrator in PowerShell

$ErrorActionPreference = "Stop"

Write-Host "=== false.work UE5 Builder - Automated Setup ===" -ForegroundColor Cyan
Write-Host ""

# Create log file
$LogFile = "C:\bootstrap-log.txt"
Start-Transcript -Path $LogFile

# Install Chocolatey (package manager for Windows)
Write-Host "[1/6] Installing Chocolatey package manager..." -ForegroundColor Yellow
if (!(Get-Command choco -ErrorAction SilentlyContinue)) {
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Chocolatey installed" -ForegroundColor Green
} else {
    Write-Host "✓ Chocolatey already installed" -ForegroundColor Green
}

# Install Git
Write-Host ""
Write-Host "[2/6] Installing Git for Windows..." -ForegroundColor Yellow
if (!(Get-Command git -ErrorAction SilentlyContinue)) {
    choco install git -y --no-progress
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Git installed" -ForegroundColor Green
} else {
    Write-Host "✓ Git already installed" -ForegroundColor Green
}

# Install Perforce CLI (P4)
Write-Host ""
Write-Host "[3/6] Installing Perforce CLI (P4)..." -ForegroundColor Yellow
$P4Url = "https://cdist2.perforce.com/perforce/r24.1/bin.ntx64/p4.exe"
$P4Path = "C:\Windows\System32\p4.exe"
if (!(Test-Path $P4Path)) {
    Invoke-WebRequest -Uri $P4Url -OutFile $P4Path
    Write-Host "✓ Perforce CLI installed" -ForegroundColor Green
} else {
    Write-Host "✓ Perforce CLI already installed" -ForegroundColor Green
}

# Download Visual Studio Build Tools installer (doesn't auto-install due to size)
Write-Host ""
Write-Host "[4/6] Downloading Visual Studio 2022 installer..." -ForegroundColor Yellow
$VSUrl = "https://aka.ms/vs/17/release/vs_BuildTools.exe"
$VSInstaller = "C:\vs_buildtools.exe"
if (!(Test-Path $VSInstaller)) {
    Invoke-WebRequest -Uri $VSUrl -OutFile $VSInstaller
    Write-Host "✓ Visual Studio installer downloaded to: $VSInstaller" -ForegroundColor Green
    Write-Host "  Run this to install (takes 30-60 min):" -ForegroundColor Yellow
    Write-Host "  $VSInstaller --quiet --wait --norestart --nocache \\" -ForegroundColor Gray
    Write-Host "    --installPath C:\BuildTools \\" -ForegroundColor Gray
    Write-Host "    --add Microsoft.VisualStudio.Workload.VCTools \\" -ForegroundColor Gray
    Write-Host "    --add Microsoft.VisualStudio.Workload.ManagedDesktopBuildTools" -ForegroundColor Gray
} else {
    Write-Host "✓ Visual Studio installer already downloaded" -ForegroundColor Green
}

# Download Epic Games Launcher installer
Write-Host ""
Write-Host "[5/6] Downloading Epic Games Launcher..." -ForegroundColor Yellow
$EpicUrl = "https://launcher-public-service-prod06.ol.epicgames.com/launcher/api/installer/download/EpicGamesLauncherInstaller.msi"
$EpicInstaller = "C:\EpicGamesLauncherInstaller.msi"
if (!(Test-Path $EpicInstaller)) {
    Invoke-WebRequest -Uri $EpicUrl -OutFile $EpicInstaller
    Write-Host "✓ Epic Games Launcher downloaded to: $EpicInstaller" -ForegroundColor Green
    Write-Host "  Install manually: msiexec /i $EpicInstaller /quiet" -ForegroundColor Yellow
    Write-Host "  Then sign in and install UE5 from the Launcher" -ForegroundColor Yellow
} else {
    Write-Host "✓ Epic Games Launcher installer already downloaded" -ForegroundColor Green
}

# Configure Windows for build performance
Write-Host ""
Write-Host "[6/6] Configuring Windows for build performance..." -ForegroundColor Yellow

# Disable Windows Search indexing
try {
    Stop-Service WSearch -Force -ErrorAction SilentlyContinue
    Set-Service WSearch -StartupType Disabled -ErrorAction SilentlyContinue
    Write-Host "✓ Disabled Windows Search indexing" -ForegroundColor Green
} catch {
    Write-Host "  Could not disable Windows Search" -ForegroundColor Yellow
}

# Set high performance power plan
try {
    powercfg /setactive 8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c
    Write-Host "✓ Set High Performance power plan" -ForegroundColor Green
} catch {
    Write-Host "  Could not set power plan" -ForegroundColor Yellow
}

# Create build directories
Write-Host ""
Write-Host "Creating build directories..." -ForegroundColor Yellow
$Dirs = @("D:\Perforce", "D:\Builds", "D:\Builds\Logs")
foreach ($Dir in $Dirs) {
    if (!(Test-Path $Dir)) {
        New-Item -ItemType Directory -Path $Dir -Force | Out-Null
        Write-Host "✓ Created: $Dir" -ForegroundColor Green
    }
}

# Download GitHub Actions runner setup script
Write-Host ""
Write-Host "Downloading GitHub Actions runner setup..." -ForegroundColor Yellow
$RunnerUrl = "https://github.com/actions/runner/releases/download/v2.311.0/actions-runner-win-x64-2.311.0.zip"
$RunnerZip = "C:\actions-runner.zip"
if (!(Test-Path "C:\actions-runner")) {
    New-Item -ItemType Directory -Path "C:\actions-runner" -Force | Out-Null
    Invoke-WebRequest -Uri $RunnerUrl -OutFile $RunnerZip
    Expand-Archive -Path $RunnerZip -DestinationPath "C:\actions-runner" -Force
    Remove-Item $RunnerZip
    Write-Host "✓ GitHub Actions runner downloaded to: C:\actions-runner" -ForegroundColor Green
}

Stop-Transcript

Write-Host ""
Write-Host "=== Bootstrap Complete! ===" -ForegroundColor Green
Write-Host ""
Write-Host "✅ Installed:"
Write-Host "  - Chocolatey package manager"
Write-Host "  - Git for Windows"
Write-Host "  - Perforce CLI (P4)"
Write-Host "  - GitHub Actions runner (extracted)"
Write-Host ""
Write-Host "📥 Downloaded (manual install needed):"
Write-Host "  - Visual Studio Build Tools: C:\vs_buildtools.exe"
Write-Host "  - Epic Games Launcher: C:\EpicGamesLauncherInstaller.msi"
Write-Host ""
Write-Host "⏭️  Next Steps:" -ForegroundColor Cyan
Write-Host ""
Write-Host "1. Install Visual Studio Build Tools (~30-60 min):" -ForegroundColor Yellow
Write-Host "   C:\vs_buildtools.exe --quiet --wait --norestart --nocache \"
Write-Host "     --installPath C:\BuildTools \"
Write-Host "     --add Microsoft.VisualStudio.Workload.VCTools \"
Write-Host "     --add Microsoft.VisualStudio.Workload.ManagedDesktopBuildTools"
Write-Host ""
Write-Host "2. Install Epic Games Launcher:" -ForegroundColor Yellow
Write-Host "   msiexec /i C:\EpicGamesLauncherInstaller.msi /quiet"
Write-Host "   Then sign in and install UE5"
Write-Host ""
Write-Host "3. Clone aesthetic-computer-vault for credentials:" -ForegroundColor Yellow
Write-Host "   git clone https://github.com/whistlegraph/aesthetic-computer-vault.git C:\aesthetic-computer-vault"
Write-Host ""
Write-Host "4. Configure Perforce (using vault):" -ForegroundColor Yellow
Write-Host "   `$vaultEnv = 'C:\aesthetic-computer-vault\false.work\ue5-builder.env'"
Write-Host "   Get-Content `$vaultEnv | ForEach-Object { if (`$_ -match '^([^=]+)=(.*)$') { Set-Item -Path `"env:`$(`$matches[1])`" -Value `$matches[2] } }"
Write-Host "   p4 set P4PORT=`$env:P4_SERVER"
Write-Host "   p4 set P4USER=`$env:P4_USER"
Write-Host "   p4 set P4PASSWD=`$env:P4_PASSWORD"
Write-Host "   p4 login"
Write-Host ""
Write-Host "5. Clone aesthetic-computer repo:" -ForegroundColor Yellow
Write-Host "   cd C:\"
Write-Host "   git clone https://github.com/whistlegraph/aesthetic-computer.git"
Write-Host ""
Write-Host "6. Run setup script:" -ForegroundColor Yellow
Write-Host "   cd C:\aesthetic-computer\false.work\unreal-builder"
Write-Host "   .\scripts\setup-build-machine.ps1"
Write-Host ""
Write-Host "6. Configure GitHub Actions runner:" -ForegroundColor Yellow
Write-Host "   cd C:\actions-runner"
Write-Host "   .\config.cmd --url https://github.com/whistlegraph/aesthetic-computer --token YOUR_TOKEN"
Write-Host "   .\svc.sh install"
Write-Host "   .\svc.sh start"
Write-Host ""
Write-Host "Log saved to: $LogFile" -ForegroundColor Gray
