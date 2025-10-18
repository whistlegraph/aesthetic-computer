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
Write-Host "[1/6] Installing Chocolatey..." -ForegroundColor Yellow
if (!(Get-Command choco -ErrorAction SilentlyContinue)) {
    Write-Host "  Installing Chocolatey package manager..." -ForegroundColor Cyan
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
    Write-Host "  Refreshing environment PATH..." -ForegroundColor Cyan
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Chocolatey installed" -ForegroundColor Green
} else {
    Write-Host "✓ Chocolatey already installed, skipping" -ForegroundColor Green
}

# Install Git
Write-Host ""
Write-Host "[2/6] Installing Git..." -ForegroundColor Yellow
if (!(Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Host "  Downloading and installing Git..." -ForegroundColor Cyan
    choco install git -y --no-progress
    Write-Host "  Refreshing environment PATH..." -ForegroundColor Cyan
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ Git installed" -ForegroundColor Green
} else {
    Write-Host "✓ Git already installed, skipping" -ForegroundColor Green
}

# Install Perforce CLI
Write-Host ""
Write-Host "[3/6] Installing Perforce CLI..." -ForegroundColor Yellow
$P4Url = "https://cdist2.perforce.com/perforce/r24.1/bin.ntx64/p4.exe"
$P4Path = "C:\Windows\System32\p4.exe"
if (!(Test-Path $P4Path)) {
    Write-Host "  Downloading p4.exe..." -ForegroundColor Cyan
    Invoke-WebRequest -Uri $P4Url -OutFile $P4Path -UseBasicParsing
    Write-Host "✓ Perforce CLI installed" -ForegroundColor Green
} else {
    Write-Host "✓ Perforce CLI already installed, skipping" -ForegroundColor Green
}

# Setup directories and GitHub runner
Write-Host ""
Write-Host "[4/6] Setting up directories and GitHub Actions runner..." -ForegroundColor Yellow

# Create build directories on C: drive (D: may not exist on single-disk VMs)
Write-Host "  Creating directories..." -ForegroundColor Cyan
$Dirs = @("C:\Perforce", "C:\Builds", "C:\Builds\Logs")
foreach ($Dir in $Dirs) {
    if (!(Test-Path $Dir)) {
        New-Item -ItemType Directory -Path $Dir -Force | Out-Null
        Write-Host "    ✓ Created $Dir" -ForegroundColor Gray
    } else {
        Write-Host "    ✓ $Dir already exists" -ForegroundColor Gray
    }
}

# Download GitHub Actions runner
Write-Host "  Downloading GitHub Actions runner (~100MB, 1-2 minutes)..." -ForegroundColor Cyan
$RunnerUrl = "https://github.com/actions/runner/releases/download/v2.311.0/actions-runner-win-x64-2.311.0.zip"
$RunnerZip = "C:\actions-runner.zip"
if (!(Test-Path "C:\actions-runner")) {
    New-Item -ItemType Directory -Path "C:\actions-runner" -Force | Out-Null
    
    # Enable progress bar for download
    $ProgressPreference = 'Continue'
    Write-Host "    Downloading from GitHub (~100MB)..." -ForegroundColor Gray
    Invoke-WebRequest -Uri $RunnerUrl -OutFile $RunnerZip -UseBasicParsing
    
    Write-Host "    Extracting runner files..." -ForegroundColor Gray
    Expand-Archive -Path $RunnerZip -DestinationPath "C:\actions-runner" -Force
    Remove-Item $RunnerZip
    Write-Host "    ✓ Runner extracted to C:\actions-runner" -ForegroundColor Gray
} else {
    Write-Host "  ✓ GitHub Actions runner already exists, skipping" -ForegroundColor Green
}

# Install Epic Games Launcher via Chocolatey
Write-Host ""
Write-Host "[5/6] Installing Epic Games Launcher..." -ForegroundColor Yellow
# Check if Epic is already installed
$EpicPath = "C:\Program Files (x86)\Epic Games\Launcher\Portal\Binaries\Win64\EpicGamesLauncher.exe"
if (!(Test-Path $EpicPath)) {
    Write-Host "  Installing Epic Games Launcher via Chocolatey (~90MB, 2-3 minutes)..." -ForegroundColor Cyan
    choco install epicgameslauncher -y --no-progress
    Write-Host "✓ Epic Games Launcher installed" -ForegroundColor Green
} else {
    Write-Host "✓ Epic Games Launcher already installed, skipping" -ForegroundColor Green
}

# Install .NET 8.0 SDK (required for UnrealBuildTool)
Write-Host ""
Write-Host "[6/6] Installing .NET 8.0 SDK..." -ForegroundColor Yellow
if (!(Test-Path "C:\Program Files\dotnet\dotnet.exe")) {
    Write-Host "  Installing .NET 8.0 SDK via Chocolatey (~150MB, 2-3 minutes)..." -ForegroundColor Cyan
    choco install dotnet-8.0-sdk -y --no-progress
    Write-Host "  Refreshing environment PATH..." -ForegroundColor Cyan
    $env:PATH = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "✓ .NET 8.0 SDK installed" -ForegroundColor Green
} else {
    Write-Host "✓ .NET 8.0 SDK already installed, skipping" -ForegroundColor Green
}

# Optimize Windows
Write-Host ""
Write-Host "Optimizing Windows settings..." -ForegroundColor Cyan
Write-Host "  Disabling Windows Search indexing..." -ForegroundColor Gray
Stop-Service WSearch -Force -ErrorAction SilentlyContinue
Set-Service WSearch -StartupType Disabled -ErrorAction SilentlyContinue
Write-Host "  Setting high performance power plan..." -ForegroundColor Gray
powercfg /setactive 8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c -ErrorAction SilentlyContinue

Write-Host ""
Write-Host "========================================" -ForegroundColor Green
Write-Host "✓ Bootstrap Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green

Stop-Transcript

Write-Host ""
Write-Host "=== Bootstrap Complete! ===" -ForegroundColor Green
Write-Host ""
Write-Host "✅ Installed:" -ForegroundColor Cyan
Write-Host "  - Git for Windows"
Write-Host "  - Perforce CLI (P4)"
Write-Host "  - Epic Games Launcher"
Write-Host "  - .NET 8.0 SDK"
Write-Host "  - GitHub Actions runner directory"
Write-Host "  - Build directories created"
Write-Host ""
Write-Host "⏭️  Next Steps:" -ForegroundColor Yellow
Write-Host ""
Write-Host "1. Launch Epic Games Launcher and install UE5 (~1-2 hours):"
Write-Host "   - Open Start Menu and search for 'Epic Games Launcher'"
Write-Host "   - Sign in (or create free Epic account)"
Write-Host "   - Go to 'Unreal Engine' tab → Install Engine"
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
