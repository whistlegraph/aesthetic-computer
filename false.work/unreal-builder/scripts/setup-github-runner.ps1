# Setup GitHub Actions Self-Hosted Runner
# This script configures the GitHub Actions runner on the build VM

param(
    [Parameter(Mandatory=$true)]
    [string]$GitHubToken  # Get from: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners/new
)

$ErrorActionPreference = "Stop"

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Cyan
Write-Host "║   Setting up GitHub Actions Self-Hosted Runner ║" -ForegroundColor Cyan
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Cyan
Write-Host ""

# Runner directory should already exist from bootstrap
$RunnerPath = "C:\actions-runner"
if (!(Test-Path $RunnerPath)) {
    Write-Error "Runner directory not found. Run bootstrap-windows-minimal.ps1 first."
    exit 1
}

cd $RunnerPath

# Configure the runner
Write-Host "Configuring runner..." -ForegroundColor Yellow
Write-Host "  Organization: whistlegraph/aesthetic-computer" -ForegroundColor Gray
Write-Host "  Labels: self-hosted, Windows, X64, ue5-builder" -ForegroundColor Gray

try {
    .\config.cmd `
        --url https://github.com/whistlegraph/aesthetic-computer `
        --token $GitHubToken `
        --name "ue5-builder-falsework" `
        --labels "self-hosted,Windows,X64,ue5-builder" `
        --work "_work" `
        --unattended `
        --replace
    
    if ($LASTEXITCODE -ne 0) {
        throw "Runner configuration failed"
    }
    
    Write-Host "✓ Runner configured" -ForegroundColor Green
} catch {
    Write-Error "Failed to configure runner: $_"
    exit 1
}

# Install runner as a Windows service
Write-Host ""
Write-Host "Installing runner as Windows service..." -ForegroundColor Yellow

try {
    .\svc.cmd install
    if ($LASTEXITCODE -ne 0) {
        throw "Service installation failed"
    }
    
    Write-Host "✓ Service installed" -ForegroundColor Green
} catch {
    Write-Error "Failed to install service: $_"
    exit 1
}

# Start the service
Write-Host ""
Write-Host "Starting runner service..." -ForegroundColor Yellow

try {
    .\svc.cmd start
    if ($LASTEXITCODE -ne 0) {
        throw "Service start failed"
    }
    
    Write-Host "✓ Service started" -ForegroundColor Green
} catch {
    Write-Error "Failed to start service: $_"
    exit 1
}

# Copy build script to C:\scripts
Write-Host ""
Write-Host "Setting up build scripts..." -ForegroundColor Yellow
$ScriptsDir = "C:\scripts"
if (!(Test-Path $ScriptsDir)) {
    New-Item -ItemType Directory -Path $ScriptsDir | Out-Null
}

# The daily-build.ps1 script should be copied here from the repo
Write-Host "  Create C:\scripts\daily-build.ps1 from the repo" -ForegroundColor Gray
Write-Host "  Path: false.work/unreal-builder/scripts/daily-build.ps1" -ForegroundColor Gray

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "║          GitHub Runner Setup Complete!         ║" -ForegroundColor Green
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""
Write-Host "✅ Runner is now active and waiting for jobs" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "1. Copy daily-build.ps1 to C:\scripts\" -ForegroundColor White
Write-Host "2. Add secrets to GitHub repo:" -ForegroundColor White
Write-Host "   - P4_SERVER: ssl:falsework.helixcore.io:1666" -ForegroundColor Gray
Write-Host "   - P4_USER: machine" -ForegroundColor Gray
Write-Host "   - P4_PASSWORD: (from vault)" -ForegroundColor Gray
Write-Host "   - P4_WORKSPACE: spiderlily_build_workspace" -ForegroundColor Gray
Write-Host "3. Trigger workflow from GitHub Actions tab" -ForegroundColor White
Write-Host ""
Write-Host "View runner status: https://github.com/whistlegraph/aesthetic-computer/settings/actions/runners" -ForegroundColor Cyan
Write-Host ""

exit 0
