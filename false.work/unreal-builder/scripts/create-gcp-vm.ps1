# Google Cloud VM Setup for UE5 Builder
# Run this on your local machine (with gcloud CLI installed)

param(
    [string]$ProjectId = "",
    [string]$Zone = "us-central1-a",
    [string]$MachineName = "ue5-builder-spiderlily",
    [string]$MachineType = "n2-standard-16",  # 16 vCPU, 64GB RAM
    [string]$BootDiskSize = "500GB",
    [string]$ServiceAccountEmail = ""
)

Write-Host "=== Google Cloud VM Setup for UE5 Builder ===" -ForegroundColor Cyan
Write-Host ""

# Check if gcloud is installed
if (-not (Get-Command gcloud -ErrorAction SilentlyContinue)) {
    Write-Host "❌ gcloud CLI not found!" -ForegroundColor Red
    Write-Host "Install from: https://cloud.google.com/sdk/docs/install" -ForegroundColor Yellow
    exit 1
}

# Get project ID if not provided
if (-not $ProjectId) {
    $ProjectId = gcloud config get-value project 2>$null
    if (-not $ProjectId) {
        Write-Host "❌ No GCP project configured" -ForegroundColor Red
        Write-Host "Run: gcloud config set project YOUR_PROJECT_ID" -ForegroundColor Yellow
        exit 1
    }
}

Write-Host "Using GCP Project: $ProjectId" -ForegroundColor Green
Write-Host "Zone: $Zone" -ForegroundColor Green
Write-Host "Machine: $MachineType ($BootDiskSize disk)" -ForegroundColor Green
Write-Host ""

# Confirm creation
$confirm = Read-Host "Create VM? This will incur GCP charges. (yes/no)"
if ($confirm -ne "yes") {
    Write-Host "Cancelled." -ForegroundColor Yellow
    exit 0
}

Write-Host ""
Write-Host "[1/5] Creating Windows Server VM..." -ForegroundColor Yellow

# Create the VM
$createCmd = @"
gcloud compute instances create $MachineName \
  --project=$ProjectId \
  --zone=$Zone \
  --machine-type=$MachineType \
  --image-family=windows-2022 \
  --image-project=windows-cloud \
  --boot-disk-size=$BootDiskSize \
  --boot-disk-type=pd-ssd \
  --metadata=enable-guest-attributes=TRUE \
  --tags=ue5-builder,https-server \
  --scopes=cloud-platform
"@

if ($ServiceAccountEmail) {
    $createCmd += " --service-account=$ServiceAccountEmail"
}

Invoke-Expression $createCmd

if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to create VM" -ForegroundColor Red
    exit 1
}

Write-Host "✓ VM created successfully!" -ForegroundColor Green
Write-Host ""

Write-Host "[2/5] Waiting for VM to boot..." -ForegroundColor Yellow
Start-Sleep -Seconds 60

Write-Host "[3/5] Setting Windows password..." -ForegroundColor Yellow
gcloud compute reset-windows-password $MachineName --zone=$Zone --user=builduser --quiet

Write-Host ""
Write-Host "[4/5] Getting connection info..." -ForegroundColor Yellow

$externalIp = gcloud compute instances describe $MachineName --zone=$Zone --format="get(networkInterfaces[0].accessConfigs[0].natIP)"

Write-Host ""
Write-Host "=== VM Created Successfully! ===" -ForegroundColor Green
Write-Host ""
Write-Host "Connection Details:" -ForegroundColor Cyan
Write-Host "  External IP: $externalIp" -ForegroundColor White
Write-Host "  Username: builduser" -ForegroundColor White
Write-Host "  Password: (shown above)" -ForegroundColor White
Write-Host ""
Write-Host "Connect via RDP:" -ForegroundColor Yellow
Write-Host "  Windows: Open 'Remote Desktop Connection' and enter: $externalIp" -ForegroundColor White
Write-Host "  Mac: Use 'Microsoft Remote Desktop' app" -ForegroundColor White
Write-Host "  Linux: Use 'remmina' or 'rdesktop'" -ForegroundColor White
Write-Host ""

Write-Host "[5/5] Configuring firewall..." -ForegroundColor Yellow

# Create firewall rule for RDP if it doesn't exist
$firewallExists = gcloud compute firewall-rules list --filter="name=allow-rdp-ue5-builder" --format="value(name)" 2>$null
if (-not $firewallExists) {
    gcloud compute firewall-rules create allow-rdp-ue5-builder `
        --project=$ProjectId `
        --direction=INGRESS `
        --priority=1000 `
        --network=default `
        --action=ALLOW `
        --rules=tcp:3389 `
        --source-ranges=0.0.0.0/0 `
        --target-tags=ue5-builder
    
    Write-Host "✓ Firewall rule created" -ForegroundColor Green
} else {
    Write-Host "✓ Firewall rule already exists" -ForegroundColor Green
}

Write-Host ""
Write-Host "=== Next Steps ===" -ForegroundColor Cyan
Write-Host ""
Write-Host "1. Connect to VM via RDP using the credentials above" -ForegroundColor White
Write-Host ""
Write-Host "2. Once connected, open PowerShell as Administrator and run:" -ForegroundColor White
Write-Host "   # Download the setup script" -ForegroundColor Gray
Write-Host "   Invoke-WebRequest -Uri 'https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/false.work/unreal-builder/scripts/setup-build-machine.ps1' -OutFile 'C:\setup-build-machine.ps1'" -ForegroundColor Gray
Write-Host ""
Write-Host "   # Or clone the repo:" -ForegroundColor Gray
Write-Host "   cd C:\" -ForegroundColor Gray
Write-Host "   git clone https://github.com/whistlegraph/aesthetic-computer.git" -ForegroundColor Gray
Write-Host "   cd aesthetic-computer\false.work\unreal-builder" -ForegroundColor Gray
Write-Host ""
Write-Host "3. Install software (UE5, Visual Studio, Perforce, Git)" -ForegroundColor White
Write-Host "   See: SETUP-GUIDE-FOR-FALSEWORK.md" -ForegroundColor Gray
Write-Host ""
Write-Host "4. Run the build machine setup script" -ForegroundColor White
Write-Host ""
Write-Host "5. Install GitHub Actions runner" -ForegroundColor White
Write-Host ""
Write-Host "=== Cost Estimate ===" -ForegroundColor Cyan
Write-Host "Running 24/7: ~`$400-500/month" -ForegroundColor Yellow
Write-Host "Running 8hrs/day (work hours): ~`$150-200/month" -ForegroundColor Yellow
Write-Host "Preemptible (can be interrupted): ~`$120-150/month" -ForegroundColor Yellow
Write-Host ""
Write-Host "💡 Tip: Stop VM when not in use to save costs:" -ForegroundColor Cyan
Write-Host "   gcloud compute instances stop $MachineName --zone=$Zone" -ForegroundColor White
Write-Host ""
Write-Host "To delete VM:" -ForegroundColor Red
Write-Host "   gcloud compute instances delete $MachineName --zone=$Zone" -ForegroundColor White
Write-Host ""

# Save connection info to file
$connectionInfo = @"
GCP VM Connection Info
======================
VM Name: $MachineName
Project: $ProjectId
Zone: $Zone
External IP: $externalIp
Username: builduser

RDP Connection:
  mstsc /v:$externalIp

Stop VM (save costs):
  gcloud compute instances stop $MachineName --zone=$Zone

Start VM:
  gcloud compute instances start $MachineName --zone=$Zone

SSH (for management):
  gcloud compute ssh $MachineName --zone=$Zone

Delete VM:
  gcloud compute instances delete $MachineName --zone=$Zone --quiet
"@

Set-Content -Path "gcp-vm-info.txt" -Value $connectionInfo
Write-Host "Connection info saved to: gcp-vm-info.txt" -ForegroundColor Green
