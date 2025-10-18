#!/bin/bash
# Quick setup script for false.work UE5 builder using existing GCP keys
# Uses a smaller, cheaper VM configuration

set -e

echo "=== Setting up UE5 Builder VM for false.work (Budget Config) ==="
echo ""

# Configuration
PROJECT_ID="aesthetic-computer"
ZONE="us-central1-a"
MACHINE_NAME="ue5-builder-falsework"
MACHINE_TYPE="n2-standard-8"  # 8 vCPU, 32GB RAM - Much cheaper!
BOOT_DISK_SIZE="300GB"  # Smaller disk
SERVICE_ACCOUN1LAO//dIS89yqq<T_KEY="../../../aesthetic-computer-vault/nanos/gcp-service-key.json"

echo "Configuration:"
echo "  Project: $PROJECT_ID"
echo "  Zone: $ZONE"
echo "  Machine: $MACHINE_TYPE (8 vCPU, 32GB RAM)"
echo "  Disk: $BOOT_DISK_SIZE SSD"
echo "  Estimated Cost: ~\$200-250/month (24/7) or ~\$80-100/month (8hrs/day)"
echo ""

# Check if gcloud is installed
if ! command -v gcloud &> /dev/null; then
    echo "❌ gcloud CLI not found!"
    echo "Install from: https://cloud.google.com/sdk/docs/install"
    exit 1
fi

# Check if service account key exists
if [ ! -f "$SERVICE_ACCOUNT_KEY" ]; then
    echo "❌ Service account key not found: $SERVICE_ACCOUNT_KEY"
    exit 1
fi

echo "✓ Found GCP service key"
echo ""

# Authenticate with service account
echo "[1/6] Authenticating with GCP..."
gcloud auth activate-service-account --key-file="$SERVICE_ACCOUNT_KEY" --project="$PROJECT_ID"
gcloud config set project "$PROJECT_ID"

echo "✓ Authenticated!"
echo ""

# Check if VM already exists
echo "[2/6] Checking if VM already exists..."
if gcloud compute instances describe $MACHINE_NAME --zone=$ZONE &>/dev/null; then
    echo "⚠️  VM '$MACHINE_NAME' already exists!"
    read -p "Delete and recreate? (yes/no) " -r
    if [[ $REPLY =~ ^[Yy]es$ ]]; then
        echo "Deleting existing VM..."
        gcloud compute instances delete $MACHINE_NAME --zone=$ZONE --quiet
        sleep 5
    else
        echo "Keeping existing VM. Exiting."
        exit 0
    fi
fi

echo "[3/6] Creating Windows Server VM with startup script..."

# Create startup script that runs on first boot
cat > /tmp/windows-startup.ps1 << 'STARTUP_SCRIPT'
# Download and run bootstrap script
$BootstrapUrl = "https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/false.work/unreal-builder/scripts/bootstrap-windows.ps1"
$BootstrapScript = "C:\bootstrap.ps1"
Invoke-WebRequest -Uri $BootstrapUrl -OutFile $BootstrapScript
PowerShell -ExecutionPolicy Bypass -File $BootstrapScript
STARTUP_SCRIPT

gcloud compute instances create $MACHINE_NAME \
  --project=$PROJECT_ID \
  --zone=$ZONE \
  --machine-type=$MACHINE_TYPE \
  --image-family=windows-2022 \
  --image-project=windows-cloud \
  --boot-disk-size=$BOOT_DISK_SIZE \
  --boot-disk-type=pd-ssd \
  --metadata=enable-guest-attributes=TRUE \
  --metadata-from-file=windows-startup-script-ps1=/tmp/windows-startup.ps1 \
  --tags=ue5-builder,https-server \
  --scopes=cloud-platform

echo "✓ VM created!"
echo ""

echo "[4/6] Waiting for VM to boot (60 seconds)..."
sleep 60

echo "[5/6] Setting Windows password..."
gcloud compute reset-windows-password $MACHINE_NAME \
  --zone=$ZONE \
  --user=builduser \
  --quiet

echo ""
echo "[6/6] Configuring firewall..."

# Create firewall rule for RDP if it doesn't exist
if ! gcloud compute firewall-rules describe allow-rdp-ue5-builder --project=$PROJECT_ID &>/dev/null; then
    gcloud compute firewall-rules create allow-rdp-ue5-builder \
        --project=$PROJECT_ID \
        --direction=INGRESS \
        --priority=1000 \
        --network=default \
        --action=ALLOW \
        --rules=tcp:3389 \
        --source-ranges=0.0.0.0/0 \
        --target-tags=ue5-builder
    
    echo "✓ Firewall rule created"
else
    echo "✓ Firewall rule already exists"
fi

echo ""
echo "=== VM Ready! ==="
echo ""

# Get external IP
EXTERNAL_IP=$(gcloud compute instances describe $MACHINE_NAME --zone=$ZONE --format="get(networkInterfaces[0].accessConfigs[0].natIP)")

echo "Connection Details:"
echo "  External IP: $EXTERNAL_IP"
echo "  Username: builduser"
echo "  Password: (shown above)"
echo ""
echo "💰 Cost Estimate:"
echo "  24/7: ~\$200-250/month"
echo "  8hrs/day: ~\$80-100/month"
echo "  Preemptible: ~\$60-80/month"
echo ""
echo "Connect via RDP:"
echo "  Mac: Microsoft Remote Desktop app"
echo "  Linux: rdesktop $EXTERNAL_IP"
echo "  Windows: mstsc /v:$EXTERNAL_IP"
echo ""
echo "⏸️  Stop VM when not building (to save \$):"
echo "  gcloud compute instances stop $MACHINE_NAME --zone=$ZONE"
echo ""
echo "▶️  Start VM:"
echo "  gcloud compute instances start $MACHINE_NAME --zone=$ZONE"
echo ""
echo "🗑️  Delete VM:"
echo "  gcloud compute instances delete $MACHINE_NAME --zone=$ZONE"
echo ""

# Save connection info
cat > gcp-vm-connection.txt << EOF
false.work UE5 Builder VM
=========================
Created: $(date)
Project: $PROJECT_ID
Zone: $ZONE
VM Name: $MACHINE_NAME
External IP: $EXTERNAL_IP
Username: builduser

Machine Type: $MACHINE_TYPE (8 vCPU, 32GB RAM)
Disk: $BOOT_DISK_SIZE SSD

RDP Connection:
  $EXTERNAL_IP

Stop VM (save costs):
  gcloud compute instances stop $MACHINE_NAME --zone=$ZONE

Start VM:
  gcloud compute instances start $MACHINE_NAME --zone=$ZONE

Check Status:
  gcloud compute instances describe $MACHINE_NAME --zone=$ZONE

SSH (for management):
  gcloud compute ssh $MACHINE_NAME --zone=$ZONE

Delete VM:
  gcloud compute instances delete $MACHINE_NAME --zone=$ZONE --quiet

Next Steps:
1. Connect via RDP
2. Install: UE5, Visual Studio 2022, Perforce CLI, Git
3. Run: false.work/unreal-builder/scripts/setup-build-machine.ps1
4. Install GitHub Actions runner
5. Test build!

See: false.work/unreal-builder/SETUP-GUIDE-FOR-FALSEWORK.md
EOF

echo "✓ Connection info saved to: gcp-vm-connection.txt"
echo ""
echo "Next: Connect via RDP and follow SETUP-GUIDE-FOR-FALSEWORK.md"
