#!/bin/bash
# Google Cloud VM Setup for UE5 Builder (Linux/Mac version)

set -e

PROJECT_ID="${1:-$(gcloud config get-value project 2>/dev/null)}"
ZONE="${2:-us-central1-a}"
MACHINE_NAME="ue5-builder-spiderlily"
MACHINE_TYPE="n2-standard-16"  # 16 vCPU, 64GB RAM
BOOT_DISK_SIZE="500GB"

echo "=== Google Cloud VM Setup for UE5 Builder ==="
echo ""

# Check if gcloud is installed
if ! command -v gcloud &> /dev/null; then
    echo "❌ gcloud CLI not found!"
    echo "Install from: https://cloud.google.com/sdk/docs/install"
    exit 1
fi

# Check project ID
if [ -z "$PROJECT_ID" ]; then
    echo "❌ No GCP project configured"
    echo "Run: gcloud config set project YOUR_PROJECT_ID"
    echo "Or pass project ID as first argument"
    exit 1
fi

echo "Using GCP Project: $PROJECT_ID"
echo "Zone: $ZONE"
echo "Machine: $MACHINE_TYPE ($BOOT_DISK_SIZE disk)"
echo ""

read -p "Create VM? This will incur GCP charges. (yes/no) " -r
if [[ ! $REPLY =~ ^[Yy]es$ ]]; then
    echo "Cancelled."
    exit 0
fi

echo ""
echo "[1/5] Creating Windows Server VM..."

gcloud compute instances create $MACHINE_NAME \
  --project=$PROJECT_ID \
  --zone=$ZONE \
  --machine-type=$MACHINE_TYPE \
  --image-family=windows-2022 \
  --image-project=windows-cloud \
  --boot-disk-size=$BOOT_DISK_SIZE \
  --boot-disk-type=pd-ssd \
  --metadata=enable-guest-attributes=TRUE \
  --tags=ue5-builder,https-server \
  --scopes=cloud-platform

echo "✓ VM created successfully!"
echo ""

echo "[2/5] Waiting for VM to boot..."
sleep 60

echo "[3/5] Setting Windows password..."
gcloud compute reset-windows-password $MACHINE_NAME --zone=$ZONE --user=builduser --quiet

echo ""
echo "[4/5] Getting connection info..."

EXTERNAL_IP=$(gcloud compute instances describe $MACHINE_NAME --zone=$ZONE --format="get(networkInterfaces[0].accessConfigs[0].natIP)")

echo ""
echo "=== VM Created Successfully! ==="
echo ""
echo "Connection Details:"
echo "  External IP: $EXTERNAL_IP"
echo "  Username: builduser"
echo "  Password: (shown above)"
echo ""
echo "Connect via RDP:"
echo "  Mac: Use 'Microsoft Remote Desktop' app"
echo "  Linux: rdesktop $EXTERNAL_IP or remmina"
echo ""

echo "[5/5] Configuring firewall..."

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
echo "=== Next Steps ==="
echo ""
echo "1. Connect to VM via RDP: $EXTERNAL_IP"
echo ""
echo "2. Install required software:"
echo "   - Unreal Engine 5"
echo "   - Visual Studio 2022"
echo "   - Perforce CLI (P4)"
echo "   - Git for Windows"
echo ""
echo "3. Run setup script (see SETUP-GUIDE-FOR-FALSEWORK.md)"
echo ""
echo "4. Install GitHub Actions runner"
echo ""
echo "=== Cost Estimate ==="
echo "Running 24/7: ~\$400-500/month"
echo "Running 8hrs/day: ~\$150-200/month"
echo ""
echo "💡 Stop VM when not in use:"
echo "   gcloud compute instances stop $MACHINE_NAME --zone=$ZONE"
echo ""
echo "To delete VM:"
echo "   gcloud compute instances delete $MACHINE_NAME --zone=$ZONE"
echo ""

# Save connection info
cat > gcp-vm-info.txt << EOF
GCP VM Connection Info
======================
VM Name: $MACHINE_NAME
Project: $PROJECT_ID
Zone: $ZONE
External IP: $EXTERNAL_IP
Username: builduser

Stop VM (save costs):
  gcloud compute instances stop $MACHINE_NAME --zone=$ZONE

Start VM:
  gcloud compute instances start $MACHINE_NAME --zone=$ZONE

SSH (for management):
  gcloud compute ssh $MACHINE_NAME --zone=$ZONE

Delete VM:
  gcloud compute instances delete $MACHINE_NAME --zone=$ZONE --quiet
EOF

echo "Connection info saved to: gcp-vm-info.txt"
