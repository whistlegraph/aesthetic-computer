#!/bin/bash
# Monitor iOS build and device logs
set -e

# Load credentials
source /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env

echo "=== iOS Build & Device Log Monitor ==="
echo ""
echo "Commands:"
echo "  1. Monitor build:  ./monitor-ios-deployment.sh build"
echo "  2. Monitor device: ./monitor-ios-deployment.sh device"
echo "  3. Deploy app:    ./monitor-ios-deployment.sh deploy"
echo "  4. Launch app:    ./monitor-ios-deployment.sh launch"
echo ""

DEVICE_ID="46720BDD-8807-53C8-AA2E-6977BCA606D3"

case "${1:-build}" in
  build)
    echo "📦 Monitoring iOS build progress..."
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no falsework@host.docker.internal \
      'tail -f /tmp/ios-build-latest.log'
    ;;
  
  device)
    echo "📱 Streaming device logs from aesthetic.computer..."
    sshpass -p "$MAC_PASSWORD" ssh -t -o StrictHostKeyChecking=no falsework@host.docker.internal \
      "/usr/bin/log stream --device $DEVICE_ID --predicate 'processImagePath CONTAINS \"SpiderLily\"' --style compact 2>&1"
    ;;
  
  deploy)
    echo "🚀 Deploying latest build to aesthetic.computer..."
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no falsework@host.docker.internal \
      "xcrun devicectl device install app --device $DEVICE_ID ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app"
    ;;
  
  launch)
    echo "▶️  Launching SpiderLily on aesthetic.computer..."
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no falsework@host.docker.internal \
      "xcrun devicectl device process launch --device $DEVICE_ID work.false.SpiderLily"
    ;;
  
  *)
    echo "Unknown command: $1"
    exit 1
    ;;
esac
