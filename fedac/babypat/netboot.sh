#!/usr/bin/env bash
# netboot.sh — serve babypat over PXE (ethernet network boot)
# Connect target machine to host via ethernet, enable PXE boot in BIOS, done.
# Uses dnsmasq as DHCP proxy + TFTP server. No USB stick needed.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EFI="${1:-${SCRIPT_DIR}/babypat-tiny.efi}"
IFACE="${2:-}"

if [ ! -f "${EFI}" ]; then
    echo "Build first: make tiny"
    exit 1
fi

echo "=== babypat netboot ==="
echo "EFI: ${EFI} ($(wc -c < "${EFI}" | tr -d ' ') bytes)"

# Find ethernet interface if not specified
if [ -z "${IFACE}" ]; then
    for dev in /sys/class/net/e*; do
        [ -d "$dev" ] && IFACE=$(basename "$dev") && break
    done
    # Also check for enp* style names
    if [ -z "${IFACE}" ]; then
        for dev in /sys/class/net/enp*; do
            [ -d "$dev" ] && IFACE=$(basename "$dev") && break
        done
    fi
fi

if [ -z "${IFACE}" ]; then
    echo "No ethernet interface found."
    echo "Usage: ./netboot.sh [efi-file] [interface]"
    exit 1
fi

echo "Interface: ${IFACE}"

# Set up TFTP directory
TFTP_DIR=$(mktemp -d)
mkdir -p "${TFTP_DIR}/EFI/BOOT"
cp "${EFI}" "${TFTP_DIR}/BOOTX64.EFI"
cp "${EFI}" "${TFTP_DIR}/EFI/BOOT/BOOTX64.EFI"

# Assign a static IP to the ethernet interface
SUBNET="10.66.0"
HOST_IP="${SUBNET}.1"
RANGE_START="${SUBNET}.10"
RANGE_END="${SUBNET}.50"

echo "Setting ${IFACE} to ${HOST_IP}/24..."
sudo ip addr flush dev "${IFACE}" 2>/dev/null || true
sudo ip addr add "${HOST_IP}/24" dev "${IFACE}"
sudo ip link set "${IFACE}" up

echo "Starting PXE server (DHCP + TFTP)..."
echo "  DHCP range: ${RANGE_START} - ${RANGE_END}"
echo "  TFTP root:  ${TFTP_DIR}"
echo ""
echo "Now boot the target machine from network (PXE/UEFI Network Boot)."
echo "Press Ctrl+C to stop."
echo ""

# dnsmasq in foreground:
#   --dhcp-range: assign IPs to PXE clients
#   --dhcp-boot: tell UEFI clients to fetch BOOTX64.EFI
#   --enable-tftp: serve files via TFTP
#   --dhcp-match: match UEFI x86_64 clients (arch 7 or 9)
#   --pxe-service: offer the EFI file to UEFI clients
cleanup() {
    echo "Stopping..."
    sudo ip addr flush dev "${IFACE}" 2>/dev/null || true
    rm -rf "${TFTP_DIR}"
}
trap cleanup EXIT

sudo dnsmasq \
    --no-daemon \
    --interface="${IFACE}" \
    --bind-interfaces \
    --dhcp-range="${RANGE_START},${RANGE_END},255.255.255.0,1h" \
    --dhcp-boot="BOOTX64.EFI" \
    --dhcp-option=6,"${HOST_IP}" \
    --dhcp-match=set:efi-x86_64,option:client-arch,7 \
    --dhcp-match=set:efi-x86_64,option:client-arch,9 \
    --tag-if=set:efi,tag:efi-x86_64 \
    --dhcp-boot=tag:efi,"BOOTX64.EFI" \
    --enable-tftp \
    --tftp-root="${TFTP_DIR}" \
    --log-dhcp \
    --log-queries \
    --port=0
