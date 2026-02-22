#!/bin/bash
# prepare-theme.sh — Generate FedAC GRUB theme assets
#
# Creates:
#   - background.png   (1920x1080 dark purple + centered pals logo)
#   - DejaVuSansBold36.pf2  (large menu font)
#   - DejaVuSans18.pf2      (subtitle font)
#   - DejaVuSans10.pf2      (timer font)
#   - select_c.png, menu_c.png (selection/menu nine-patch slices)
#
# Requirements: ImageMagick (convert), grub2-mkfont
# Run from repo root or fedac/grub-theme/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PALS_SRC="$SCRIPT_DIR/../plymouth/pals.png"
OUT_DIR="$SCRIPT_DIR"

RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}=== FedAC GRUB Theme Builder ===${NC}"

# ── Check deps ──
for cmd in convert grub2-mkfont; do
  if ! command -v "$cmd" &>/dev/null; then
    # Try grub-mkfont as fallback
    if [ "$cmd" = "grub2-mkfont" ] && command -v grub-mkfont &>/dev/null; then
      MKFONT="grub-mkfont"
      continue
    fi
    echo -e "${RED}Missing: $cmd${NC}"
    echo "Install: sudo dnf install ImageMagick grub2-tools-minimal"
    exit 1
  fi
done
MKFONT="${MKFONT:-grub2-mkfont}"

if [ ! -f "$PALS_SRC" ]; then
  echo -e "${RED}Missing pals.png at $PALS_SRC${NC}"
  exit 1
fi

# ── 1. Background image (1920x1080 purple + pals centered in upper portion) ──
echo -e "${CYAN}[1/4] Creating background...${NC}"

# pals.png is 400x240 — scale to ~600px wide for visibility on 1080p
convert \
  -size 1920x1080 "xc:#1a0a2e" \
  \( "$PALS_SRC" -resize 600x360 \) \
  -gravity North -geometry +0+120 -composite \
  "$OUT_DIR/background.png"

echo -e "  ${GREEN}background.png (1920x1080)${NC}"

# ── 2. Generate .pf2 fonts ──
echo -e "${CYAN}[2/4] Generating fonts...${NC}"

# Find DejaVu Sans Bold
FONT_BOLD=""
for path in \
  /usr/share/fonts/dejavu-sans-fonts/DejaVuSans-Bold.ttf \
  /usr/share/fonts/dejavu/DejaVuSans-Bold.ttf \
  /usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf \
  /usr/share/fonts/TTF/DejaVuSans-Bold.ttf; do
  if [ -f "$path" ]; then
    FONT_BOLD="$path"
    break
  fi
done

FONT_REG=""
for path in \
  /usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf \
  /usr/share/fonts/dejavu/DejaVuSans.ttf \
  /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf \
  /usr/share/fonts/TTF/DejaVuSans.ttf; do
  if [ -f "$path" ]; then
    FONT_REG="$path"
    break
  fi
done

if [ -z "$FONT_BOLD" ] || [ -z "$FONT_REG" ]; then
  echo -e "${RED}DejaVu Sans fonts not found${NC}"
  echo "Install: sudo dnf install dejavu-sans-fonts"
  exit 1
fi

$MKFONT -s 36 -o "$OUT_DIR/DejaVuSansBold36.pf2" "$FONT_BOLD"
echo -e "  ${GREEN}DejaVuSansBold36.pf2${NC}"

$MKFONT -s 18 -o "$OUT_DIR/DejaVuSans18.pf2" "$FONT_REG"
echo -e "  ${GREEN}DejaVuSans18.pf2${NC}"

$MKFONT -s 10 -o "$OUT_DIR/DejaVuSans10.pf2" "$FONT_REG"
echo -e "  ${GREEN}DejaVuSans10.pf2${NC}"

# ── 3. Selection bar (nine-patch style) ──
echo -e "${CYAN}[3/4] Creating selection bar...${NC}"

# Selected item highlight — semi-transparent magenta bar
convert -size 10x52 "xc:#8855bb88" "$OUT_DIR/select_c.png"
echo -e "  ${GREEN}select_c.png${NC}"

# Menu background — very subtle dark overlay
convert -size 10x10 "xc:#1a0a2e00" "$OUT_DIR/menu_c.png"
echo -e "  ${GREEN}menu_c.png${NC}"

# ── 4. Summary ──
echo ""
echo -e "${CYAN}[4/4] Theme assets ready:${NC}"
ls -la "$OUT_DIR"/*.png "$OUT_DIR"/*.pf2 2>/dev/null
echo ""
echo -e "${GREEN}Done!${NC} Theme files are in $OUT_DIR"
echo "These will be installed to the USB EFI partition by patch-usb.sh / make-usb.sh"
