#!/usr/bin/env python3
# make-icon.py — emit a 1024×1024 Notepat icon.png using only stdlib.
# Design: rounded-square teal background, yellow "N" carved out via a simple
# pixel-space rasterizer. Intentionally zero-dep so it runs on any macOS.
#
# Output: absolute path passed as argv[1].
import struct, sys, zlib, math

W, H = 1024, 1024
BG = (45, 90, 110, 255)       # deep teal
FG = (255, 210, 90, 255)       # warm yellow
CORNER = 180                    # px radius on the rounded square

# "N" geometry — left bar, right bar, diagonal.
MARGIN = 220
BAR_W  = 110
N_LEFT_X  = MARGIN
N_RIGHT_X = W - MARGIN - BAR_W
N_TOP     = MARGIN
N_BOT     = H - MARGIN

def inside_rounded_rect(x, y):
    # 32px margin so the rounded square doesn't touch the icon edges (macOS
    # adds its own mask for menu bar renderings).
    pad = 24
    if x < pad or x >= W - pad or y < pad or y >= H - pad:
        return False
    # Corner cutouts
    cx, cy = None, None
    if x < pad + CORNER and y < pad + CORNER:
        cx, cy = pad + CORNER, pad + CORNER
    elif x >= W - pad - CORNER and y < pad + CORNER:
        cx, cy = W - pad - CORNER - 1, pad + CORNER
    elif x < pad + CORNER and y >= H - pad - CORNER:
        cx, cy = pad + CORNER, H - pad - CORNER - 1
    elif x >= W - pad - CORNER and y >= H - pad - CORNER:
        cx, cy = W - pad - CORNER - 1, H - pad - CORNER - 1
    if cx is not None:
        dx = x - cx; dy = y - cy
        return dx*dx + dy*dy <= CORNER*CORNER
    return True

def inside_n(x, y):
    if y < N_TOP or y >= N_BOT:
        return False
    # Left bar
    if N_LEFT_X <= x < N_LEFT_X + BAR_W:
        return True
    # Right bar
    if N_RIGHT_X <= x < N_RIGHT_X + BAR_W:
        return True
    # Diagonal — from top-left inner edge to bottom-right inner edge
    span = N_BOT - N_TOP
    t = (y - N_TOP) / span
    dx = N_LEFT_X + BAR_W + t * (N_RIGHT_X - (N_LEFT_X + BAR_W))
    if abs(x - dx) <= BAR_W * 0.7:
        return True
    return False

pixels = bytearray()
for y in range(H):
    row = bytearray()
    for x in range(W):
        if not inside_rounded_rect(x, y):
            row += bytes((0, 0, 0, 0))
        elif inside_n(x, y):
            row += bytes(FG)
        else:
            row += bytes(BG)
    pixels += row

def chunk(tag, data):
    crc = zlib.crc32(tag + data) & 0xFFFFFFFF
    return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", crc)

header = b"\x89PNG\r\n\x1a\n"
ihdr = struct.pack(">IIBBBBB", W, H, 8, 6, 0, 0, 0)  # RGBA8
raw = bytearray()
stride = W * 4
for y in range(H):
    raw.append(0)  # filter: None
    raw += pixels[y * stride : (y + 1) * stride]
idat = zlib.compress(bytes(raw), 9)

png = header + chunk(b"IHDR", ihdr) + chunk(b"IDAT", idat) + chunk(b"IEND", b"")
with open(sys.argv[1], "wb") as f:
    f.write(png)
print(f"wrote {sys.argv[1]} ({len(png)} bytes)")
