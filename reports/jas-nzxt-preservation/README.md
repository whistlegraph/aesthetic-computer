# jas-nzxt Fleet OS preservation record

Preserved during the 22 July 2026 `jas-nzxt` cleanup and maintenance window.

## Accepted artifact

- Board: `octopus`
- Build ID: `R152-16749.0.0-d2026_07_16_063041-a1`
- Raw image size: `7,713,405,440` bytes
- Raw image SHA-256: `8bfab7e497df22d63c0379cf425f8a7fe56a6e5acc28c0c298e181546c9ae2c0`
- Compressed archive: `accepted-octopus-R152-16749.0.0-20260716.bin.zst`
- Compressed size: `919,359,674` bytes
- Compressed SHA-256: `d3701cb126edefd39c73bfce2f5176efb0057dfa15251450394c1306ef4aefdd`
- Compression verification: `zstd -t` passed and reported the original `7,713,405,440` bytes.

The compressed archive was verified byte-for-byte at both locations:

- Working source: `jastow:/home/me/fleet-os-builds/preservation/accepted-octopus-R152-16749.0.0-20260716.bin.zst`
- Durable fleet copy: `silo:/var/lib/aesthetic-computer/preservation/fleet-os/accepted-octopus-R152-16749.0.0-20260716.bin.zst`

The temporary copy on Neo was removed after the Silo hash matched, to avoid
leaving Neo's nearly full system disk under additional pressure.

## Setup files

The two previously untracked files from the otherwise-clean AC clone on
`jas-nzxt` are preserved in this directory:

- `docs/native-fedora.md` — SHA-256 `150fe03e9364d8f64260cf4e0d05bec1f4360d67f48967f1754198e8b69e16ac`
- `scripts/fedora-native-setup.sh` — SHA-256 `faaebf091d21b30f4b66c0e74e95686662e0934d8ad859b11aceba064c992ecc`

The full ChromiumOS checkout, SDK/package caches, build scripts, private state,
and accepted raw image remain on `jastow`; they were intentionally excluded
from cleanup.
