# Frisbee (MacBook Neo J700 / T8140) — Aurora Linux bring-up status

_Last updated: 2026-10-14. Status: **paused; chosen resume path = Route B (in-place
power-domain patch, no wipe).** No wipe performed._

## Goal
Boot aurora Linux on Frisbee (second MacBook Neo, A18 Pro / T8140, `Mac17,5`,
`apple,j700`) with working **audio**, via the AC OS effort.

## TL;DR
The complete native boot chain is **built and working end-to-end**, but aurora's
kernel panics with a power-domain `SError` that is almost certainly a
**macOS-27-firmware incompatibility** (aurora qualifies the Neo only on macOS
**26.x**; Frisbee is on **27.0.1**). The clean fix is upstream (aurora supporting
macOS 27) or a firmware downgrade. **We chose to document and wait** rather than
wipe Frisbee on a hypothesis. Resuming later is cheap — see "How to resume".

## What works ✅
- **MTP keyboard/trackpad** in Linux (earlier milestone; see
  `memory/frisbee-acos-mtp-keyboard.md`).
- **Native two-stage boot chain**, hand-built to aurora's production design and
  **blessed on Frisbee's Linux volume**:
  - **Stage 1**: bare m1n1 (`RELEASE=1 CHAINLOADING=1 T8140_KIS_PROXY=1
    BUILTIN_LOGO=aurora`) with its `AURORA-S1-CFG01` config slot filled by
    `tools/fill_stage1_config.py` (esp PARTUUID + stage2 path). Small enough to
    bless (iBoot rejects boot objects much over ~m1n1's 0x350000 size — this is
    why bundling the kernel failed).
  - **Stage 2**: m1n1 (`ESP_STAGE2=1`) on the **ACBOOT** FAT32 partition, with a
    `boot=` line that loads the kernel + DTB + initramfs directly from ACBOOT.
  - Proven: cold boot → stage1 → stage2 → `boot=` → **"Booting Linux" → ALSA
    initializes** → then the panic below.

## The wall ❌
```
[0.000000] Booting Linux ... aurora 7.1.12
[0.044944] Advanced Linux Sound Architecture Driver Initialized.
[0.225252] SError Interrupt on CPU0, code 0x00000000be000000
[0.225]    Kernel panic ... pm_genpd_init ... apple_pmgr_ps_probe
```
- Uncategorized external abort (gated/protected register) while registering a
  power domain in `drivers/pmdomain/apple/pmgr-pwrstate.c`.
- **Proven NOT** a boot-method artifact (identical in proxy RAM-boot and clean
  native two-stage boot), **NOT** inherited from m1n1 (DISR drained clean),
  **NOT** dodgeable by disabling DTB nodes (that breaks early boot).
- **Leading cause: macOS 27.0.1 firmware.** aurora/omarchy qualify the Neo on
  macOS **26.x** (restore image `UniversalMac_26.6`; the omarchy installer warns
  against macOS 27). The firmware leaves a power domain in a state the
  macOS-26-era kernel faults on.

## Infrastructure built (all reusable)
- **Boot chain + fast dev loop**: ACBOOT = `disk0s3` FAT32, PARTUUID
  `666d9a8f-6720-4f5c-915a-7b77e5d377c4`, laid out as
  `aurora/stage2.bin` + `aurora/kernel/{Image,t8140-j700.dtb,initramfs.cpio.gz}`.
  **Updating stage2/kernel on ACBOOT needs NO re-bless** (only re-blessing stage1
  needs 1TR). m1n1 source + `fill_stage1_config.py` on blueberry
  `~/asahi/aurora-m1n1`; built stage1/stage2 in `/tmp/s1`, `/tmp/s2`.
- **Kernel dev env on poorslice** (M1 Pro, 16GB, arm64-macOS = aurora's exact
  platform): clang/lld **22.1.8** (`brew llvm@22 lld@22`), rustc **1.89.0**,
  bindgen **0.71.1**, gmake 4.4.1, case-sensitive volume `/Volumes/kernelsrc`
  with the Neo kernel source (`iconidentify/aurora-linux` branch
  `integrate/neo-complete-release-20261010`). **Native arm64-macOS clears the
  Rust `iosys_map` binding wall** that x86_64-Linux cross-compile could not.
  Remaining kernel-build blocker = macOS host-tool C conflicts (elf.h/byteswap/
  endian shimmed into `/tmp/elfinc`; stuck on `uuid_t` vs the macOS SDK) — needs
  aurora's host-port patches.
- **macOS 26.6.2 IPSW downloaded** (route A, not used): poorslice
  `~/neo-ipsw/UniversalMac_26.6.2_25G83_Restore.ipsw` (19,772,231,540 bytes).
- `macvdmtool` supports `dfu`, `reboot debugusb`, etc. (passwordless on blueberry
  via `/etc/sudoers.d/macvdmtool`).

## Frisbee's current state
- Linux volume cold-boots the two-stage chain → pmgr panic (expected).
- **macOS is intact** — boot-pick "Macintosh HD" to use it normally. Not wiped.
- `frisbee.local` on the LAN (Bonjour); tailnet "active" status can lie.

## Resume paths (when ready)
**→ Chosen: Route B (#3 below)** — in-place power-domain patch, no wipe. Rationale:
the kernel reaches ALSA init (0.045s) and dies at 0.225s on a *single* power-domain
register access — a bounded problem, fixable with tools that already work (m1n1
build + DTB edits + the two-stage loop), without finishing the stuck kernel build
or wiping Frisbee. Start point: patch m1n1 to probe/report (or power-on) each
power domain before handoff to identify the faulting one, OR bisect by marking
domain groups `apple,always-on` in `ACBOOT/aurora/kernel/t8140-j700.dtb`.

1. **Wait for aurora to qualify macOS 27** (most likely real fix). When a working
   kernel exists, just drop its `Image` + matching `t8140-j700.dtb` on
   `ACBOOT/aurora/kernel/` and cold-boot — **no re-bless, no wipe.** Trivial retry.
2. **Firmware downgrade to macOS 26.6.2** (route A, destructive): DFU-restore via
   Apple Configurator/`cfgutil` on poorslice (IPSW already downloaded). Gated on
   Apple still signing 26.6.2. Wipes Frisbee; rebuild the boot chain after.
3. **Patch the faulting power domain** (route B, no wipe): diagnose which domain
   faults in `apple_pmgr_ps_probe`, mark it `apple,always-on` in the DTB or
   power it on in m1n1 pre-handoff; iterate via the two-stage loop. Fiddly; may
   be whack-a-mole if macOS 27 changed several domains. For a real upstream
   kernel patch, finish the poorslice kernel build (needs aurora's host-port
   shims).

## Key reference
Full technical detail + gotchas: `memory/frisbee-acos-mtp-keyboard.md`.
Aurora install recipe: `~/asahi/aurora-m1n1/docs/j700-install.md`.
