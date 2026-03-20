# AC Native OS Boot Speed Optimization Report

Analysis of the full boot path from kernel handoff to running JS piece, with concrete proposals for reducing the ~9-second typical boot time.

Source files analyzed:
- `fedac/native/initramfs/init`
- `fedac/native/src/ac-native.c` (main boot sequence, `try_mount_log`, `draw_startup_fade`, `draw_boot_status`, `load_boot_visual_config`)
- `fedac/native/src/audio.c` (`audio_init`)
- `fedac/native/src/wifi.c` (`wifi_init`)
- `fedac/native/src/tts.c` (`tts_init`, `tts_precache`)
- `fedac/native/src/input.c` (`input_init`)
- `fedac/native/kernel/config-minimal`

---

## 1. Current Boot Timeline

Typical boot from kernel to piece running: **~9 seconds** (when audio hardware cooperates). Measured by the `boot_start`/`boot_end` monotonic clock in `main()` (ac-native.c:1774, 2101-2105).

| Phase | Location | Duration | Blocking? | Notes |
|---|---|---|---|---|
| Kernel decompress + hw init | kernel/BIOS | ~1-2s | Yes | LZ4 initramfs, built-in driver probe |
| Init script | `initramfs/init` | ~1-2s | Yes | mounts, zram, dmesg dump (1s sleep) |
| PID 1 setup + DRI wait | ac-native.c:1778-1802 | 0-1s | Yes | Polls `/dev/dri/card0` every 10ms for 1s |
| Display init (DRM) | ac-native.c:1860 | <100ms | Yes | Opens card, finds CRTC, creates dumb buffers |
| Framebuffer + font init | ac-native.c:1883-1888 | <10ms | Yes | Software render context |
| **USB mount wait** | ac-native.c:266-273 | **0-2s** | **Yes** | Polls `/dev/sda1` every 20ms for 2s |
| Config load | ac-native.c:542-620 | <1ms | Yes | File read + JSON parse |
| **Audio init** | audio.c:616-928 | **0-18s** | **Yes** | Sound card poll + ALSA retries (see below) |
| TTS init | tts.c:125-158 | ~50ms | Yes | Flite voice registration |
| **TTS precache** | tts.c:204-222 | **~500ms-1s** | **Yes** | 41 Flite renders (a-z, 0-9, 5 keys) |
| Boot beep | audio.c:1407-1413 | ~200ms | Yes | Two-tone E5/B5 with 80ms gap |
| **Boot animation** | ac-native.c:1288-1700 | **0-5s** | **Skippable** | 300 frames at 60fps, key-skippable after 1s |
| Input init | input.c:315+ | <100ms | Yes | evdev scan + hidraw |
| **WiFi init** | wifi.c:653-758 | **0-3s** | **Yes** | Interface poll every 100ms for 3s |
| WiFi autoconnect | wifi.c (after init) | ~async | Partly | Starts connection, returns |
| JS init | js-bindings.c:1637+ | ~100-200ms | Yes | QuickJS context + API registration |
| Config re-read + piece resolve | ac-native.c:2016-2077 | <10ms | Yes | |
| Piece load + boot() | ac-native.c:2080-2144 | ~100ms | Yes | Module eval + boot() call |
| TTS wait + melody | ac-native.c:2128-2134 | **~700ms** | **Yes** | 300ms TTS drain + 400ms melody ring-out |

### Audio Init Breakdown (the big variable)

Audio initialization at `audio.c:616-928` has two stacked wait loops:

**Loop 1 -- Sound card detection** (audio.c:665-670):
- Polls for `/dev/snd/pcmC{0,1,2}D0p` every 20ms
- 400 iterations = **8 seconds max**
- On fast boots with working HDA: exits in <500ms
- On slow codec probe: hits full 8s timeout

**Loop 2 -- ALSA device open** (audio.c:722-743):
- 5 retry attempts, each trying 11 device names
- 2-second `usleep()` between retries
- **10 seconds additional worst case** (5 retries x 2s)

**Worst case total: 8s + 10s = 18 seconds for audio alone.**

Typical case (hardware ready quickly): <1s.

---

## 2. Polling vs. Event-Driven Detection

The current code uses busy-polling for all hardware detection. Every poll loop wastes CPU cycles spinning on `usleep()` and `access()` calls, and more importantly, it cannot react faster than its poll interval.

### 2.1 Current Polling Inventory

| Subsystem | File | What it polls | Interval | Max wait | Location |
|---|---|---|---|---|---|
| Sound card | audio.c | `/dev/snd/pcmC*D0p` | 20ms | 8s | Line 665-670 |
| USB block device | ac-native.c | `/dev/sda1`, `/dev/sdb1` | 20ms | 2s | Line 270-273 |
| WiFi interface | wifi.c | `detect_iface()` via sysfs | 100ms | 3s | Line 736-743 |
| DRI device | ac-native.c | `/dev/dri/card0`, `/dev/fb0` | 10ms | 1s | Line 246 (PID 1 path) |

### 2.2 Proposed Alternatives

**inotify on `/dev/snd/`** (for audio card detection):
- Create an `inotify` watch on `/dev/snd/` with `IN_CREATE` mask.
- Block on `read()` with a timeout (via `poll()` or `select()` with 8s timeout).
- When a `pcmC*D0p` node appears, immediately break out.
- Reacts within milliseconds of device node creation instead of up to 20ms poll granularity.
- Implementation: ~30 lines of C replacing the 6-line poll loop.

```c
// Pseudocode
int ifd = inotify_init1(IN_NONBLOCK);
inotify_add_watch(ifd, "/dev/snd", IN_CREATE);
struct pollfd pfd = { .fd = ifd, .events = POLLIN };
while (!card_found) {
    // Check if already present
    if (access("/dev/snd/pcmC0D0p", F_OK) == 0) { card_found = 1; break; }
    int ret = poll(&pfd, 1, 8000); // 8s timeout
    if (ret > 0) { /* read events, check name */ }
    else break; // timeout
}
close(ifd);
```

**inotify on `/dev/` for USB block devices**:
- Same pattern as above, watching `/dev/` for `IN_CREATE` events matching `sd[a-d]1`.
- Replaces the 2s poll in `try_mount_log()`.

**Netlink socket for WiFi interface detection**:
- Open a `NETLINK_ROUTE` socket, subscribe to `RTNLGRP_LINK` group.
- Listen for `RTM_NEWLINK` messages with `ifi_flags` containing `IFF_UP`.
- Eliminates the 3s poll loop in `wifi_init()` and reacts to interface creation instantly.
- More complex (~80 lines) but the canonical Linux approach. Already used by NetworkManager, iwd, etc.

**Why not udev?**
- AC Native OS runs no udevd (no service manager at all). Starting udevd would add its own startup cost.
- A minimal `libudev` monitor is possible without udevd (it reads the same netlink/inotify events), but adds a library dependency.
- The inotify/netlink approaches above are lower-level but dependency-free, which matches the system's philosophy.

---

## 3. Parallelization Opportunities

Currently, every boot phase in `main()` is fully sequential. The execution order is:

```
try_mount_log() -> load_boot_visual_config() -> audio_init() -> tts_init() ->
tts_precache() -> boot_beep -> draw_startup_fade() [5s animation] ->
input_init() -> wifi_init() -> wifi_autoconnect() -> js_init() -> piece load
```

Several of these have no data dependencies on each other.

### 3.1 Dependency Graph

```
                    display_init
                         |
                    framebuffer + font
                         |
              +----------+----------+
              |          |          |
        try_mount_log  (parallel)  (parallel)
              |
        load_config
              |
    +---------+---------+
    |         |         |
audio_init  wifi_init  input_init    <-- all independent of each other
    |
 tts_init
    |
 tts_precache
    |
boot_animation  <-- needs: display, audio (for beep), tts (for greeting)
    |
 js_init        <-- needs: graph, input, audio, wifi, tts
    |
 piece_load
```

### 3.2 Concrete Parallelization Plan

**Phase A: Start audio + wifi + input concurrently**

These three subsystems have zero shared state during init:
- `audio_init()` touches ALSA devices only
- `wifi_init()` touches wireless sysfs/netlink only
- `input_init()` touches evdev/hidraw only

Spawn threads for audio and wifi, run input on the main thread (it is fast):

```c
pthread_t audio_thread, wifi_thread;
pthread_create(&audio_thread, NULL, audio_init_threaded, NULL);
if (!wifi_disabled) pthread_create(&wifi_thread, NULL, wifi_init_threaded, NULL);
input = input_init(display->width, display->height, pixel_scale);
pthread_join(&audio_thread, (void**)&audio);
if (!wifi_disabled) pthread_join(&wifi_thread, (void**)&wifi);
```

**Estimated savings: 2-3 seconds typical** (wifi 3s wait runs during audio 8s wait instead of after).

**Phase B: Overlap boot animation with hardware waits**

The boot animation (`draw_startup_fade`, 5 seconds) is currently run *after* audio/TTS init. But the animation only needs:
- Display (already initialized)
- Audio (for boot beep at start, TTS greeting at frame 10)

Strategy: start the animation immediately after display init. Audio can still be initializing in a background thread. The boot beep and TTS greeting are delayed until audio is ready (a simple flag check per frame).

```c
// Start audio init in background
pthread_create(&audio_thread, NULL, audio_init_threaded, NULL);
// Start animation immediately
// Inside animation loop: if (audio_ready && !beep_played) { audio_boot_beep(); beep_played=1; }
```

**Estimated savings: up to 5 seconds** -- the animation runs *during* audio init instead of after, and the 5s animation cost is fully overlapped.

**Phase C: Defer TTS precache to after piece starts**

`tts_precache()` renders 41 utterances (~500ms-1s). This only matters when the user starts typing, which happens well after boot. Defer it to a background thread launched after `js_call_boot()`.

```c
js_call_boot(rt);
// Precache TTS in background -- user won't type for several seconds
pthread_create(&tts_precache_thread, NULL, (void*)tts_precache, tts);
```

**Estimated savings: ~500ms-1s.**

**Phase D: Move JS init earlier**

`js_init()` has no dependency on wifi or input -- it only needs the `ACGraph` pointer (and NULL is acceptable for input/audio/wifi, which can be set after). The QuickJS runtime creation and API registration could be done in parallel with hardware waits.

However, `js_init()` currently takes the pointers at creation time. Refactoring to allow late-binding of subsystem pointers would require changes to `js-bindings.c`. Lower priority.

---

## 4. Specific Optimizations

### Optimization 1: Replace audio polling with inotify

**Current cost:** Up to 8s polling at 20ms intervals (400 iterations).
**Proposed:** inotify watch on `/dev/snd/` with `IN_CREATE`, blocking `poll()` with 8s timeout.
**Expected savings:** Reacts within ~1ms of device node creation instead of up to 20ms. On fast boots where the card appears at 200ms, saves no time. On slow boots where it appears at 7.5s, saves ~20ms max. The real win is cleaner code and less CPU wake-up churn.
**Risk:** Low. inotify is a stable Linux API (since 2.6.13). Fallback: if inotify_init fails, fall back to current poll loop.
**Estimated time savings:** 0-20ms typical (marginal), but enables Phase B parallelization cleanly.

### Optimization 2: Smarter ALSA retry strategy

**Current cost:** 5 retries x 2s sleep = up to 10s additional on top of 8s card wait.
**Problem:** The 2s sleep between retries is a fixed delay. If the codec probes 100ms after the last attempt, we waste 1.9s.

**Proposed approach -- inotify + exponential backoff:**
1. After the card wait loop finds a PCM device node, do NOT sleep-retry on `snd_pcm_open()` failure.
2. Instead, use inotify on `/dev/snd/` to detect NEW device nodes appearing (codec re-probe creates additional nodes).
3. When a new node appears, immediately retry all devices.
4. If no new nodes appear within 500ms, try again with exponential backoff: 200ms, 400ms, 800ms, 1600ms (4 retries, 3s total instead of 10s).

**Alternative -- probe via sysfs:**
Before trying `snd_pcm_open()`, check `/proc/asound/card0/codec#0` for a `Codec:` line. If present, the codec is probed. If not, wait for it via inotify on `/proc/asound/card0/`.

**Estimated time savings:** 0-7s on pathological hardware. Reduces worst-case audio init from 18s to ~11s (8s card wait + 3s smarter retry).
**Risk:** Medium. ALSA timing edge cases vary by hardware. Must keep the fallback timeout.

### Optimization 3: Parallelize audio + wifi + boot animation

**Current cost:** Sequential: audio (0-18s) + animation (5s) + wifi (3s) = up to 26s serial.
**Proposed:** Run audio_init and wifi_init in background threads while the boot animation plays on the main thread. See Section 3.2, Phases A and B.

**Estimated time savings:** 3-8s typical.
- WiFi 3s overlapped with audio: saves 3s.
- Animation 5s overlapped with audio: saves up to 5s.
- Total overlap with audio: audio takes 0-18s, animation+wifi take 5-8s. If audio finishes in <5s (typical), the animation is the gating factor. If audio takes >5s (rare), audio is the gating factor but we saved the animation's 5s by running it concurrently.

**Risk:** Medium. `audio_init()` writes to `audio->pcm`, `audio->audio_status`, etc. The boot animation currently reads `audio` only for `audio_boot_beep()` and TTS. These calls would need to be guarded with an `audio_ready` flag or atomic. `wifi_init()` is already self-contained (returns an opaque struct) and safe to thread.

### Optimization 4: Defer TTS precache

**Current cost:** ~500ms-1s blocking on main thread before animation starts.
**Proposed:** Move `tts_precache()` to a background thread launched after `js_call_boot()`.
**Estimated time savings:** 500ms-1s.
**Risk:** Low. The only consumer of cached TTS is `tts_speak_cached()`, called on keypress. If a keypress arrives before precache finishes, the cache entry will be NULL and `tts_speak_cached()` already handles this (falls back to nothing). Could add a fallback to `tts_speak()` (live render) for uncached entries during the precache window.

### Optimization 5: Eliminate init script dmesg dump sleep

**Current cost:** 1 second (`sleep 1` at init:69).
**Problem:** The init script does `cat /dev/kmsg > /mnt/dmesg.log &` then `sleep 1; kill $PID`. This is a debugging aid that costs 1 second on every boot.
**Proposed:** Two options:
1. Remove entirely and rely on ac-native's USB log (which already captures kernel messages).
2. Replace with a bounded read: `timeout 0.2 cat /dev/kmsg > /mnt/dmesg.log 2>/dev/null` (busybox `timeout` if available, or just skip).
3. Move to background: let ac-native copy `/dev/kmsg` to USB asynchronously after boot completes.

**Estimated time savings:** 1s.
**Risk:** Low. Loss of kernel boot log on USB is acceptable since ac-native logs diagnostic info anyway.

### Optimization 6: Reduce boot animation to 3 seconds

**Current cost:** 300 frames at 60fps = 5 seconds.
**Observation:** The animation was designed for 3s (`180 frames` in the comment at line 1381) but was extended to 5s (`total_frames = 300` at line 1384) to accommodate TTS greeting. If TTS greeting runs in a background thread (it already does -- `tts_speak` is async), and we overlap the animation with hardware init (Optimization 3), the extra 2s are unnecessary padding.
**Proposed:** Reduce `total_frames` back to 180 (3 seconds). Or better: make the animation end when all hardware init threads join, with a 3s minimum.
**Estimated time savings:** 0-2s (only saves time when animation is the gating factor).
**Risk:** Low. TTS greeting may get cut short if it exceeds 3s, but the melody at the end serves as the "ready" signal anyway.

### Optimization 7: Kernel config -- skip unnecessary subsystem probes

**Current state:** All audio (HDA, SOF) and WiFi (iwlwifi) drivers are built-in (`=y`), not modules. This means they probe at kernel boot time, which is correct for fast startup -- no `modprobe` overhead.

**Potential wins:**
- `CONFIG_SND_HDA_CODEC_HDMI` is already disabled, good.
- Ensure only needed HDA codecs are enabled. Currently only `CONFIG_SND_HDA_CODEC_REALTEK=y` and `CONFIG_SND_HDA_GENERIC=y`. This is already well-tuned.
- Consider disabling `CONFIG_SND_PROC_FS=y` and `CONFIG_SND_VERBOSE_PROCFS=y` -- saves a few ms of procfs setup. But useful for debugging; probably not worth it.
- `CONFIG_SND_CTL_LED=y` can be disabled if no LED indicators are needed -- tiny savings.

**Estimated time savings:** <100ms. The kernel config is already lean for the target hardware.
**Risk:** Very low for cosmetic changes. Disabling needed codecs would break audio.

### Optimization 8: Initramfs compression -- uncompressed for NVMe

**Current state:** LZ4 compression (`CONFIG_INITRAMFS_COMPRESSION_LZ4=y`).
**Analysis:** LZ4 is already the fastest decompression option Linux supports. On NVMe (>3 GB/s read), decompression CPU cost may actually exceed the I/O savings from smaller size. However:
- The initramfs contains firmware blobs, the ac-native binary, Flite voice data, pieces, and shared libraries -- likely 50-150MB uncompressed.
- LZ4 decompresses at ~4 GB/s on modern CPUs, so a 100MB initramfs decompresses in ~25ms.
- Uncompressed would save those 25ms but increase read time from NVMe by ~20ms (assuming 2:1 compression ratio, 50MB extra at 3 GB/s).
- Net difference: essentially zero.

For USB boot (USB 3.0 at ~400 MB/s), LZ4 is clearly better -- the smaller size saves more I/O time than the decompression costs.

**Estimated time savings:** ~0ms (LZ4 is already optimal).
**Risk:** Switching to uncompressed increases kernel image size, which matters for EFI partition space and OTA upload time.

### Optimization 9: Eliminate post-boot delays

**Current cost:** After piece loads, there are explicit waits:
- `usleep(300000)` -- 300ms TTS ring buffer drain (ac-native.c:2131)
- `usleep(400000)` -- 400ms melody ring-out (ac-native.c:2134)

**Proposed:** The ring buffer drain and melody can continue playing after `js_call_boot()`. The audio thread is independent. Move `js_call_boot()` before the melody/drain wait:

```c
// Current:
tts_wait(); usleep(300000);
audio_ready_melody(); usleep(400000);
audio_prewarm();
js_call_boot();

// Proposed:
audio_prewarm();
js_call_boot();  // piece starts running immediately
tts_wait();      // TTS finishes in background (audio thread handles playback)
audio_ready_melody(); // plays over the already-running piece
```

**Estimated time savings:** 700ms.
**Risk:** Low. The piece's first `paint()` frame will render while the melody is still playing, which is fine -- it creates a seamless transition. The `audio_prewarm()` must happen before boot to avoid first-note latency, but it is nearly instant (50ms at 0.001 volume).

### Optimization 10: USB mount -- skip poll when block device already exists

**Current cost:** `try_mount_log()` always waits up to 2s, even when booted from NVMe where `/dev/sda1` may never appear.
**Proposed:** Check once. If no USB block device exists and we detect NVMe (`/dev/nvme0n1p1`), skip the 2s wait and go directly to NVMe mount.

```c
// Quick check -- is a block device already present?
int found_quick = (access("/dev/sda1", F_OK) == 0 || access("/dev/sdb1", F_OK) == 0);
if (!found_quick && access("/dev/nvme0n1p1", F_OK) == 0) {
    // NVMe boot, USB probably won't appear. Mount NVMe ESP directly.
    goto mount_phase;
}
// Only poll if nothing found and no NVMe fallback
for (int w = 0; w < 100; w++) { ... }
```

**Estimated time savings:** 0-2s (saves the full 2s on NVMe-installed boots where no USB is plugged in).
**Risk:** Low. If a USB drive is being slow to enumerate on an NVMe system, we miss it -- but we still mount NVMe ESP for config.json, which is the critical need.

---

## 5. Combined Impact Estimate

| Optimization | Time Saved (typical) | Time Saved (worst case) | Risk |
|---|---|---|---|
| 1. inotify for audio detection | ~0ms | ~20ms | Low |
| 2. Smarter ALSA retry | ~0ms | ~7s | Medium |
| 3. Parallel audio + wifi + animation | **3-5s** | **8s** | Medium |
| 4. Defer TTS precache | **500ms-1s** | **1s** | Low |
| 5. Eliminate init dmesg sleep | **1s** | **1s** | Low |
| 6. Shorter boot animation | **0-2s** | **2s** | Low |
| 7. Kernel config tuning | <100ms | <100ms | Very low |
| 8. Initramfs compression | ~0ms | ~0ms | N/A |
| 9. Eliminate post-boot delays | **700ms** | **700ms** | Low |
| 10. Skip USB poll on NVMe | **0-2s** | **2s** | Low |

### Projected Boot Times

**Current typical:** ~9s (kernel to piece running)

**After low-risk optimizations (4, 5, 6, 9, 10):**
- Savings: ~1s (init dmesg) + ~700ms (post-boot delays) + ~750ms (TTS precache) + ~1s (shorter animation, NVMe skip) = **~3.5s**
- **Projected: ~5.5s**

**After all optimizations including parallelization (1-10):**
- Audio, WiFi, and animation all run concurrently. The gating factor becomes `max(audio_init, 3s_animation)`.
- Typical audio init: <1s. Animation: 3s. WiFi: overlapped.
- **Projected: ~3-4s** (kernel to piece running)

**Worst-case (bad audio hardware):**
- Current: ~27s (18s audio + 5s animation + 3s wifi + 1s init)
- After: ~13s (11s audio with smarter retry, overlapped with everything else)

---

## 6. Implementation Priority

Recommended implementation order, balancing impact vs. effort:

1. **Eliminate init dmesg sleep** (5 min, saves 1s) -- change `sleep 1` to `sleep 0.1` or remove entirely
2. **Eliminate post-boot delays** (10 min, saves 700ms) -- reorder `js_call_boot()` before melody
3. **Defer TTS precache** (15 min, saves ~750ms) -- spawn background thread after boot
4. **Skip USB poll on NVMe** (20 min, saves 0-2s) -- quick-check before poll loop
5. **Shorten animation to 3s** (5 min, saves 0-2s) -- change `total_frames = 300` to `180`
6. **Parallel audio + wifi init** (2 hours, saves 3-5s) -- thread spawning + ready flags
7. **Overlap animation with audio init** (4 hours, saves additional 0-3s) -- requires animation to handle audio-not-ready state
8. **inotify for device detection** (2 hours, saves marginal) -- cleaner code, enables further async work
9. **Smarter ALSA retry** (3 hours, saves 0-7s worst case) -- inotify + exponential backoff

Items 1-5 are low-risk, high-value, and can be done independently in an afternoon. Items 6-9 require more careful testing across hardware variants.

---

## 7. Risk Assessment Summary

| Risk Level | Optimizations | Gotchas |
|---|---|---|
| **Very Low** | 7 (kernel config), 8 (compression) | Negligible impact; don't bother unless chasing <100ms |
| **Low** | 4 (defer TTS), 5 (init sleep), 6 (shorter anim), 9 (post-boot delays), 10 (NVMe skip) | TTS precache race: keypress before cache ready (already handled by NULL check). NVMe skip: misses slow USB enumeration (acceptable). |
| **Medium** | 1 (inotify audio), 2 (smarter retry), 3 (parallel init), overlap animation | Thread safety: `audio_init()` return value must be visible to main thread (use `pthread_join` or atomic flag). WiFi thread must not conflict with audio's sysfs reads. Boot animation must handle `audio == NULL` during early frames. ALSA retry timing: some codecs need the full 2s between probes; shorter retry could miss a working window. |
| **High** | Removing the 8s card wait entirely | Some hardware genuinely takes 5-7s for HDA codec probe. Removing the timeout would break those machines. Always keep a generous maximum. |

### Hardware-Specific Concerns

- **ThinkPad HDA codecs** (Realtek ALC257): Known to have variable probe times. The 5-retry ALSA loop was added specifically for machines where the codec appears in `/dev/snd/` but `snd_pcm_open()` fails for another 2-4 seconds.
- **SOF audio** (Intel Sound Open Firmware): Not currently targeted but present in firmware bundles. If future hardware needs SOF, its probe time is typically longer (3-5s for DSP firmware load).
- **USB audio**: If an external USB audio device is the primary output, the 8s wait for PCM nodes may be insufficient (USB enumeration + audio class driver). This is an edge case.
- **NVMe vs. USB boot**: NVMe boots are faster for everything except situations where config.json is on USB. The USB poll skip (Optimization 10) must not break USB-only boots.
