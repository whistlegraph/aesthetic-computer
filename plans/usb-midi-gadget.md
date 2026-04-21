# USB MIDI Gadget for ac-native → Ableton direct

> Status: **plan / unscoped**. Lets the ac-native ThinkPad present itself
> to the MacBook (running Ableton) as a USB MIDI controller, bypassing the
> session-server WebSocket relay entirely for the lowest possible
> ThinkPad→Ableton latency.

## Motivation

Current path: `notepat.mjs (ThinkPad) → UDP → session-server.aesthetic.computer:10010 → WS fanout → notepat-remote.amxd (Mac) → Operator`. That's a round trip over public internet. Observed latency is fine for async use but **40-100ms** is unavoidable even on good wifi.

Direct USB MIDI gets us:

- **~1-3ms** ThinkPad key → MacBook MIDI-in (USB controller polling + kernel hop)
- No internet dependency, no session-server required
- No WS subscriptions, no handle/machineId negotiation
- Ableton sees it as a native MIDI controller — appears in MIDI input list, can be mapped, recorded, etc.

The session-server relay can stay as a **networked** backup / for multi-ThinkPad scenarios (one ThinkPad playing into multiple remote DAWs), but the USB path becomes the default for a local setup.

## Architecture

```
┌───────────────────────────┐    USB-C    ┌──────────────────────┐
│  ThinkPad (ac-native OS)  │ ═════════▶ │  MacBook (Ableton)   │
│  Linux g_midi gadget      │            │  CoreMIDI auto-mount │
│  /dev/snd/midiCxDy        │            │  "Linux USB MIDI"    │
│  ↑ notepat.mjs writes     │            │  → MIDI input list   │
│    raw MIDI bytes         │            │  → Ableton track     │
└───────────────────────────┘            └──────────────────────┘
```

`notepat.mjs` on ac-native gains a new output sink alongside the existing UDP→session-server path:

```
keypress → playSoundKey → [ UDP relay | USB MIDI | USB audio synth | ... ]
```

Configurable at runtime via `/mnt/config.json` (new key `usbMidiGadget: true`). Both paths can be on simultaneously — it's fire-and-forget MIDI data, the ingest endpoints are independent.

## Hardware prerequisites

### ThinkPad 11e Yoga Gen 6

The USB-C port must support **device/OTG mode** in its controller. Most 8th-gen Intel and newer support this through the Thunderbolt PHY, but it needs to be enabled in:

1. **BIOS/UEFI**: look for "USB-C device mode", "Thunderbolt configuration", or similar. On Lenovo ThinkPads this is usually under "Config → USB" or "Thunderbolt(TM) 3".
2. **Kernel**: `CONFIG_USB_GADGET=y`, `CONFIG_USB_GADGETFS=y`, `CONFIG_USB_CONFIGFS=y`, `CONFIG_USB_CONFIGFS_F_MIDI=y`. Verify with `zcat /proc/config.gz | grep GADGET` on the running kernel.
3. **UDC driver**: the "USB Device Controller" needs a loaded driver. On Intel platforms this is often `dwc3` or `xhci` with gadget support. Check `ls /sys/class/udc/` — there should be at least one entry when device mode is active.

**If the ThinkPad's USB-C is host-only**, the fallback is a USB device such as a **Raspberry Pi Zero W / 2 W** or **BeagleBone** tethered to the ThinkPad via network/serial, acting as the gadget. Out of scope for this plan.

### MacBook

No setup required. macOS auto-detects USB MIDI devices on plug-in and exposes them through CoreMIDI. They show up in:

- Audio MIDI Setup → Window → Show MIDI Studio
- Ableton → Preferences → Link Tempo MIDI → MIDI Ports → check "Track" for the gadget

### Cable

USB-C to USB-C (or USB-C to USB-A with correct gadget direction). Thunderbolt cables work but are overkill — a standard USB 2.0 data cable is plenty for MIDI (31.25 kbps).

## Linux gadget setup (ac-native)

### Option A — legacy `g_midi` driver (simplest)

```bash
modprobe g_midi \
    iProduct="AC Notepat" \
    iManufacturer="Aesthetic Computer" \
    id="AC_NOTEPAT"
```

Creates `/dev/snd/midiC<N>D0` and shows up on the Mac as "AC Notepat".

### Option B — configfs composite gadget (more control)

Scripted at boot time so it survives reboots and can be composed with
other gadget functions later (e.g. serial console).

```bash
#!/bin/sh
# /usr/local/bin/ac-usb-gadget-start

set -e
GADGET=/sys/kernel/config/usb_gadget/ac_notepat
mkdir -p "$GADGET"

echo 0x1d6b > "$GADGET/idVendor"   # Linux Foundation
echo 0x0104 > "$GADGET/idProduct"  # Multifunction Composite Gadget
echo 0x0100 > "$GADGET/bcdDevice"
echo 0x0200 > "$GADGET/bcdUSB"

mkdir -p "$GADGET/strings/0x409"
echo "ACNP-$(cat /etc/machine-id | cut -c1-8)" > "$GADGET/strings/0x409/serialnumber"
echo "Aesthetic Computer" > "$GADGET/strings/0x409/manufacturer"
echo "AC Notepat"         > "$GADGET/strings/0x409/product"

mkdir -p "$GADGET/configs/c.1/strings/0x409"
echo "MIDI config" > "$GADGET/configs/c.1/strings/0x409/configuration"
echo 250           > "$GADGET/configs/c.1/MaxPower"

mkdir -p "$GADGET/functions/midi.usb0"
echo 1  > "$GADGET/functions/midi.usb0/in_ports"
echo 1  > "$GADGET/functions/midi.usb0/out_ports"
echo 64 > "$GADGET/functions/midi.usb0/buflen"   # small buffer = low latency
echo 32 > "$GADGET/functions/midi.usb0/qlen"

ln -s "$GADGET/functions/midi.usb0" "$GADGET/configs/c.1/"

# Bind to the first available UDC. ls /sys/class/udc shows the devices.
UDC=$(ls /sys/class/udc | head -n1)
echo "$UDC" > "$GADGET/UDC"
```

Run at boot via an ac-native init hook. A matching teardown script writes empty to `UDC` and `rmdir`s the tree.

### Verifying on the Mac

With the cable plugged in:

```bash
system_profiler SPUSBDataType | grep -A 4 "AC Notepat"
# Should list it as a USB device.

# In Audio MIDI Setup app: "AC Notepat" appears with a MIDI icon.
```

In Ableton → Preferences → Link Tempo MIDI → MIDI Ports, the new input appears. Enable "Track" to make it a note source, or "Remote" to map its controls.

## ac-native integration

### New code in `fedac/native/src/`

`usb-midi.c` (new file):

```c
int usb_midi_open(const char *device);       // opens /dev/snd/midiC0D0
void usb_midi_close(int fd);
int usb_midi_send_note_on(int fd, int channel, int pitch, int velocity);
int usb_midi_send_note_off(int fd, int channel, int pitch);
```

Uses raw ALSA MIDI bytes:

- note-on:  `0x90 | channel, pitch, velocity`  (3 bytes)
- note-off: `0x80 | channel, pitch, 0`

`write(fd, buf, 3)` on the MIDI char device. Non-blocking mode preferred so a stalled receiver doesn't wedge the audio thread.

### JS bindings (`fedac/native/src/js-bindings.c`)

Mirror the existing `system.udp.sendMidi` helpers:

```js
system.usbGadget.open()                        // → bool
system.usbGadget.sendMidi(event, note, vel, ch)
system.usbGadget.close()
system.usbGadget.status                         // { connected, device, bytesSent }
```

### notepat.mjs wiring

At the existing `sendUdpMidiEvent(…)` call site (around `fedac/native/pieces/notepat.mjs:1847-1850`), also emit USB MIDI when enabled:

```js
const usbMidiGadgetEnabled =
  cfg.usbMidiGadget === true || cfg.usbMidiGadget === "true";

function sendMidiEvent(system, event, midiNote, velocity, channel = 0) {
  // Existing: UDP relay
  if (udpMidiBroadcast && system?.udp?.connected) {
    system.udp.sendMidi(event, midiNote, velocity, channel, "notepat");
  }
  // NEW: USB gadget direct to Mac
  if (usbMidiGadgetEnabled && system?.usbGadget?.status?.connected) {
    system.usbGadget.sendMidi(event, midiNote, velocity, channel);
  }
  // Existing telemetry counters
  udpMidiSentCount += 1;
  …
}
```

### Prompt command

Add `usb midi on/off/status` to `prompt.mjs`, parallel to the existing `midi relay on/off` command. Persists the flag to `/mnt/config.json`.

## Expected latency budget

| Stage | Cost |
|---|---|
| ThinkPad key press → notepat handler | <1 ms |
| JS → C `sendMidi()` binding | <0.1 ms |
| `write()` → kernel USB gadget | <0.2 ms |
| USB bus traversal (2× full-speed frame ≈ 1 ms polling interval) | ~1-2 ms |
| MacBook USB host → CoreMIDI callback | <0.5 ms |
| Ableton MIDI-in → track → instrument | <1 ms |
| **Audio buffer output** (Live at 64 samples / 48 kHz) | ~1.3 ms |
| **Total keypress → audible** | **~5-7 ms** |

Versus current session-server path (~40-100 ms), that's **~10-15× faster**.

## Out of scope / follow-ups

- **Bi-directional MIDI**: this plan is one-way ThinkPad→Mac. Receiving MIDI from Ableton back to ac-native (e.g., for playback-synced visuals) needs a `read()` loop on the same gadget endpoint. Easy to add.
- **MIDI clock sync**: ac-native can emit `0xF8` every 24 PPQ to sync Ableton's transport.
- **Multi-channel**: currently all notes fire on channel 0. Exposing channel selection in notepat's UI is trivial once the gadget is up.
- **Power**: the ThinkPad draws from its own battery; USB in device mode doesn't supply power to the Mac. Bus power is one-directional.
- **SysEx / MPE**: raw `write()` handles arbitrary MIDI bytes — both just work.

## Test plan

1. On the ThinkPad: `dmesg | grep gadget` after `g_midi` modprobe — expect "using random self ethernet address" + MIDI gadget init lines.
2. `aconnect -l` lists the gadget port.
3. `amidi -l` shows the device with its hw:N,M address.
4. `amidi -p hw:N,M -S '90 3C 7F'` plays a C4 note-on — verify by ear through Ableton after enabling the MIDI input.
5. Wire into `notepat.mjs`, rebuild ac-native, flash to ThinkPad.
6. Plug into Mac. Enable in Live's MIDI preferences.
7. Press keys in ac-native notepat — notes should fire instantly on the Mac.
8. A/B against the session-server relay path (`midi relay on`) to confirm the latency improvement.

## Why not this?

- If the ThinkPad's USB-C can't do device mode (BIOS or silicon limitation), fall back to a USB-serial adapter or a Pi Zero bridge.
- If you're primarily using ac-native remotely (not physically next to the Mac), keep the session-server path — USB obviously requires a cable.
- The M4L `notepat-remote.amxd` device is still useful either way: on USB MIDI the AC-native-sourced notes land on *any* MIDI track directly, no device needed; on network relay the device is required to subscribe + route.
