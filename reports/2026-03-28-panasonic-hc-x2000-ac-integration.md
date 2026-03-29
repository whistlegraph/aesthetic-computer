# Panasonic HC-X2000 Integration Report (2026-03-28)

## Summary

The Panasonic HC-X2000 is already a viable source device for Aesthetic Computer, but it does not map cleanly onto the stack as a pure browser-native camera today.

Right now, the strongest integration paths are:

1. **Wired ingest with remote Wi-Fi control**
   - Use HDMI or SDI into a capture card.
   - Let AC ingest that feed as a local camera/video source.
   - Use Panasonic's Wi-Fi tools only for framing, transport, and remote control.

2. **Wi-Fi stream ingest with a bridge**
   - Have the HC-X2000 stream over RTMP or RTSP on the local network.
   - Run a bridge service that converts that stream into something AC can use directly, such as browser playback, WebRTC, or a local virtual camera.

3. **Deferred media flow**
   - Record on the camera.
   - Move clips into AC's tape/video/post pipeline after capture.
   - This is the least live, but the lowest-risk option if the goal is publishing rather than realtime performance.

## What the HC-X2000 gives us

### Network and streaming

According to Panasonic's current HC-X2000 specifications, the camera supports:

- Wi-Fi: `IEEE 802.11 b/g/n`
- Band: `2.4 GHz`
- Live streaming resolutions:
  - `1920x1080`
  - `1280x720`
  - `640x360`
  - `320x180`
- Streaming methods:
  - `Unicast`
  - `Multicast`
- Network protocols:
  - `RTSP`
  - `RTP`
  - `RTMP`
  - `RTMPS`
- Audio for streaming:
  - `AAC-LC`
  - `48 kHz / 16-bit / 2ch`
- Streaming bitrates:
  - `0.5 Mbps` through `24 Mbps`

This means the camera can already behave like a small network encoder and does not need a computer to originate a live stream.

### Remote control

Panasonic documents HC-ROP support for the HC-X2000. Their current support flow shows:

- the camera can join infrastructure Wi-Fi,
- `IP Remote` can be enabled,
- a user account can be configured,
- the HC-ROP app can then control the camcorder by IP.

Panasonic also documents a firmware milestone:

- **Ver. 1.6**, dated **February 26, 2025**
- adds **HC-ROP Live View** support on the app screen

That matters because it makes the camera more useful as a remotely operated node in an AC performance or installation setup.

### Physical outputs and I/O

The HC-X2000 also exposes stronger-than-Wi-Fi integration paths:

- `3G/1.5G HD-SDI` out
- `HDMI` out
- `XLR` audio inputs x2
- built-in stereo mic
- USB LAN adapter support

For AC specifically, SDI and HDMI are the shortest route to dependable ingest.

## What AC already has today

### Browser camera capture

AC already has a browser camera path based on `getUserMedia`, including camera enumeration and front/back camera handling in:

- [bios.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs#L19500)
- [camera.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/camera.mjs#L45)

This is a good fit for:

- webcams,
- phones used as webcams,
- HDMI/SDI capture cards that appear as standard video devices.

It is **not yet** a network-camera ingest system.

### Tape and video pipeline

AC already has a mature enough client-side recording/export/post flow:

- `MediaRecorder`-based export in [bios.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs#L14692)
- video/tape posting from [video.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/video.mjs#L1240)
- planning docs around camera+tape A/V work in [tape-camera-av-feature.md](/workspaces/aesthetic-computer/plans/tape-camera-av-feature.md)

This means AC can already:

- capture browser-visible media,
- export video,
- post tape artifacts,
- and support microphone/audio workflows.

But the current system is still oriented around:

- canvas capture,
- local browser camera access,
- and client-side export,

not direct RTSP/RTMP camera ingest.

## Best integration paths for AC right now

### Option 1: HDMI or SDI capture into AC

This is the best immediate path.

Flow:

1. HC-X2000 outputs over HDMI or SDI.
2. Capture hardware presents the signal to the host as a standard camera device.
3. AC uses its existing browser camera path.
4. AC tape/video/export/post features stay mostly unchanged.

Why this is the best fit:

- It works with AC's current browser capture model.
- It avoids building a network ingest layer first.
- It keeps latency and debugging simpler.
- It preserves the camera's optics, audio options, and framing advantages.

### Option 2: RTMP/RTSP ingest bridge

This is the best Wi-Fi-native path, but it needs new glue.

Flow:

1. HC-X2000 streams RTMP or RTSP over LAN.
2. A bridge service receives the stream.
3. The bridge republishes it as one of:
   - a browser-playable video element,
   - WebRTC,
   - HLS for monitoring,
   - or a local virtual webcam for AC.
4. AC records or remixes that feed.

What would need to be added:

- a receiver endpoint or local ingest service,
- stream lifecycle controls,
- latency/error handling,
- and likely a new disk or service wrapper for network cameras.

### Option 3: Remote control plus deferred import

This is useful if the actual goal is making media for AC rather than using the camera live.

Flow:

1. Use HC-ROP over Wi-Fi for remote camera operation.
2. Record directly in-camera.
3. Import the resulting clips into AC's publishing or processing pipeline.

This avoids live ingest complexity but gives up the immediacy of a live AC piece using the feed directly.

## Recommendation

If the goal is to integrate the HC-X2000 with Aesthetic Computer **right now**, the recommendation is:

1. **Primary path:** use **HDMI or SDI capture** as the media path.
2. **Secondary path:** use **Wi-Fi + HC-ROP** for remote control and monitoring.
3. **Future path:** add an **RTMP/RTSP ingest bridge** if AC should support dedicated network cameras as first-class devices.

In short:

- the camera is capable enough,
- AC is close enough,
- but the missing piece is not on the Panasonic side,
- it is on the AC side: a dedicated network video ingest layer.

## Suggested follow-up work in AC

### Small scope

- Add a report-backed note or plan for `network camera ingest`.
- Prototype an `ffmpeg` receiver for HC-X2000 RTMP or RTSP.
- Test whether a bridge can expose the feed back to the browser with acceptable latency.

### Medium scope

- Add a new disk such as `netcam` or `streamcam`.
- Support selecting:
  - local webcam,
  - capture card,
  - RTMP source,
  - RTSP source.

### Larger scope

- Unify camera, tape, and video around a single A/V source abstraction.
- Treat:
  - browser webcams,
  - capture cards,
  - network cameras,
  - prerecorded clips
  as interchangeable sources.

## Sources

Panasonic official sources:

- Panasonic HC-X2000 features/specifications:
  - https://help.na.panasonic.com/answers/features-and-specifications-camcorder-model-hc-x1500-model-hc-x2000/
- Panasonic basic owner's manual:
  - https://help.na.panasonic.com/wp-content/uploads/2023/02/HCX1500_X2000_DVQX2023ZA_ENG.pdf
- Panasonic HC-X2000 firmware updates:
  - https://help.na.panasonic.com/answers/hc-x2000-firmware-updates-improvements
- Panasonic HC-ROP connection instructions:
  - https://help.na.panasonic.com/answers/how-to-connect-to-the-hc-rop-app-hc-x1500-hc-x2000/
- Panasonic direct streaming setup:
  - https://help.na.panasonic.com/answers/setting-up-direct-streaming-hc-x1500-hc-x2000-ag-cx10/

Relevant AC repo references:

- [bios.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs#L19500)
- [camera.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/camera.mjs#L45)
- [bios.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs#L14692)
- [video.mjs](/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/video.mjs#L1240)
- [tape-camera-av-feature.md](/workspaces/aesthetic-computer/plans/tape-camera-av-feature.md)
