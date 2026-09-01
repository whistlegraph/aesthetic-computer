# toolchain/tv — the broadcast rig

Turns a web page into a 24/7 YouTube live stream. First station:
**tv.whistlegraph.org → the whistlegraph YouTube channel** ("the archive as
broadcast", completed — the site itself is what airs). Runs on jasellite.

## Pieces

- `rig.sh <env-file>` — the whole station in one process tree: Xvfb virtual
  screen + PulseAudio null sink + Chrome kiosk on the page + ffmpeg
  (x11grab + sink monitor → x264 → RTMP). ffmpeg is the foreground process,
  so process supervision of ffmpeg supervises the station.
- `wgtv.service` — systemd user unit (Restart=always) for the 24/7 phase.
- `../youtube/live.mjs` — YouTube Live API plumbing (persistent stream key,
  broadcasts, status) using the same vault OAuth as yt.mjs (`--as whistlegraph`).

## Setting up a station

1. Host needs: google-chrome, Xvfb, pulseaudio(+utils), xdotool, ffmpeg.
2. `node toolchain/youtube/live.mjs ensure-stream --title "Whistlegraph TV" \
     --as whistlegraph --env-out wgtv.env` → scp `wgtv.env` (0600) to the rig.
3. `node toolchain/youtube/live.mjs create-broadcast --title "…" \
     --stream <id> --as whistlegraph --privacy unlisted [--no-auto-stop]`
   — autoStart means it goes live when ffmpeg connects.
4. Start `rig.sh` (or install the unit) and watch the youtu.be URL.

## Hard-won details

- **HEVC**: the whistlegraph archive rips are HEVC; Chrome without a GPU
  can't decode it (black picture, working audio). tv.html detects HEVC-less
  viewers and serves `-avc.mp4` H.264 twins (`?avc` forces it). New curated
  works need their twin generated: `wgtv/avc-batch.sh` pattern on the rig —
  transcode HEVC / remux H.264, upload beside the original in Spaces.
- **The unmute tap**: tv.whistlegraph.org starts muted; first click enables
  sound, *every later click changes channel*. The page remembers sound-on in
  the profile, so rig.sh wipes its Chrome profile every boot to keep the
  single tap meaning "unmute".
- **GPU-less VMs** need `--disable-gpu --disable-accelerated-video-decode
  --disable-gpu-compositing` or video composites black even when decodable.
- **YouTube wants** keyframes ≤4s (rig uses 2s), 30fps 1080p at ~4.5Mbps.
- Streams >12h don't archive as VODs — fine, the works are already uploads.
