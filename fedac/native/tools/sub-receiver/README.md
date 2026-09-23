# SUB receiver

Windows Edge/Chrome receiver for the room subwoofer. The controller serves the app and follows a native laptop's running score; the PC synthesizes the bass and kick lanes locally, one octave down. It does not receive a mixed audio stream.

Run on the controller (Node 20+):

    node fedac/native/tools/sub-receiver/server.mjs

Open the printed LAN URL on the Windows PC. Choose the subwoofer device as Windows' output, click **Enable audio**, then **Test 60 Hz**. **Save Windows shortcut** downloads a desktop Internet shortcut. Keep the controller and browser open, with the PC on the same reachable network.

The receiver registers as SUB at `/api/receivers`; a heartbeat older than four seconds is offline. Playback requires a local audio-enable click, follows the current passage and later cues, and stops on stale source/network data. Start level is 25%. Output defaults to left only; right and dual mono are selectable. Filtering: 25 Hz high-pass and two cascaded 80 Hz low-pass sections; compressor and final ±0.89 sample clamp. This is not a calibrated hardware or true-peak limiter.

`SUB_SOURCE=192.168.1.237:8080` selects the reference laptop; `PORT=8788` changes the listening port. The source must run spatial-rehearsal-compatible status updates. The source score must match the bundled Note(s)pat(ial) Native score. Wi-Fi polling, browser scheduling, and device latency make synchronization approximate; adjust Timing offset after listening. Windows hardware/cabling and acoustic phase require an on-site test. The app does not request a microphone.

    node --test fedac/native/tools/sub-receiver/core.test.mjs

This is a local-network service with no Internet account or dependency installation. Keep it on the rehearsal LAN. The app uses Windows' selected output; it does not automatically change system audio devices.

Windows portable package: distribute SUB-Windows.zip, extract fully and run Start SUB.cmd. It includes the verified official Windows x64 Node runtime and the score, and binds its UI to localhost. config.json lists the AC OS sources; the receiver fails over between them and advertises its status back to the fleet. The existing RustDesk connection handles remote support. No additional remote-access service is installed. Windows launch requires an on-PC check.

Trio mode uses localhost-only POST endpoints `/api/trio/load`, `/prepare`, `/play`, `/keepalive`, and `/stop`. Load validates the new score rather than reusing the original duration. Prepare accepts the arrangement hash, run ID, and a future UNIX start epoch; the server anchors it to its monotonic clock. Keepalive must arrive within three seconds while prepared or playing. Receiver heartbeats include the loaded hash, duration, audio state, and fullscreen state. The MacNeopolitan conductor supplies these calls and owns the global stop.
