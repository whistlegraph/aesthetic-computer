# Windows track display

The concert view shows the active title, phase, elapsed/total time and progress.
Audio and visual metadata refresh together when the score hash changes.
Receiver heartbeats include the same track state for remote verification.

Based on the working Blueberry SUB receiver modules, with track presentation
informed by Neo’s Oskiewar performance stage. `track-info.mjs` normalizes ready,
playing and finished states and provides a readable Notepat title.

Apply while idle to an existing receiver directory:

```sh
python3 bundle.py /path/to/receiver
node --test track-info.test.mjs
```

The bundler saves initial backups and inlines the helper into existing routes;
no server restart is needed. Reload the browser, enable audio and restore
fullscreen. Verified on Windows: automatic Wake→Notepat title/duration switch,
25% output, both channels, -60ms timing offset. The offset remains a listening
estimate, not an acoustic calibration.
