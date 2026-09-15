# Live draft broadcasts

Every signed-in artifact automatically reserves a stable QR and broadcasts its saved preview. Reopening resumes the same link; there is no `/live` command. Waiting viewers reconnect automatically. The selected artifact renews an unchanged frame before its one-hour expiry; closing the app leaves the last frame available until expiry. Publication remains separate.

`/api/easel-live` keeps one current draft frame per broadcast in Redis for one hour. It creates no painting, published piece, or permanent media record. A watch link is a read capability: anyone holding it can view the current draft until stopped or expired. Writes require an authenticated account with an AC handle and remain bound to the stable account subject.

The client chooses a cryptographically random 128-bit ID encoded as 32 lowercase hexadecimal characters. It persists a strictly increasing positive integer `sequence` before sending each update or stop. Version describes the artifact revision; sequence orders requests, including stops and restarts.

- `POST` JSON: `{id, sequence, kind, version, mime, data, status:"live"}`. `data` is canonical base64, at most 8 MiB decoded. Unknown fields are rejected.
- `GET ?id=…` returns `{id, route, kind, version, mime, sequence, status, updatedAt, expiresAt}`. `route` is the canonical `https://aesthetic.computer/watch/?id=…` address. No frame bytes or owner identity are included.
- `GET ?id=…&frame=sequence` returns raw bytes. A superseded sequence returns 409, stopped returns 410, and an expired or unknown ID returns 404.
- `DELETE` JSON: `{id, sequence}`. Owner-only; removes frame bytes immediately and retains a stopped metadata tombstone for one hour. A newer owner sequence may restart that ID.

Metadata and frames use `Cache-Control: no-store`. Viewers poll metadata every 500–1000 ms and fetch bytes only after a sequence change. Late joiners receive the current frame. There is no frame history, prompt, conversation, or workspace-file upload in this protocol. Piece source is an explicit supported draft output and must only execute within a sandboxed viewer.

| Medium | MIME types |
|---|---|
| Picture | `image/png`, `image/jpeg`, `image/webp` |
| Sound | `audio/wav`, `audio/mpeg`, `audio/ogg` |
| Paper | `application/pdf`, `text/plain` before compilation |
| Game Boy | `application/x-gameboy-rom`, `text/plain` before compilation |
| Piece | `text/javascript`, `text/plain` |

One Redis Lua transaction enforces ownership, increasing sequence, frame replacement, expiration, and owner limits. Each owner can have 64 active broadcasts, 120 successful writes/stops per minute, and 64 MiB of decoded updates per minute. Client coalescing targets at most 256 KiB/second on average; large frames must update less often. The 8 MiB maximum is a frame-size ceiling, not a promise of that size at two frames per second. Stop and expiration remove frame bytes; stopped metadata also expires. Anonymous reads do not extend retention.

## Measured envelope — 2026-09-15

A bounded test executed the actual Lua script and Redis adapter from Lith against its configured Redis service, with 30 injected authenticated subjects and anonymous viewer reads. It sent 600 updates and 1,200 viewer reads using 64 KiB frames. Every frame was checked against its maker's expected bytes; total verified frame transfer was 39,321,600 bytes.

The run completed in 4.15 seconds, including additional isolation, stop, and expiry checks. Combined update → metadata → frame latency was 92 ms at p50 and 202 ms at p95. It also verified rejected foreign writes, rejected stale sequences, the four-active-broadcast limit, physical frame removal on stop, and actual expiry. The expiry check shortened only one uniquely namespaced test key to one second. All test keys were removed afterward; production keys and expiration settings were untouched.

This measures the backend Redis transport with generated fixture bytes. It excludes real Auth0 validation, public-network latency, viewer rendering, model inference, media generation, and sustained classroom operation. It does not establish support for 30 simultaneous 8 MiB frames at two updates per second. At 30 maximum-sized current frames, base64 payload alone would occupy about 320 MiB before Redis overhead.

Focused regression checks: `node --test easel/test/live-broadcast-api.test.mjs`. These include a separate 30-maker in-memory simulation; its timings are not Redis or network capacity measurements.
