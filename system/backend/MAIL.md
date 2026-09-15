# Mail media

Letters use chat-style references: `#painting`, `!tape`, and `$kidlisp` codes.
The inbox and sent views resolve public records, show painting previews, and
open each reference in its AC viewer. Unrecognized codes remain text. Outgoing
email includes canonical links in plain text and linked paintings in HTML;
it does not upload or copy media files.

Incoming SMTP attachments, including inline MIME images, are stored with the
letter in `tells`. They are not published as paintings or MIME posts. The
recipient can preview PNG/JPEG/GIF/WebP images and download any file type.
Remote images in an HTML email are not fetched.

Limits: 10 files, 8 MiB combined decoded bytes, 12 MiB wire message. An oversized
letter receives SMTP 552 before any recipient's copy is filed. Base64 storage
plus the bounded letter body remains under MongoDB's 16 MiB document limit.
Existing message-ID deduplication also covers the files.

`GET /api/mail` returns file metadata only. Authenticated downloads use
`?id=<letter-id>&attachment=<index>`. The caller must be the stored recipient
or sender. The default response downloads raw bytes; `&json=1` returns base64
for the worker bridge, and `&preview=1` returns a bounded PNG thumbnail. All
responses use `private, no-store`. Invalid images remain downloadable.

Validation (no production data or outbound delivery):

```sh
npm ci --prefix lith --ignore-scripts
node --experimental-vm-modules spec/mail-privacy-spec.mjs
node --experimental-vm-modules spec/mail-media-spec.mjs
```

The media spec exercises real MIME generation and loopback SMTP with an
isolated in-memory mailbox. Deploying this change requires both lith's web
process and `lith-mail.service` to reload the changed modules.

## Delivery inspection

Mail emits structured `mail.event` JSON into the `lith` / `lith-mail` journals
and mirrors it asynchronously into MongoDB `mail-events`, retained for 30 days.
The mirror has a bounded queue; database failures emit `telemetry_unavailable`
in the journal and never delay or fail a letter. A sudden process exit may lose
queued database events; use the journal to investigate gaps. Failures before an
API database connection exists are journal-only.

Each API response includes `X-Mail-Trace`. That UUID follows the send through
storage, SMTP relay, and push. An inbound SMTP transaction has its own trace,
shared by its recipient copies. Stored copies include their opaque letter ID
in telemetry. Bodies, subjects, addresses, handles, IPs, filenames, raw MIME
message IDs, and provider response text are excluded. Nothing is sent to PostHog.

From the repository on lith (or locally with a configured `system/.env`):

```sh
node --env-file=system/.env system/backend/mail-events-cli.mjs --since 60
node --env-file=system/.env system/backend/mail-events-cli.mjs --since 1440 --failures
node --env-file=system/.env system/backend/mail-events-cli.mjs --trace <UUID> --json
node --env-file=system/.env system/backend/mail-events-cli.mjs --letter <letter-id>
journalctl -u lith -u lith-mail --since '1 hour ago' -o cat | rg '"kind":"mail.event"'
```

The CLI summary covers the full selected window; the chronological detail is
limited to the latest 100 events (`--limit` up to 1000). Counts are **events**,
not unique letters: a rejected message can also have a `smtp_response` status
and a disconnect event. Successful unread-count polls are omitted.

- `stored`: inserted in the recipient mailbox (or the sender's SMTP sent list).
- `accepted` / SMTP 250: inbound copies filed or recognized as duplicates.
- `relay_accepted`: the external SMTP relay accepted it; this does **not** prove
  delivery to the destination inbox. A later storage failure shares its trace.
- `duplicate`: an incoming retry was already filed; no second copy or push.
- `rejected` / 5xx: fixed reason such as unknown recipient, DMARC, route stamp,
  attachment/wire limits, or sender-to-recipient hourly limit.
- `deferred` / 451: temporary lookup/storage failure or global rate limit;
  the sending relay should retry.
- `smtp_response`: a 4xx/5xx status sent by the SMTP library, including SIZE and
  protocol rejections that precede application callbacks. No response text.
- `push`: attempted/succeeded/failed/pruned counts. `no_devices` means the
  letter was stored but the recipient has no matching registered push device.
  `push_quiet` means the inbound notification quota was reached; mail is kept.
- `ready`: inbound TLS, open-relay testing mode, and route-secret presence.
  `relays_refreshed` / `relays_failed`: hourly Google relay-list refresh health.

Limits remain enforced. A multi-recipient DATA transaction checks all quotas
before storing any copy; it cannot return 250 while skipping a throttled box.
Recipient state resets at every MAIL command, including reused connections.

This sees AC and SMTP handoff outcomes from deployment forward. It cannot see
Google Workspace quarantine/routing decisions before a connection reaches AC,
or a remote provider's spam placement after relay acceptance. For those, use
Workspace Email Log Search and the sender's bounce notice.

Additional regression coverage:

```sh
node spec/mail-events-spec.mjs
```
