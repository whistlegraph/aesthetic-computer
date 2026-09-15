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
