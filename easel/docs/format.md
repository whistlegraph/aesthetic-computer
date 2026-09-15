# `.easel` transcript format, version 1

An `.easel` file is UTF-8 JSON Lines: one session header, followed by ordered
messages, artifact references and selected progress events. Every line ends in
LF, including the final line. It is plain text, appendable, and queryable with
`jq`; it is not a ZIP archive or an opaque provider session.

```jsonl
{"type":"session","format":"aesthetic.easel","version":1,"id":"demo-session","createdAt":"2026-09-15T12:00:00.000Z","metadata":{"medium":"sound","projectId":"project-one","title":"Bell phrase"},"consent":{"sharing":"private"},"provenance":{"application":"easel","version":"0.7"}}
{"type":"message","id":"turn-one","seq":1,"at":"2026-09-15T12:00:01.000Z","role":"user","text":"Make a bell phrase","backend":"ac","model":"glm"}
{"type":"artifact","id":"revision-one","seq":2,"at":"2026-09-15T12:00:04.000Z","artifactId":"sound-one","medium":"sound","revision":1}
{"type":"message","id":"turn-two","seq":3,"at":"2026-09-15T12:00:05.000Z","role":"assistant","text":"The bell phrase is ready.","backend":"ac","model":"glm"}
```

```sh
jq -r 'select(.type == "message") | "\(.role): \(.text)"' session.easel
jq -c 'select(.type == "artifact")' session.easel
```

IDs are stable strings of 1–80 ASCII letters, digits, hyphens or underscores.
Record sequence numbers increase monotonically; upload batches can omit earlier
sequence numbers. The session header has no sequence number. Record timestamps
are ISO date strings. Session `metadata` permits only `medium`, `projectId` and
`title`; project IDs are opaque IDs, never filesystem paths. Media are `picture`,
`sound`, `piece`, `paper`, and `gameboy`.

Message roles are only `user` or `assistant`; text is capped at 32,768 characters.
Optional `backend` and `model` describe who produced a final message. Token chunks,
system prompts, tool output, credentials, contacts, mail, arbitrary files, and
raw provider payloads are not accepted record fields. The user's own final turn
text can still contain sensitive details they typed; a whitelist cannot infer
all private information inside prose.

An artifact record contains only its artifact ID, medium and revision number.
It does not embed an image, sound, PDF, program source, path, or file contents.
Event names are `turn-start`, `turn-complete`, `turn-interrupted`, `render-start`,
`render-complete`, `render-failed`, and `model-change`; optional status is
`working`, `complete`, `interrupted`, or `failed`. Provider state needed for a live
restart remains separate and private. A transcript alone is not a complete
provider checkpoint or artifact bundle.

`transcript-format.mjs` provides `validateHeader`, `validateRecord`,
`serializeTranscript`, `parseTranscript`, and `redactTranscriptText`. Unknown
fields and duplicate/out-of-order record IDs/sequences are rejected. Documents
are capped at 8 MiB. Exports are strict about the final newline. Only an explicit
`parseTranscript(text, {recoverPartial:true})` may discard an unfinished final
line after a crash; malformed complete lines remain errors.

## Required company sharing

Before using Easel, each signed-in account must acknowledge disclosure version 2: future user messages, assistant replies, and artifact revision references are shared with authorized AC staff for product improvement, with a 30-day retention limit. Declining exits before creating a session or transmitting conversation content. Existing private messages are not backfilled. Account changes require a separate acknowledgment. The disclosure is available through `/sharing`; there is no optional private-use mode.

A `.easel` journal remains on disk. Uploads use authenticated batches of at most 100 records / 256 KiB. Recognizable credentials are redacted on both client and server. Failed uploads remain queued; a new generation cannot start until its user message has been journaled and the pending upload succeeds. `/transcript export FILE.easel` exports the local transcript; `/transcript delete` deletes the uploaded copy, while future messages continue under required sharing.

Uploaded records use consent disclosure version 2. Format readers retain support for historical version 1 consent records. Raw reads require an explicit server-side staff subject allowlist; absent configuration denies all reads. Owners can delete their uploaded transcripts but cannot read arbitrary company records. TTL indexes enforce 30-day retention, and reads exclude expired records immediately.

## Company access and retention

The endpoint uses AC's server-verified subject for ownership. It has no public
read route. GET requires an explicit subject in `EASEL_TRANSCRIPT_STAFF_SUBS`
(comma-separated Auth0 subjects); an email, handle, client-supplied owner field,
or unverified staff claim cannot grant access. Staff reads require a named owner
and session and return at most 100 records, with an `afterSeq` cursor. DELETE
always scopes to the authenticated owner and accepts `{sessionId}` or explicit
`{all:true}`.

Records live in Mongo's `easel-transcripts-private` collection. Each row has
server-assigned receipt and expiry dates. The TTL index deletes after 30 days;
read queries also exclude expired rows while Mongo's TTL worker catches up.
Retries do not renew retention or mutate an accepted stable record ID. A partial
batch failure is retryable with identical IDs. The unique owner/session/sequence
index prevents two different events from claiming the same position.

These are company-private application records for improving Easel. They are not
sent to PostHog or automatically forwarded to model providers or training jobs.
This prototype supplies storage and access controls, not a training pipeline,
company review UI, or a promise of end-to-end encryption.
