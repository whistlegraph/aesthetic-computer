# Letters: privacy design and migration

2026-09-14 · Proposed architecture; encryption is not implemented by this document.

First implementation slice prepared in `/Users/jas/ac-amail`: generic letter
notifications (including `/tell` and incoming email), fixed mail diagnostic codes,
sanitized API errors and browser request failures, plus a rebuilt disk worker.
`node --experimental-vm-modules spec/mail-privacy-spec.mjs` exercises these paths
with synthetic content and mocked storage/transports. These are local changes;
publication and production deployment have not been performed. Stage 0 remains
open for disclosure, the broader data-access/retention inventory, and any paths
that inventory identifies. Existing stored messages are still plaintext.

People send and keep **letters**. The goal is that AC delivers private letters
without possessing the keys to read them. Outside email remains useful, with an
explicitly different protection level. “Letters” is the product concept; this
plan does not rename `/mail`, addresses, APIs, or database collections.

## Current evidence

Source baseline: deployed commit `12f2cd64e1239064cb406cf8f164edca4fe8dd8b`,
inspected in `/Users/jas/ac-amail`. The primary checkout was on another branch;
links below identify paths, whose contents may differ until that branch catches up.
No private message bodies were read for this design.

| Surface | Current behavior | Consequence |
| --- | --- | --- |
| [Mail backend](../system/backend/mail.mjs) | `deliver`, `deliverFromOutside`, and `sendOutside` store `text` and optional `subject` in `tells` | Database administrators can read internal, incoming, and sent letters |
| [Mail API](../system/netlify/functions/mail.mjs) | Authenticated inbox/sent queries return readable content | Authentication separates accounts; it does not exclude the operator |
| [Tell API](../system/netlify/functions/tell.mjs) | Calls the same `deliver` function | Privacy changes must cover this older entry point too |
| [SMTP receiver](../lith/mail-inbound.mjs) | Google Workspace forwards ordinary mail to lith, which parses it | Both services handle readable content during delivery |
| [Push transport](../shared/push.mjs) | Mail supplies subject/body previews to Web Push and APNs | Notification payloads create another content exposure; encrypted Web Push transport does not remove server access |
| [Piece runtime](../system/public/aesthetic.computer/lib/disk.mjs) | Captures console output for piece telemetry | Mail must not emit letters or keys into captured diagnostics |
| [Mail piece](../system/public/aesthetic.computer/disks/mail.mjs) | Runs inside the general AC client | Strong key isolation requires a separate trust boundary |

SPF/DKIM and SMTP TLS help authenticate or transport outside email. They do not
keep its contents from the mail operator. End-to-end protection is a separate
mechanism. [RFC 9787](https://www.rfc-editor.org/rfc/rfc9787.html)

## Privacy contract

| Letter type | Intended protection | What AC can still observe |
| --- | --- | --- |
| Private AC letter | Sender encrypts; authorized recipient and sender devices decrypt | Routing identities, delivery times, approximate sizes, device delivery state |
| Ordinary outside email | Encrypt before persistent mailbox storage once recipient keys exist | Plaintext during receipt or sending; outside addresses and delivery metadata; Google also handles content |
| Encrypted outside email | Standards-based encryption at the endpoints, when both support it | Routing metadata; exact header exposure depends on interoperable header protection |
| Older letter | Existing plaintext, or an explicitly recorded migration to encrypted storage | Earlier plaintext access and historical copies cannot be undone |

The client derives the displayed protection from the actual verified envelope and
route. It must not trust a server-provided “private” badge. Use one concise status
per letter: “Private letter”, “Outside email”, or “Older letter”. For ordinary
outside email, explain at send time: “Email providers can read this letter.”
Incoming mail encrypted only after receipt must never be labeled end-to-end encrypted.

Protection excludes a compromised reader's device, a recipient sharing a letter,
and screenshots or exported copies. Availability, traffic analysis, and the social
graph remain separate problems. Do not promise anonymous correspondence.

## Threat model and release promises

1. **Current service:** reduce accidental exposure and unnecessary staff/agent
   access. Disclose that administrators can access stored letters.
2. **Encrypted AC pilot:** a database leak or ordinary backend access reveals no
   private content. This requires keys to remain on approved devices and prevents
   account recovery from silently enrolling a decrypting device.
3. **Operator-resistant clients:** protect against key substitution and malicious
   client delivery using independently verifiable releases and verified identities.
   This is the gate for a stronger claim about a malicious operator.

Web encryption alone cannot establish the third promise: the operator can serve
modified JavaScript that steals a key or a displayed letter. CSP and a separate
origin reduce attack surface; neither proves the delivered code is honest. An
installed Electron shell that still loads remote mail code has the same problem.

## Keys, devices, and recovery

- Generate secret material locally using the selected protocol's implementation;
  do not derive it from an Auth0 token or have AC generate users' private keys.
  Local generation follows [RFC 9787 §8.2.1.3](https://www.rfc-editor.org/rfc/rfc9787.html#section-8.2.1.3).
- Bind cryptographic identities to a stable account identifier, not a mutable
  handle. Specify identity continuity for handle changes, account deletion,
  account linking, and reused names. A new owner never inherits old keys.
- Each device has its own identity and revocable authorization. A signed device
  list must prevent the server from silently adding itself as a recipient.
- Pair devices through approval on an existing device, with an authenticated QR
  or code comparison. Login alone authorizes ciphertext retrieval, not decryption.
- Show identity changes and support out-of-band fingerprint verification. Add a
  public-key transparency design with independently witnessed consistency checks;
  a log controlled and observed only by AC cannot stop targeted substitution.
- Offer a locally generated, high-entropy recovery code that wraps an optional
  encrypted archive backup. AC may store the wrapped backup, never the recovery
  secret. Keep the code out of URLs, telemetry, support tools, and server requests.
- Account reset restores account access only. Without an approved device or
  recovery material, old private letters are lost. Starting fresh creates a new
  identity and alerts correspondents; it must not look like continuity.
- Revocation stops future delivery after clients learn the new device list. It
  cannot retract content or keys already obtained by a revoked device. Specify
  freshness checks, offline behavior, and conflict resolution before shipping.

History transfer is an explicit device action. Newly paired devices do not
automatically receive every historical session key. Users may transfer an
encrypted archive or start with future letters only.

## Protocol decision

Use a maintained, reviewed implementation of an established protocol. This is a
requirements decision, not permission to invent a wire protocol or combine crypto
primitives ad hoc.

For AC letters, evaluate asynchronous ratcheting protocols first: offline delivery,
multiple devices, sender history, and identity changes are core requirements.
Signal's [Sesame specification](https://signal.org/docs/specifications/sesame/)
describes asynchronous multi-device session management; its
[Double Ratchet specification](https://signal.org/docs/specifications/doubleratchet/)
describes evolving per-message keys. These are references, not a finding that a
particular library is suitable for AC's browser/runtime/license constraints.

Keep [MLS, RFC 9420](https://www.rfc-editor.org/rfc/rfc9420.html) as the alternative
if group letters become a concrete requirement. Do not incur group membership
complexity solely for a hypothetical future feature.

Outside interoperability should use established email formats, such as
[OpenPGP, RFC 9580](https://www.rfc-editor.org/rfc/rfc9580.html), with the composition
and interpretation guidance in RFC 9787. Ordinary OpenPGP mailbox encryption must
not be marketed as providing ratcheting transport's forward secrecy.

Before choosing a library, record maintained releases, license, security review
scope, browser/native support, key storage options, official test vectors,
offline/reordered delivery behavior, device revocation, upgrade compatibility,
resource use on low-end devices, and the consequences of compromise. An external
review must cover our integration as well as the chosen implementation.

**Archive tradeoff:** ratcheting transport can discard old session keys, but a
recoverable archive preserves another route to old content. Compromise of an
archive key can expose retained history. Keep live session state out of backups;
document separately the protection of transport, local history, and archive
recovery. Do not promise forward secrecy for all retained history.

## Storage and API boundary

Introduce a versioned content representation with exclusive plaintext-legacy and
encrypted variants. The application record should carry a format version, the
allowlisted protocol/version identifier, opaque protocol envelope, migration
provenance, and routing/delivery fields. Cryptographic envelope structure and key
identifiers come from the chosen protocol, not this document.

Encrypted bodies include subjects, content, and private thread references. Drafts,
sent copies, and any later attachments receive the same treatment. Authenticate
relevant recipient, sender, version, and message identity within the protocol so
that copied or relabeled ciphertext cannot change its meaning.

Server rules:

- Encrypted records cannot contain a readable `text`, `subject`, preview, draft,
  search index, or attachment filename alongside the envelope.
- Reject unknown protocol versions and oversized/malformed envelopes; never run
  plaintext `clean()` filtering over ciphertext.
- Preserve mailbox authorization, quotas, deduplication, idempotent retries, and
  abuse controls. Encryption is not permission to write to someone else's box.
- Keep content protection separate from delivery status and SMTP sender checks.
  A DKIM pass is not proof of end-to-end identity or confidentiality.
- Search private content locally. Keep unread state separate from content, with
  no sender-visible read receipts by default.
- A recipient without private-mail keys is “not ready for private letters”. Do
  not silently use the legacy endpoint or SMTP as a fallback. Older clients must
  show an upgrade requirement, not unreadable ciphertext or plaintext fallback.

Inventory all callers of `deliver`, every writer/reader of `tells`, notification
consumers, exports, backups, and support scripts before enforcing the new schema.
The inventory must include `/tell` and any server-originated messages. System
notifications need their own honest provenance; they cannot impersonate private
user letters.

## Client isolation and operations

Private keys and decrypted content must live outside the general piece runtime.
Use a dedicated mail origin for the web client, with no user-published pieces,
third-party scripts, session replay, general console capture, or broad extension
bridges. Restrict cross-origin messages to fixed operations with checked origins;
the AC launcher receives navigation/unread information, never keys or bodies.
Audit service workers, caches, browser storage, desktop preload APIs, and exports.

For the strongest tier, bundle the mail implementation in a client with reproducible
builds, public release hashes, independently checked artifacts, and reviewed update
authorization. Signing alone is insufficient when the operator controls the signing
key. Explicitly address targeted updates and old-version rollback.

Use generic notifications, “You have a new letter”, with only an opaque routing
identifier where necessary. Keep raw content out of errors, push provider details,
crash reports, mail transport debugging, analytics, and local agent/MCP access.
Operational telemetry should use allowlisted codes and aggregate counts, with
documented retention and tightly controlled metadata access.

Separate SMTP gateway, mail storage, and other production credentials. Least
privilege limits accidental access today; a root operator can still override it.
Private-mail abuse controls use blocking, sender quotas, delivery limits, and
explicit recipient reports. A report shares only the letters the user selects,
with a preview of the disclosure; it never grants administrators mailbox keys.

## Outside email

Ordinary inbound email is readable by Google and our receiving gateway. Once a
recipient has a storage public key, encrypt immediately before persistent filing.
Audit queues, temporary files, SMTP diagnostics, swap/core dumps, and backups so
that “encrypted storage” describes the complete retention path. Public-key lookup
and rotation must be authenticated; otherwise the operator can substitute a key.

Ordinary outbound email similarly exposes plaintext to the sending gateway and
providers. Store the sender's archive copy encrypted on their device. Do not route
a private AC letter through SMTP when a private delivery fails.

For a mailbox with no key, retain the disclosed legacy behavior during transition
or defer delivery according to an explicit enrollment policy; never generate a
server-held private key and call it user-owned encryption. Define retry/bounce
behavior before changing the inbound gateway.

Preserve encrypted outside messages without destructively filtering or truncating
their cryptographic MIME structure. The current text extraction and 2,000-character
limit need a separate reviewed path before OpenPGP interoperability is offered.

## Migration and release gates

| Stage | Work | Exit evidence |
| --- | --- | --- |
| 0: Limit exposure | Generic push; accurate disclosure; sanitized diagnostics; inventory data access, backups, exports, and all `tells` consumers | Synthetic content canaries absent from diagnostics/push; all known copies and credentials documented |
| 1: Prepare compatibility | Versioned representation; explicit protection state; safe old-client behavior; API contract | Existing mail works; encrypted variant rejects plaintext sidecars; no silent downgrade |
| 2: Private pilot | Isolated client; selected reviewed protocol; local keys; pairing, recovery, revocation, identity verification | Two consenting test accounts pass lifecycle and adversarial scenarios below |
| 3: Migrate storage | Enroll users; migrate selected old letters; encrypt ordinary inbound storage; retire plaintext copies | Verified migration ledger, recoverable encrypted copies, retention deadlines, restore rehearsal |
| 4: Stronger operator resistance | Independent client verification and witnessed key transparency | External review and tests for targeted code/key substitution before stronger public claims |

For stage 3, client-side migration should fetch only the user's authorized legacy
letters and create encrypted archive copies, then verify decryption and content
integrity before requesting deletion of that user's plaintext copy. Make batches
idempotent, resumable, and auditable without logging content. Preserve unread state,
dates, and duplicate handling.

Today one internal `tells` row serves both sender and recipient. Split ownership
into per-account archive copies or equivalent independent grants before deletion.
One person's migration or deletion must not erase the other person's letter.
An unenrolled correspondent's plaintext copy means that content remains
operator-readable; do not describe partial migration as complete confidentiality.

Inventory replicas, exports, push traces, and backup providers. Assign actual
retention deadlines from that inventory before claiming deletion; those schedules
are currently unknown. Backup restores must reapply deletion/migration tombstones
before serving mail. Do not create a new plaintext backup to make migration easier.
Evidence of historical plaintext access remains a limit even after all copies expire.

Rollback stops new enrollment and keeps ciphertext readable through the last safe
client. It must never decrypt server-side, recreate plaintext records, or downgrade
new letters. Roll out separately from unrelated piece and SMTP changes.

## Required verification before a private pilot

- Offline recipient; reordered, duplicate, delayed, and replayed messages; retry
  after interrupted send; sent history available only to approved sender devices.
- Second device pairing; rejected rogue device; revoked device; stale device list;
  missing prekeys/session material; explicit history transfer.
- Recovery success; wrong/lost recovery code; all devices lost; Auth0/password
  reset cannot decrypt archives or silently preserve cryptographic identity.
- Handle rename, name reuse, account linking/deletion, identity rotation, and
  attempted directory substitution or inconsistent transparency views.
- Tampered body, subject, sender, recipient, version, and message ID fail safely;
  invalid envelopes cannot trigger plaintext fallback or content-bearing errors.
- Cross-account API access fails; ciphertext-only database snapshot and production
  credentials cannot decrypt pilot letters; a separately controlled client detects
  the modeled malicious-directory attack.
- Synthetic unique body/subject/key markers do not appear in server logs, piece
  telemetry, analytics, push payloads, temporary files, exports, or backups outside
  the explicitly disclosed ordinary-SMTP boundary.
- Old client/new record, new client/legacy record, incomplete migration, independent
  sender/recipient deletion, backup restore, and rollback all preserve the contract.
- Resource use and accessibility remain acceptable on a small phone; locked keys,
  recovery loss, outside delivery, and changed identities have understandable UI.

Use synthetic letters and consenting pilot accounts. No real users' mail is needed
for these checks. The first implementation slice is stage 0 plus the data-consumer
inventory; protocol selection and an isolated prototype follow. This document does
not authorize stronger privacy claims before their release gates pass.
