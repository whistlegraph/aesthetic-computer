# Aesel account, execution, and data contract

Current implementation, September 30, 2026. This replaces the early local-only
proposal. Aesel requires a verified AC account and an @handle for native,
browser, and terminal work, including Claude/Codex and private terminal mode.

## Execution

- Mac and iPhone use SwiftUI/WebKit around the shared JavaScript session. The
  browser uses that session too; its build deploys separately from Lith.
- AC hosted inference sends conversation context and tools to AC's inference
  endpoint, which relays them to OpenRouter. Free allowance and purchased
  braincells belong to AC, independently of vendor subscriptions.
- Claude and Codex use installed vendor CLIs. Native Mac connects through an
  authenticated loopback helper; it rejects browser-origin requests. Phone
  pairing is not implemented. Provider credentials stay on the execution host.
- Terminal piece mode restricts provider configuration. Pro mode inherits
  supported local provider configuration and tooling. Approval and containment
  depend on the bridge: Claude approvals are not an operating-system sandbox;
  Codex uses its configured workspace sandbox. Neither implies offline use.

## Credentials and recovery

The terminal shares `~/.ac-token` with AC tools. Native access and rotating
refresh credentials use device-only Keychain items; refresh credentials never
enter notebooks, exported files, or the session WebView. Native sign-in requests
`offline_access`; older grants without a refresh token need a new sign-in.
Concurrent native windows share a refresh operation. Sign-out or account
replacement invalidates late refresh results. Transient failures preserve saved
credentials and work. Only account verification is retried after renewal;
inference and publishing are never automatically replayed because of auth failure.
The browser uses Auth0's SPA SDK for its own sign-in lifecycle.

## Publishing and preview

Piece auto-publishing is on by default. Native sessions save the publication
setting per thread; terminal sessions can use `/autopublish off`,
`--no-autopublish`, or `AESEL_AUTOPUBLISH=0`. Pro/private terminal sessions do not
publish pieces. Turning publishing off does not unpublish earlier public work.

Publishing sends source under the signed-in handle and verifies public bytes.
Native receipts bind source, owner, and revision; a newer draft is not marked
published by an older upload. Local history and notebook export are separate
from public publication.

Terminal live channels send source and an AC bearer token to `/run`; signed-in
channel ownership is enforced by the token, not secrecy of the channel name.
Native preview injects local source into the network-loaded AC runtime. Turning
publishing off therefore does not imply an offline runtime or no runtime traffic.

Native preview evidence matches thread, revision, and SHA-256 of the supplied
source. The worker carries identity on rendered frames and the browser
acknowledges after drawing. Captures without matching evidence are not certified.
The local diagnostic bridge retains the latest 100 worker-console events,
truncated to 2,000 characters each. Ordinary runtime telemetry remains separate.
Older deployed runtimes cannot supply this proof; app and runtime updates are
both needed. Rendering proof does not certify interaction, accessibility, or
correct artwork.

## Conversations and retention

Non-private terminal sessions require the account-bound transcript disclosure:
messages, assistant replies, and artifact references are uploaded to AC and
retained until deletion. `/transcript delete` removes the uploaded transcript;
future messages can be uploaded under the accepted policy. Private terminal
mode omits the shared journal and hides its subject from Slab; provider inference
and account verification still use the network.

Native notebooks live in app-local storage; browser notebooks live in
account-separated localStorage. These clients do not instantiate the terminal
transcript journal. Hosted inference still sends their conversation context
through AC to the provider. Browser disclosure currently also mentions staff
access; do not treat that wording as proof of a separate transcript-sync feature.
Local notebooks are not cross-device sync or an automatic backup.

## Purchased braincells

Purchased-wallet reservations atomically include settled daily spending, all
outstanding holds, and the new request. Pending work crossing midnight remains
reserved until resolved. A settlement records its intended charge before the
balance mutation; removal of the hold prevents duplicate or late charges.

Inference has a five-minute upstream deadline. Lith reconciles persisted
settlements and releases unknown holds after ten minutes, once that deadline
has passed. Unknown usage is AC's expense. Reservations also reconcile when the
account next requests paid work. A database outage delays recovery and is logged;
it must not trigger another inference request. These limits apply per upstream
request, not to a user-selected whole-turn spending ceiling.

The free allowance still uses post-request metering and can overshoot under
concurrency. A customer-visible settlement history, whole-turn budgets, and a
versioned tariff ledger remain separate work; the older AC-stones proposal is
not the current billing contract.
