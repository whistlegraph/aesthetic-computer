# Aesel implementation and remaining work

Reviewed September 30, 2026. This inventory describes source behavior;
release manifests and device acceptance determine what users actually have.
Electron is retired. Mac and iPhone share SwiftUI/WebKit and the JavaScript
session; browser and terminal remain separately packaged clients.

| Area | Implemented | Remaining work |
| --- | --- | --- |
| Piece workflow | Hosted inference, verified account/handle gate, automatic publishing, saved notebooks | Physical-device acceptance across errors, interruptions, and account changes |
| Native providers | Mac loopback helper, Claude/Codex readiness and models, scoped approvals, durable operations/reconnect | iPhone pairing, host sleep/revocation acceptance |
| Preview | Retained runtime, expansion, volume/tempo, thread/revision/source-hash evidence, bounded worker diagnostics | Live runtime/app compatibility checks, interaction and accessibility acceptance |
| Credentials | Keychain, PKCE, rotating refresh tokens, shared refresh across windows, sign-out fencing | Live Auth0 rotation/device acceptance; existing grants may require sign-in |
| History | Source editing, version selection, append-only restore, unsent drafts, notebook import/export | Cross-device sync and conflict policy |
| Publishing | Per-thread control, owner/source/version receipts, bounded upload retries | Live account-switch and interrupted-upload acceptance |
| Billing | Free/purchased balances, conditional funding and refunds, atomic paid cap including holds, durable settlement recovery | Atomic free allowance, user whole-turn caps, receipt/history UI, real Mongo concurrency and purchase/device acceptance |
| Browser delivery | Extracted build from pushed Git revision, atomic release switch, served-revision verification | Deploy current source; verify live login, inference, and publication |
| Mac delivery | Signed/notarized direct release tooling, bundled terminal/Node/helper | Publish coordinated app/runtime/TUI releases; old Electron notebook migration |
| Media | Terminal Picture, Sound, Paper, Game Boy toolkits and artifact revisions | Native renderers, toolchains, file grants, import/export and device controls |
| Diagnostics | Native automation, scoped preview evidence, local history, billing recovery logs | Redacted support bundle, end-to-end failure dashboard |

Acceptance for this recovery: extracted browser build; concurrent reservation
and interrupted-settlement tests; refreshed credentials without duplicate work;
wrong-thread/stale-frame rejection; native compile checks. Mocked checks do not
prove live Auth0, MongoDB, provider billing, App Store purchases, or device UI.

Before expanding media or phone pairing, verify the current Piece workflow from
an installed release: sign in → create → revise → inspect → publish → restart →
restore. Exercise token expiration, interrupted inference, failed publication,
multiple windows, keyboard focus, VoiceOver, and reduced motion.

See [the current data contract](../../aesel/docs/local-contract.md),
[build and release instructions](README.md), and
[the historical media proposal](../../aesel/docs/next-release.md).
