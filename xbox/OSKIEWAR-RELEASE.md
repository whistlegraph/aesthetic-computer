# Oskiewar release

`npm run oskiewar:deploy` is the only canonical live release command. It
fingerprints `xbox/live/oskiewar.js`, refuses uncommitted game source, records
an obligation for web, iOS, and Xbox, deploys the web first, verifies its
production bytes, then updates Xbox through Device Portal.

iOS loads that verified web game and polls every two seconds, with its bundled
copy as an offline fallback. Game-only updates therefore need no App Store or
device rebuild. Changes under `apple/oskiewar` or `xbox/native-bios` escalate
the receipt to a native refresh instead of claiming false parity.

Use `npm run oskiewar:parity` to inspect the current fingerprint and all three
channel states. If a device or service was unavailable, the failed or pending
state remains in `.git/oskiewar-parity.json`; run
`npm run oskiewar:reconcile` when it returns. A new source fingerprint creates
a new set of obligations without erasing the last channel receipts.

Direct canonical Xbox source deployment is blocked by `xbox/tools/live.mjs`.
Diagnostic pieces and non-Oskiewar experiments remain available through that
tool.
