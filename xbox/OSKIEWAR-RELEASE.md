# Oskiewar release

The web character generator uses an authenticated AC account and its current
handle. REGARDE receives only a purpose-scoped pseudonym. An explicit Allow
opens photo upload; generation requires a live, photo-bound capability. Review
and acceptance are separate: acceptance saves the server's fighter for 24 hours,
with shorter play authority renewed against the standing grant. Reopening Add
yourself restores it without another model call. Handle changes retain ownership.
Withdrawal removes the account's saved fighter and source material. While equipped,
authority is checked every 15 seconds; a failed check clears the selection,
and its current expiry is also enforced at render time. Presentation is
limited to local practice: acceptance opens `/?practice`, restores the accepted
fighter against its live grant, and disables game broadcasts and replay uploads.
Online play uses the standard fighter. The REGARDE
gate still signs with its documented demo key.

Run browser checks on Poorslice under Node 24:

```sh
node system/tests/oskiewar-turnstile.browser.mjs
node system/tests/oskiewar-practice.browser.mjs
```

That browser regression uses synthetic API fixtures. A live test must also
exercise the deployed AC bridge, REGARDE gate, storage, and generation provider;
fixture results alone are not proof of the live turnstile. Use a designated test
account or obtain permission before withdrawing an account's existing material.

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

For an Xbox-only development update, run
`node xbox/tools/oskiewar-release.mjs deploy-xbox-dev --hot`. This uploads the
working source into the running app and verifies a new native live generation
without relaunching it. A rejected update reports the native error; an
unconfirmed reload asks you to quit and reopen oskiewar on the Xbox and check
its version. Web and iOS remain pending until a unified release.

For the temporary Xbox-versus-AC-OS venue match, use
`node xbox/tools/oskiewar-lan-test.mjs <ac-host> --hot`. It preserves a lowered
curtain while updating both seats. This is a paired development build.

The local stage MCP (`xbox/tools/oskiewar-stage-mcp.mjs`, port 7792) exposes
`oskiewar_curtain`: `down:true` pauses both games, `down:false` resumes them.
Its optional `style` is `auto`, `directions`, or `singalong`. Auto and singalong
show timed Trio lyrics when available, otherwise the right-pointing character
and arrows. Directions always shows the arrows. Lyrics come from the matching
arrangement and live audio clock; a stale feed returns to arrows. The bridge
reads music state without starting or changing the performance.
