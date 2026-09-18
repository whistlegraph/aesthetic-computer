# Easel Mac App Store track

Easel has two Mac distribution tracks:

* The Developer ID DMG/ZIP is the full-power desktop build. It can use Easel's
  native PTY, optional Claude/Codex provider bridges, and Easel's signed binary
  update feed.
* The `mas` target is the App Store candidate. It is sandboxed, uses the AC
  hosted provider as its default, and receives updates only through the Store.

Build the candidate on an Apple-silicon Mac after installing the Mac App Store
distribution certificate and provisioning profile:

```sh
cd easel/desktop
npm install
npm run dist:mas
```

Before submitting, test the exported `.app` in a clean macOS user account. In
particular, verify first launch, sign-in, required transcript-sharing
disclosure, creating and publishing a piece, picture preview, QR links, text
selection, and resume after relaunch. Confirm that the sandbox does not break
the PTY or preview webview. If App Review rejects child-process execution under
the sandbox, the Store track must use a hosted-session renderer without the
local Claude/Codex bridges; the direct DMG remains the full Easel build.

Store metadata still needs to be entered in App Store Connect: subtitle,
description, privacy answers, support URL, screenshots, age rating, and review
notes explaining the hosted inference and required transcript-sharing
disclosure. The Store build must not call `electron-updater`; App Store Connect
owns its update lifecycle.

The release script signs the app with the Mac App Store application identity,
then signs the outer `.pkg` with the separate installer identity required by
Apple. This works around electron-builder 25's single-identity installer step.

Current account check: the shared App Store Connect API key has registered
`computer.aesthetic.easel`, created a Mac App Store distribution certificate,
and created an active `Easel Mac App Store` profile (profile UUID
`5725cc89-feef-4bda-80eb-1c4f30f43500`). The profile is installed on the release
Mac. The macOS app record was created through Chrome on September 16, 2026,
as **Aesthetic Easel**, then renamed to **Aesel**, app ID `6812823093`,
SKU `easel-mac`, locale `en-US`. Aesel is the chosen product name.
Apple accepted Aesel and rejected the plain name `Easel` as already in use.

Listing: https://appstoreconnect.apple.com/apps/6812823093

Use `node slab/menuband/bin/asc.mjs status --app 6812823093 --platform MAC_OS`
from the main repository to inspect the submission state. Creating the record
does not upload a build or submit it for review.

## September 17 review repair

The visible product name is now lowercase `aesel`. The registered bundle ID
remains `computer.aesthetic.easel`, including the new iOS target, so iOS can
be added to the same App Store record.

Apple's 2.4.5 review asks why `network.server` is present. It is needed by
`ACSession.login()` for a temporary loopback PKCE callback; removing it would
break account sign-in. See [review notes](APP-REVIEW-NOTES.md). The listener now
stops accepting connections before the outgoing code exchange. Focused tests
cover success and timeout/retry cleanup.

The rebuilt package is `dist/mas-arm64/aesel-0.7.1-mas-arm64.pkg`, build 0.7.3.
App and installer signatures were verified. Detailed entitlement notes have
been saved in App Store Connect; upload, build selection, and resubmission
remain separate release steps.
