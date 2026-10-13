# choir

A tagged room for art-world professionals, from Fía's notes of 2026-10-12.
One main chat; any post can grow a thread; every post carries at least two
tags; anyone can start a tag; a brake stops over-posting. Links get a preview,
no uploads. Notifications are a per-account switch.

## Where it lives

| part | file |
| --- | --- |
| store logic (pure, tested) | `system/backend/choir.mjs` + `choir.test.mjs` |
| HTTP door | `system/netlify/functions/choir.mjs` → `/api/choir` |
| iPhone app (the main way in) | `apple/choir` (SwiftUI, xcodegen, `fastlane ios …`) |
| pointer page | `system/public/choir/index.html` → `aesthetic.computer/choir/` |
| web room (fallback) | `system/public/choir/room/index.html` → `/choir/room/` |
| app sign-in bridge | `system/public/choir/auth.html` (invisible web view) |
| live trail | `system/backend/choir-events.mjs` → `choir-events`, read at `/api/choir?events=1` (admin) |
| link previews | existing `/api/og-preview` |
| sign-in | aesthetic tenant, passwordless, `lib/auth0-otp.mjs` (email + six digits) |

Mongo collections: `choir-posts`, `choir-tags`, `choir-prefs`, `choir-events`. Post ids are
24-hex strings. A reply carries `replyTo` = the root's id and inherits its tags;
the main chat lists roots only. Deleting blanks the words and keeps the row so
threads keep their shape.

```
node --test system/backend/choir.test.mjs
```

## Decisions so far

- Name: **choir**. Domain not yet chosen (a new TLD AC manages).
- Auth: AC's Auth0 tenant, same handles, fully passwordless. Posting needs a
  handle; an account without one is pointed at aesthetic.computer to pick one.
- Federation comes from handles already syncing to the AT Protocol PDS.
- An iOS app, paid, over the same API. Since posts federate, the price buys
  membership and posting, not reading.

## When the domain exists

1. Caddy: copy the `mime.ac` block in `lith/Caddyfile` (static root
   `system/public/choir`, everything else `import lith_proxy`), plus the
   `www.` redirect. Add the host to `utilities/machine-properties.mjs` and
   regenerate the machine manifests.
2. Auth0 (aesthetic tenant, the SPA client `LVdZ…`): add the new origin to
   Allowed Web Origins, Allowed Origins (CORS) and Allowed Callback URLs. The
   OTP flow's silent leg matches `redirect_uri` against that list.
3. `system/public/choir/index.html`: `API` stays relative; the module import
   and og-preview already fall back to `https://aesthetic.computer` off-origin.

## Not built yet

- Live updates (the page polls every 15 s). The session-server chat manager
  can carry a `chat-choir` instance when it matters.
- Push for notifications (the switch is stored, nothing fires).
- Setting a handle from inside choir.
- Moderation beyond the rate brake: blocking, reporting, admin delete.
- The paid iOS shell.

## Accounts

An email-code sign-in is its own Auth0 account (`email|…`), apart from the
password or social account that already holds someone's @handle. With the same
verified email, choir posts as the handle-holding account
(`handleBySharedEmail` in `system/backend/authorization.mjs`). Account deletion
from the app deletes the account that signed in, not that sibling.

## iPhone app

Native SwiftUI. Sign-in UI is native; the email-code exchange runs in an
invisible web view on `aesthetic.computer/choir/auth.html`, because Auth0 ends
the flow in an iframe that must return to a registered origin. Refresh is a
plain token call. Hide and Report live in each post's long-press menu.

```
cd apple/choir
fastlane ios bundle                                  # done 2026-10-12
FASTLANE_USER=<Apple ID> fastlane ios create         # once, needs 2FA
fastlane ios build && fastlane ios upload
fastlane ios distribute                              # makes "Choir Friends", submits beta review
fastlane ios invite email:<email> name:<name>
fastlane ios status
```

Watch the trail:

```
curl -s -H "Authorization: Bearer $AC_TOKEN" "https://aesthetic.computer/api/choir?events=1&limit=50"
```
