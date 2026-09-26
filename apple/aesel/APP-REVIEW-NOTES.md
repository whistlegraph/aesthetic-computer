# Aesel 0.7.19 for Mac

This build replaces the Electron implementation with SwiftUI and WebKit under
the existing `computer.aesthetic.easel` App Store identity. It is a universal
Apple silicon / Intel app for macOS 14 or later. The app is named Aesel.

Sign-in uses `ASWebAuthenticationSession`, with OAuth state and PKCE checks.
The account page runs in the system authentication browser. The app never
receives or stores the account password.

The `network.server` entitlement receives the existing AC OAuth callback at
`127.0.0.1:44233` during sign-in. The listener binds only to loopback, checks
the route and unique OAuth state, then redirects into the authentication
session's `aesel-auth` callback. It closes on completion, cancellation, failure,
or a five-minute timeout. It does not serve files or listen on a LAN interface.
`network.client` separately permits outgoing authentication, inference,
previews, publishing, and the optional user-installed local provider helper.

Settings offers Sign in, Sign out, and Delete account. Deletion identifies the
account and requires a destructive confirmation. The existing AC endpoint
permanently deletes the account and published work across AC apps. The app
reports success only after a confirmed server response, signs out matching
windows, and preserves local notebooks. Failed requests retain the login and
show an error; they are not automatically replayed.

Review steps:

1. Open Settings and choose Sign in to Aesthetic Computer.
2. Complete sign-in in the system authentication session.
3. For deletion testing, use a disposable account, then Settings → Delete account.
4. Confirm deletion; verify the Account deleted dialog and signed-out state.
5. Confirm that the deleted account can no longer sign in.

The physical-Mac sign-in/deletion recording requested by the previous review
must be attached before resubmitting for review. Automated callback and deletion
tests do not replace that recording.

Build using `AESEL_MAS_PROFILE=/path/to/profile bash apple/aesel/build-app-store.sh`.
The script archives the Swift app, verifies both architectures and code signing,
and creates a signed installer package. It never builds or launches Electron.
