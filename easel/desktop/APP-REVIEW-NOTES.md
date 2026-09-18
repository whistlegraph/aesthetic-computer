# aesel — macOS review notes

## Guideline 2.4.5: incoming network entitlement

`com.apple.security.network.server` is required for account sign-in. aesel
uses Authorization Code with PKCE and temporarily starts an HTTP listener
bound exclusively to `127.0.0.1:44233`. After authentication, the user's browser
connects to `http://localhost:44233/callback` to return the authorization code.
This is an incoming connection to the app, even though it stays on the Mac.
The callback verifies the OAuth state, and the code exchange uses the PKCE
verifier. It does not serve workspace files or accept LAN connections.

The listener is started only by sign-in and stops accepting connections after
the callback, before the outgoing token exchange. It also closes on error or
after the five-minute sign-in timeout. Ordinary startup does not open it.

To exercise this functionality:

1. Launch aesel and click the account label in the header, or enter `/login`.
2. Complete Aesthetic Computer sign-in in the browser.
3. The browser returns to the local callback and displays “Signed in”. Return
   to aesel; its header shows the account handle (or the handle-claim flow).
4. If already signed in, use `/logout` before repeating `/login`.

`com.apple.security.network.client` is separately required for outgoing HTTPS
requests for authentication, hosted inference, previews, and publishing.

Source: `easel/src/ac-session.mjs`, `ACSession.login()`.

The entitlement explanation and reproduction steps were saved to App Review
Information on September 17, 2026. No review reply has been sent and the app
has not been resubmitted. The rebuilt candidate is version 0.7.1, build 0.7.3.
