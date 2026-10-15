# AC Auth0 sign-in

Live source for the `aesthetic` tenant at `hi.aesthetic.computer`.

`login.css` is the style source. `template.html` embeds it, so login never depends on a development server. `settings.json` records the theme, text, identifier-first profile and password/email-code switch forms. `before-2026-10-15.json` contains the previous settings for rollback; no custom page template or switch forms existed before this release.

For a live preview, open a fresh sign-in through AC (so Auth0 owns the transaction), find that exact tab with `puppet list`, then run:

```sh
node system/backend/auth0-branding/watch.mjs --machine neo --target <page-id>
```

Editing `login.css` updates only that Auth0 tab and rebuilds `template.html`. Reloading discards unsaved previews. The watcher rejects other origins, including Stripe and the admin dashboard. `--once` applies one preview; `--puppet <path>` selects a local Puppet CLI checkout.

Publishing uses Auth0's Management API, or the same-origin `/api/` proxy in an authenticated dashboard. The production backend credential is deliberately limited to user records and cannot publish branding. Never export dashboard credentials into this repo.

Apply the template with `PUT branding/templates/universal-login` and JSON `{ "template": "<template.html contents>" }`. Apply the theme with `PATCH branding/themes/<themeId>` (omit `themeId` from the body). Patch `prompts` with the profile; put each entry in `text` at `prompts/<name>/custom-text/en`, and each entry in `partials` at `prompts/<name>/partials`. Read the saved values back and inspect fresh desktop and phone transactions. Keep the Auth0 widget, validation, focus states and all authentication controls intact.

The No Paint callback `https://aesthetic.computer/nopaint.art/ai/signin.html` was appended to the existing AC application's callback list. Existing callbacks are preserved. No Paint uses the Auth0 SDK's state/PKCE verification, retains the scanned pairing in session storage, and only connects the painting after authentication. Payment returns retain their checkout ID through reauthentication.

Verified with real hosted identifier/password screens without sending a code, and isolated browser fixtures for callback success, invalid state, account creation, QR pairing, checkout reauthentication and wallet reconciliation. Delivery of a real email code and a physical Apple Pay purchase require a separate user test.

Sources: [page templates](https://auth0.com/docs/customize/login-pages/universal-login/customize-templates), [email/password switching](https://auth0.com/docs/customize/login-pages/universal-login/customize-signup-and-login-prompts/connection-switching).
