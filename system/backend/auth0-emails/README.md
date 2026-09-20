# Auth0 passwordless email

Applied to the `aesthetic` production tenant on 2026-09-13 through the Auth0
dashboard. Reloaded and verified the saved sender, subject, and exact body.

Matches the existing signup email recovered from mail@aesthetic.computer's
Sent Mail on 2026-09-13: lowercase text, default browser typography, two line
breaks, and an aesthetic.computer link. No logo, layout tables, or welcome copy.

In the `aesthetic` tenant, open **Authentication → Passwordless → Email**:

- **From:** `aesthetic.computer <mail@aesthetic.computer>`
- **Subject:** `verify your email 💌`
- **Message:** contents of [passwordless.html](passwordless.html)

The previous sender, subject, body, and relevant connection settings are saved in
[passwordless.before-2026-09-13.json](passwordless.before-2026-09-13.json) for
rollback. OTP expiry (180 seconds), length (6), signup and domain flags, and
authentication parameters were verified unchanged after reload. The template handles codes,
magic links, and email-change notifications. It deliberately makes no claim
about code or link expiry.

This is minimal HTML matching the signup email, not a request to change the
message's MIME type. The existing signup message has both plain-text and HTML
parts. Delivery of the replacement has not yet been tested. Inspect the next
requested code email in the mailbox to
verify sender, subject, both MIME parts, and absence of Auth0 branding. Requesting
a test email requires an explicitly authorized recipient.

Auth0 configures passwordless messages separately from its regular email
templates: [Passwordless Authentication with Email](https://auth0.com/docs/authenticate/passwordless/authentication-methods/email-otp).

Access checked on 2026-09-13: the production M2M credential lacks
`read:email_templates`, `read:connections`, and `read:email_provider`.
The authenticated Chrome dashboard was used to apply this change.
