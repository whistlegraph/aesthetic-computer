# Auth0 Forms — "Pick your handle"

A new account picks its @handle on the hosted sign-in page
(`hi.aesthetic.computer`), right after it is created, and comes back to AC
already named. Source of record for three things that live in Auth0:

- `flow.json` — the flow. It POSTs the name to `https://aesthetic.computer/handle`
  with the signup secret and shows AC's reply when the name is refused.
- `form.json` — the form: one field, one button, then the flow.
- `ask-handle.js` — the post-login Action that shows the form when AC says the
  account has no handle.

AC's side is `backend/signup-handle.mjs`, wired into `netlify/functions/handle.mjs`.

## One handle system

`@handles` in MongoDB stays the only truth. The form writes through `/handle`,
the same code a bearer token reaches, so the allowed characters, the length,
the word filter, quarantine after deletion and holds are AC's and are not
restated in Auth0. Apps that read handles from AC (Choir and the rest) see a
handle made here like any other.

Before it asks, the Action asks AC whether the account already has a handle:
its own, its sotce sister's (matched by address, as `handleFor` always has), or
one on an older account with the same verified address — the case where
`link-email-identity` folds a code or social sign-in into an existing account.
Bind `ask-handle` after `link-email-identity`.

The sotce tenant is separate and has no form. A sotce account still gets its
handle on sotce.net, and a person with a sotce handle who joins AC is not asked.

## Unverified addresses

A password signup has not verified its address when it picks a handle. The
handle is real at once and marked `unverifiedAt`. If the account is still
unverified a week later, the name goes to the next person who asks for it
(`releaseAbandoned`). Google, Apple and emailed-code sign-ins arrive verified.

## Install

```sh
auth0 login                      # approve in the browser, on the right tenant
node system/backend/auth0-forms/install.mjs --only '^mail\+signuptest'   # test addresses only
node system/backend/auth0-forms/install.mjs --only '^mail\+signuptest' --bind
npm run test:onboarding:e2e
node system/backend/auth0-forms/install.mjs --bind                       # everyone
```

Unbound, nobody is asked. To switch it off, remove `ask-handle` from the
post-login flow in the dashboard (Actions → Triggers → post-login).

## It must never cost a login

The Action skips the form whenever AC cannot be reached or answers oddly, on
token exchanges and silent refreshes (no page to show a form on), and when its
secrets are missing. A person who slips through without a handle is asked by AC.

Native sign-ins that never open the hosted page (oskiewar's mailed code) do not
see the form.
