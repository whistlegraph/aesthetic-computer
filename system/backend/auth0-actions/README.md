# Auth0 Actions

Source of record for Actions installed in the `aesthetic` tenant. Actions live
in Auth0, not in this repo — these files are the reviewable copy, the way
[`lith/mirror`](../../../lith/mirror) holds units that run on lith.

## `link-email-identity.js` — post-login

An emailed code signs you into the account you already have.

### The problem it solves

Auth0 treats every connection as a different person. Every Aesthetic Computer
account is on `Username-Password-Authentication` — of the first hundred users
in the tenant, ninety-nine are on it and **not one has a second identity**.
oskiewar's native sign-in (a mailed code, no redirect, no Turnstile) uses the
`email` passwordless connection, which mints a separate user.

Measured on 2026-09-11, not assumed: signing in as `mail@aesthetic.computer`
produced `email|6aa4572f21241b122b90e4d3`, an Auth0 user one minute old, and

```
GET https://aesthetic.computer/handle?for=email|6aa4572f21241b122b90e4d3
→ 404 {"message":"No handle(s) found."}
```

Without this Action, every returning player arrives as a stranger and is asked
to claim a handle they already own — and cannot, because it is taken, by them.

### Install

1. **Auth0 Dashboard → Actions → Library → Build Custom**, name it
   `link-email-identity`, trigger **Login / Post Login**.
2. Paste the contents of [`link-email-identity.js`](./link-email-identity.js).
3. **Dependencies** → add `auth0` (latest).
4. **Secrets** → add three, from `vault/lith/.env` (NOT the devcontainer copy,
   which is stale — see the gotcha below):
   - `AUTH0_DOMAIN` = `aesthetic.us.auth0.com`
   - `AUTH0_M2M_CLIENT_ID` = the value of `AUTH0_M2M_CLIENT_ID`
   - `AUTH0_M2M_SECRET` = the value of `AUTH0_M2M_SECRET`
5. **Deploy**, then **Actions → Triggers → post-login** and drag it into the
   flow.

No new Auth0 scopes are needed. That M2M client already carries
`read:users update:users delete:users read:stats`, and linking wants only the
first two.

### Testing it

The tenant had no account on `mail@aesthetic.computer` before this work, so
that address cannot exercise the link — it has nothing to link *to*. Use an
address that already has an Aesthetic Computer account:

```bash
# 1. mail a code
curl -s -X POST https://hi.aesthetic.computer/passwordless/start \
  -H 'content-type: application/json' -H 'origin: https://oskiewar.com' \
  -d '{"client_id":"<oskiewar client>","connection":"email",
       "email":"<an address with an existing account>","send":"code"}'

# 2. read it, then spend it — see the flow in
#    system/public/aesthetic.computer/lib/auth0-otp.mjs
```

It worked when the `sub` in the returned `id_token` is the **existing**
`auth0|…` subject rather than a fresh `email|…` one, and `GET /handle?for=<sub>`
answers with the handle instead of 404.

### It only ever touches a brand new identity

Linking makes the secondary user **cease to exist as a subject**, and Aesthetic
Computer keys by subject — `@handles` is `findOne({_id: sub})`, and it is not
the only place. So a passwordless identity that already has history cannot be
merged here; that needs an AC-side re-key first, which is separate work.

This is not hypothetical. `me@jas.life` has two unlinked Auth0 users:

```
auth0|63effeeb…  Username-Password-Authentication, 783 logins  → @jeffrey
email|63ed8722…  email passwordless,                 35 logins  → @jeffrey
```

Both own a `@handles` row. Merging them would have silently orphaned the
second one's history. The Action therefore declines anything with
`logins_count > 1` — a brand new identity has nothing to lose, and that is the
only case it is allowed to touch.

An account already split this way keeps working exactly as it does today: two
subjects, both resolving to the same handle. Unifying them is a migration, not
a login-time decision.

### The security rule, and why it is not optional

Linking on an unverified address is account takeover: anyone who controls a
mailbox could be merged into an account that merely *claimed* that address.
The mailed code settles the passwordless side. The existing account only counts
if Auth0 has already verified it, so the Action requires `email_verified` on
both and otherwise does nothing. A stranger with no handle is recoverable; a
stranger inside somebody else's account is not.

### Gotcha: two copies of the M2M credentials, one stale

`vault/.devcontainer/envs/devcontainer.env` and `vault/lith/.env` both define
`AUTH0_M2M_CLIENT_ID` / `AUTH0_M2M_SECRET`, they share the first six
characters, and **only lith's works**. The devcontainer copy answers

```
{"error":"access_denied","error_description":"Unauthorized"}
```

which is the same thing Auth0 says for a client that does not exist, for a
wrong secret, and for a client with no grant on the Management API — so it
reads like a dead client or a billing problem and is neither. Take these from
`vault/lith/.env`, which is what production uses.
