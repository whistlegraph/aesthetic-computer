# Connect Bluesky prototype

A working AT Protocol OAuth flow with a separate confirmation before linking
the verified DID. The local runner uses a disposable test account. It neither
changes an AC account nor signs someone into AC.

```sh
cd at/login-prototype
npm ci --ignore-scripts
npm start
# Open http://127.0.0.1:4287
npm test
```

On 2026-10-08, the real Bluesky flow for `aesthetic.computer` returned
`did:plc:k3k3wknzkcnekbnyde4dbatz`; explicit confirmation connected it to the
test account. The browser consent screen requested identity access only.
Seventeen tests cover account/browser binding, freshness, expiry, replay,
conflicting links, CSRF, host checks, OAuth callback handling, and discovery
restrictions. The live browser test also exercised form submission and the
cross-origin authorization redirect.

`oauth.mjs` uses the official `@atproto/oauth-client-node` SDK for PKCE, PAR,
DPoP, issuer validation, and DID verification. Scope is only `atproto`.
Authorization and session secrets stay in bounded, expiring memory stores;
the OAuth grant is revoked after identity proof and its local session discarded.
Nothing is written to MongoDB, Auth0, `users.atproto`, or a credential file.

`link-flow.mjs` expects a trusted server-side principal. The HTTP runner
substitutes a five-minute local test principal, binds the process to
`127.0.0.1`, and rejects other Host headers. Two test accounts cannot claim the
same DID, and an existing connection cannot be silently replaced. Links and
OAuth state disappear when the process stops. Disconnect affects only the
test mapping.

Discovery is intentionally limited to Bluesky and the AC PDS. Arbitrary PDS
hosting needs DNS/IP validation and safe redirect handling before expanding
the allowlist. This runner is for a single local process and must not be
exposed by a public tunnel or deployed as an authentication service.

## Production integration still needed

- Replace the test principal with a freshly reauthenticated Auth0 session,
  verified server-side with its canonical subject, browser binding, and
  current lock/deletion status. Recheck these at confirmation.
- Persist DID bindings with unique indexes on provider/DID and account/provider;
  use an atomic operation for conflicts. Keep them separate from the AC-owned
  publishing identity in `users.atproto`.
- Add encrypted, expiring OAuth state and distributed session locks if tokens
  will be retained; use HTTPS metadata, a confidential client key, and an
  audited discovery policy.
- Wire Connect/Disconnect into account settings, including account deletion
  and recovery. Standalone **Continue with Bluesky** then needs the Auth0
  adapter described in [the assessment](../../plans/atproto-integration-plan.md).
  Email or handle equality must never authorize an account merge.

Sources: [official SDK](https://github.com/bluesky-social/atproto/tree/main/packages/oauth/oauth-client-node),
[AT Protocol OAuth specification](https://atproto.com/specs/oauth).
