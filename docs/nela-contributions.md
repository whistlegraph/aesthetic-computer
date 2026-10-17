# NELA contributions

Code by [Jeffrey](https://prompt.ac/@jeffrey) for
[NELA Computer Club](https://nela.computer), developed in
[Aesthetic Computer](https://aesthetic.computer).

[NELA contribution repository](https://git.nela.computer/jeffrey/nela-contributions) ·
[Aesthetic Computer on Tangled](https://tangled.org/aesthetic.computer/core)

| Work | Source | State |
| --- | --- | --- |
| Login skin | [nela-authentik](../nela-authentik/) | Live Authentik custom CSS and light/dark brand assets, recovered October 16, 2026. |
| Outline skin | [nela-outline](../nela-outline/) | Deployed October 3 on Outline v1.9.2. NELA is the default; Outline remains selectable. Also includes the patch for main at `c775aaf43c`. |
| Signup | [signup prototype](../sites/nela-signup-prototype/) | Simulated verification, consent, and checkout; no accounts or payments are created. |
| Guest BOOk | [Reset In Peace guestbook](../experiments/nela-reset-guestbook/) | Browser addition and Node reference server; not deployed. Original page and art attributed to NELA / Echo. |
| Nelamesh Scope | [macOS source](../slab/nelamesh-scope/) | Local service discovery prototype; does not expose or share services. |
| Donation page | [page](../system/public/nela.aesthetic.computer/) and [login function](../system/netlify/functions/nela-signin.mjs) | Legacy work. Its Signal OAuth issuer has been retired; authentication needs replacement before reuse. |

The contribution repository lives under `jeffrey` on NELA's official forge.
The organization's [auth repository](https://git.nela.computer/ncc/auth) is
currently empty, and Jeffrey has read access only. Moving contributions into
`ncc` requires an organization owner to grant access or transfer the repository.

## Contributing across repositories

Use a separate checkout of the NELA contribution repository, with
`https://git.nela.computer/jeffrey/nela-contributions.git` as `origin`.
Bring the relevant code into a contribution branch, keep its tests and
attribution, and record the source commit from Tangled in the commit message.
Fetch NELA's changes before each update; review the diff and push that branch for
integration. Keep changes made on either side when bringing work back to AC.

```sh
git clone https://git.nela.computer/jeffrey/nela-contributions.git
cd nela-contributions
git switch -c my-change
# Edit the relevant project, then run its checks.
git add <changed-files>
git commit
git push -u origin my-change
```

HTTPS Git uses a Forgejo access token. Store it in a credential helper; never put
it in a remote URL or a source file. SSH is advertised on port 2222 but was not
reachable during setup.

For Outline, apply the patch matching NELA's installed version with `git am`;
the [skin README](../nela-outline/README.md) names both supported bases.

Keep the AC monorepo's existing Tangled remote. NELA receives the relevant
project files, not a mirror of AC's entire history. Do not export local data,
recordings, message receipts, hosting metadata, credentials, or seat policies.

## Checks

From the AC repository root, with Node 22 or later:

```sh
node --test sites/nela-signup-prototype/tests/flow.test.mjs \
  experiments/nela-reset-guestbook/server.test.mjs
```

Run the signup prototype with a static server rooted at its `dist/` directory.
Run the guestbook with `npm start` from its directory. Nelamesh Scope uses
`swift run` on macOS 13 or later; Outline's patches include tests for its theme.
