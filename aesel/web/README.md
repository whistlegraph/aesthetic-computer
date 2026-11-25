# Aesel in a browser

The illustrated guide is `aesel.app` itself (`/about/` redirects there). The account desk lives at `aesel.app/try/` and also opens from `try.aesel.app`.

## Dash

`aesel.app/dash/` shows what Aesel keeps for a signed-in account. It lists
uploaded conversations, the artifacts recorded in them, braincells, drafts in
this browser and published pieces. People can read or delete each conversation
there, or all of them. It is plain static files in `system/public/aesel/dash/`,
so a normal lith deploy ships it with no build step. It reads
`easel-transcripts?scope=mine` (with `view=summary` for counts and
artifacts), `easel-credits` and the public `media-collection`. Sign-in uses the
same Auth0 client and needs `https://aesel.app/dash/` (and
`http://localhost:8771/dash/` for local use) in its callback and logout URLs. All sign-ins and browser drafts use the original apex origin.

The `/try/` client uses `phone/session.mjs` and the shared AC engine. The first
release makes JavaScript pieces using hosted AC braincells, publishes edits to
the signed-in handle and embeds the public piece. It does not run a local CLI.

```sh
npm ci --prefix aesel/web
node aesel/web/build.mjs
node aesel/web/serve.mjs
# http://localhost:8771/try/
```

The build writes only to `system/public/aesel/try/` (or a supplied output
directory). It bundles explicit browser shims and guides; it never serves the
repository or exposes a development token endpoint. Its only build dependency
is pinned esbuild. Build outputs are ignored by git.

Auth0 uses the existing AC SPA client, `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`,
at `hi.aesthetic.computer`, with the existing vendored Auth0 SPA SDK and PKCE.
Append these settings without replacing other AC URLs:

| Setting | Production | Local |
| --- | --- | --- |
| Allowed Callback URLs | `https://aesel.app/try/` | `http://localhost:8771/try/` |
| Allowed Logout URLs | `https://aesel.app/try/` | `http://localhost:8771/try/` |
| Allowed Web Origins | `https://aesel.app` | `http://localhost:8771` |
| Allowed Origins (CORS) | `https://aesel.app` | `http://localhost:8771` |

Auth0 owns token storage and refresh. Engine credentials stay in memory. Drafts,
conversation context and the last-opened piece are stored separately per Auth0
subject in this browser's localStorage. They survive sign-out but are not synced
between computers. The account archive reads previously uploaded terminal journals through `easel-transcripts?scope=mine`. Ownership comes from the verified token. Selecting a journal opens it for reading. Download source before clearing browser data. Model output
is rendered as text; preview code runs on the separate AC origin in a sandboxed
iframe. The gate discloses public publishing and existing staff access.

After committing and pushing to main, run `bash aesel/web/deploy.sh`. It builds
from that pushed commit on lith, then atomically switches the `/try/` symlink
to the new release. It does not deploy other working-tree changes or restart
the monolith. The existing aesel.app Caddy site serves the result. Prior releases
remain in `/opt/ac/.aesel-web-releases/`; `try/build.json` identifies the revision.
A regular monolith deployment does not rebuild this client; run this script
again when its shared engine dependencies change.

Validation:

```sh
node --test aesel/web/storage.test.mjs aesel/test/phone-session.test.mjs aesel/test/phone-credits.test.mjs
node aesel/web/browser.test.mjs
```

The browser test needs the local server and installed Chrome. It uses the real
bundle and agent loop with mocked authentication, inference and uploads. It
checks saved conversations, two tool rounds, publication and preview wiring,
account isolation, escaping, logout and mobile layout. It does not prove live
Auth0, live inference or rendering on older browsers. Browser build targets:
Chrome 90, Firefox 91 and Safari 15.6; actual oldest-device support still needs
device testing, including the AC runtime inside the preview.

Only pieces are supported initially. Native preview capture, local files, other
media and vendor CLI subscriptions remain desktop capabilities.
