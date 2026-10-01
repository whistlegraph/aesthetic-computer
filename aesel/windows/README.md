# Aesel for Windows

Windows 10/11 x64 beta. WPF + WebView2 hosts a bundled copy of Aesel's shared
JavaScript session and browser interface: AC account sign-in, hosted generation,
local notebooks, automatic publishing, source download, and piece preview.
This release does not include the Mac interface, terminal app, Claude/Codex CLI
providers, or automatic updates. It is unsigned; Windows may show an unknown
publisher or SmartScreen notice. No security setting needs to be disabled.

Sign-in opens the system browser using AC's existing PKCE registration and
`http://localhost:44233/callback`. The receiver binds only to 127.0.0.1, bounds
request size and duration, and rejects mismatched/duplicate state parameters.
Refresh credentials stay in a Windows CurrentUser DPAPI envelope. The web view
receives access tokens through a bridge restricted to the bundled top document;
it has no filesystem or process host objects. External HTTPS links open in the
browser. One app process owns credential rotation.

Data lives under `%LOCALAPPDATA%\Aesthetic Computer\Aesel`. Notebooks are
separated by account and survive updates and uninstall. Sign out removes saved
credentials. Delete that directory separately to erase all local notebooks and
credentials. There is no cross-device notebook sync. Hosted inference and
publication use AC's network services. A launch sends the existing anonymous
`app-open` schema (`app`, version, Windows platform, random install UUID, first
launch flag); create an empty `disable-launch-ping` file in the data directory
to disable it. No account identity is included in that ping.

Build on Windows with Node 24, .NET 10 SDK, Microsoft WebView2 Runtime, and Inno
Setup 6 installed:

```powershell
npm ci --prefix aesel/web
node aesel/windows/prepare.mjs
pwsh -File aesel/windows/package.ps1
```

`package.ps1` publishes the self-contained x64 app, runs it on Windows with an
isolated profile and mocked account/inference/upload traffic, captures the
workspace/sign-in screens, then builds the per-user installer and SHA-256 feed.
The installer includes Microsoft's signature-verified WebView2 bootstrapper and
installs the runtime if needed. The installed app is tested again after setup.
The smoke checks generation, upload, preview, escaping, persistence, thread
switching, sign-out, account isolation, callback boundaries, Windows DPAPI,
and serialized refresh-token rotation.
It does not certify real-user OAuth or provider billing.

The existing AppVeyor project builds the `aesel-windows` branch using its
separate branch configuration. GitHub's manual workflow is also available when
the account billing lock is cleared. Inspect both screenshots and the test
result before publication; never publish a compile-only artifact.

Release artifacts go under `releases.aesthetic.computer/aesel/windows/`.
Run `node aesel/windows/publish.mjs ARTIFACT_DIR` with the existing Spaces
credentials in the environment. When running this script from a staged copy,
set `AESEL_SOURCE_REPO` to the canonical checkout used for ancestry checks.
It uploads the immutable installer first and `latest.json` last. The manifest records
the build revision, hash, architecture, beta channel, and unsigned status.
Only a revision preserved on knot `main` may be published. The website links
through `/api/download?app=aesel&file=aesel-VERSION-windows-x64-setup.exe` so
Windows downloads use the existing AC download reporting.
