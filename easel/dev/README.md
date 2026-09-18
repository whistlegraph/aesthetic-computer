# Aesel Dev

`Aesel Dev.app` and `aesel-dev` follow the committed `easel/` tree on monorepo
`origin/main`. `aesel --dev` opens the same app. Ordinary `aesel` opens the signed
release and follows its existing release feed. They use separate bundle IDs,
application data, windows, and update channels. Account credentials remain with
the existing AC/provider login stores.

Both fleet machines check main once a minute. A change outside `easel/` does not
create a dev build. New source is extracted to an immutable directory, native
helpers are compiled, and `current` moves only after staging succeeds. Runtime
changes checkpoint and restart the agent in its existing window. UI changes
also reload the interface. Host changes checkpoint and relaunch the Dev app.
Active requests finish before the checkpoint. Project files are never synced
between machines, and working Git checkouts are never reset.

Settings shows Dev/Release/Local, app version, content-based dev build ID, and update state. Hover the settings status for its Git commit. Identical Aesel source keeps the same build ID even when other projects change.
Piece versions such as v3 are separate. Offline or old checks say they cannot
verify freshness. Local edits inside an installed dev snapshot pause syncing
and show “Local changes”; move them into a source checkout before republishing.
An offline machine catches up when it reconnects. “Lockstep” means the same
committed source tree once both machines are online and have finished their
current requests; it does not claim simultaneous swaps or copy uncommitted work.

Install once on each Mac using matching Electron and production dependencies:

```sh
node easel/dev/install.mjs --repo ~/aesthetic-computer \
  --base ~/Applications/aesel.app --dependencies /path/to/extracted/node_modules
```

The installer creates `~/Applications/Aesel Dev.app`, `~/.local/bin/aesel-dev`,
and the launch agent `computer.aesthetic.aesel-dev-sync`. State, logs, immutable
source snapshots, and the atomic `current` link live in
`~/.local/share/aesel-dev/`. The last three snapshots, any live PTY snapshot, and locally edited snapshots are retained. The native Electron shell is not replaced by source
sync. If Electron or npm dependency requirements change, syncing stops visibly
until this installer is rerun with matching dependencies. No signing credentials
are copied; the development shell is signed locally with an ad-hoc identity.
