# Slab terminal seed

Canonical Terminal.app appearance for any machine running Slab. This is the
single source of truth for the look — capture it once on a reference machine,
commit it, and `slab-seed-terminal` reproduces it byte-for-byte on every new
machine.

## What's here

- `terminal-profiles.plist` — the `Grass` base profile plus every
  `Slab-<state>-<dark|light>[-pulse]` settings set (the named profiles the Slab
  menubar switches between per Claude Code session status). Colors + font are
  captured verbatim.
- `terminal-profiles.list` — plain-text manifest of the profile names in the
  plist (the installer reads names from here, not by parsing the binary plist).
- `fonts/` — Monaspace Argon (Regular/Bold/Italic/BoldItalic/Medium/Light), the
  font the profiles reference. Installed into `~/Library/Fonts`.

## Install / redeploy on a machine

```bash
slab-seed-terminal            # merge profiles + set Grass as default & startup
slab-seed-terminal --no-default   # profiles only, leave default profile alone
slab-seed-terminal --seed PATH    # use an alternate seed plist
```

`install.sh` calls this automatically. Quit Terminal first (the script does this
for you) so its in-memory copy doesn't clobber the plist edits on exit; relaunch
Terminal afterward.

## Re-capture the seed (when you tweak the palette on the reference machine)

The Slab menubar can create/adjust the `Slab-*` sets live. Once they look right,
re-export them into this seed:

```bash
node ../bin/slab-capture-terminal.mjs   # or the python snippet in that file's header
```

This rewrites `terminal-profiles.plist` + `.list` from the current machine's
`com.apple.Terminal` preferences (Grass + all `Slab-*` sets). Commit the result.
