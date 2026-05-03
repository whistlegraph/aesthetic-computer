# Menu Band

A tiny piano in your macOS menu bar. Click keys with the mouse, or type letters
to play. Sends notes out a virtual MIDI port for your DAW, and ships with a
built-in General MIDI synth so it makes sound on its own.

Free and open source. Part of [aesthetic.computer](https://aesthetic.computer).

- **Download (notarized DMG):** <https://aesthetic.computer/menuband>
- **Landing page + changelog:** <https://aesthetic.computer/menuband>
- **Issue tracker / pull requests:** here on GitHub

## Building from source

```sh
swift build -c release
./install.sh        # builds, signs, installs ~/Applications/Menu Band.app,
                    # and loads the launchd agent so it starts on login
```

`install.sh` will use a Developer ID Application certificate from your keychain
if one is available, or generate a self-signed identity for local testing.

## Mirror notice

This repository is a **snapshot mirror** of `slab/menuband/` from the
[aesthetic.computer monorepo](https://tangled.org/@aesthetic.computer/core).
The monorepo is canonical; the maintainer pushes new "Mirror of `<hash>`"
commits onto `main` whenever upstream code changes.

Your forks and feature branches are not disturbed by upstream sync — only
`main` advances.

### Submitting changes

Forks + pull requests work normally on GitHub. When the maintainer accepts
your PR:

1. The maintainer runs `slab/menuband/bin/mirror-pull.sh` from the monorepo.
   That script walks every contributor commit on `main` since the last
   "Mirror of …" snapshot and applies them via
   `git format-patch | git am --3way --directory=slab/menuband/`,
   preserving your authorship.
2. Then `slab/menuband/bin/mirror-sync.sh` snapshots back to `main`. The
   sync script **refuses to push** if any contributor commit on the mirror
   hasn't been landed in the monorepo first — so your work can't get
   silently regressed by an out-of-order sync.

The mirror's commit hash for your work won't match the monorepo's (git am
rewrites hashes), but the author and subject do. After the next sync, the
mirror's tip tree will contain your changes.

## Acknowledgements

Performance improvements in 0.5 (visualizer pause-when-hidden, Metal vsync
disable) were contributed by [Esteban Uribe](https://github.com/estebanuribe).
Thanks Esteban!

## License

The code in this repository is part of aesthetic.computer. See the
[upstream LICENSE](https://tangled.org/@aesthetic.computer/core/blob/main/LICENSE)
for terms.
