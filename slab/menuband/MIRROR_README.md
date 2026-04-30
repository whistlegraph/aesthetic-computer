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

This repository is a **read-only mirror** of `slab/menuband/` from the
[aesthetic.computer monorepo](https://tangled.org/@aesthetic.computer/core).
The monorepo is canonical; this mirror is force-pushed by a `git subtree split`
hook every time the upstream Menu Band code changes.

Force-push only affects `main`. Your forks and feature branches are not
disturbed by upstream sync.

### Submitting changes

Forks + pull requests work normally on GitHub. When the maintainer accepts
your PR, the changes get applied back to the monorepo via `git format-patch`
+ `git am`, preserving your authorship in the commit log. The next mirror
sync after that lands those commits here too — so your hash on the mirror
will eventually match a hash in the upstream monorepo's history.

## Acknowledgements

Performance improvements in 0.5 (visualizer pause-when-hidden, Metal vsync
disable) were contributed by [Esteban Uribe](https://github.com/estebanuribe).
Thanks Esteban!

## License

The code in this repository is part of aesthetic.computer. See the
[upstream LICENSE](https://tangled.org/@aesthetic.computer/core/blob/main/LICENSE)
for terms.
