# juked

`juked` is Aesthetic Computer's stable, headless Spotify playback boundary for
macOS. JukeWizard talks only to this command contract. The current engine is a
pinned daemon build of `spotify_player`, which embeds `librespot` for playback
and uses Spotify's OAuth Web API for catalog search and control.

Spotify.app is not used. A Spotify Premium account and a one-time browser OAuth
approval are required.

```sh
slab/juked/install.sh
juked authenticate
juked start
juked search "auwbe"
```

The daemon listens on UDP loopback port `24839`; OAuth redirects use `24840`.
Credentials remain mode `0700` under `~/.cache/juked`. The engine is replaceable
or forkable without changing clients as long as the `juked` commands remain
stable. Successful searches are cached by query so a temporary Spotify catalog
outage does not empty JukeWizard's results.
