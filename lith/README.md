# lith

Secrets and runtime env for the Aesthetic Computer monolith deploy.

`lith/deploy.fish` expects:
- `aesthetic-computer-vault/lith/.env`

That file is uploaded to:
- `/opt/ac/system/.env`

Why `system/.env` on the server:
- [`lith.service`](/workspaces/aesthetic-computer/lith/lith.service) uses `EnvironmentFile=/opt/ac/system/.env`
- The monolith serves the main site and API from the shared `system/` tree

Minimum required keys:
- `NODE_ENV=production`
- `CONTEXT=production`
- `DEPLOY_SECRET=...`

Recommended workflow:
1. Copy `.env.example` to `.env`
2. Fill in the real production values
3. Re-run `fish vault-tool.fish status` to confirm `lith/.env` is tracked
4. Deploy with `fish /workspaces/aesthetic-computer/lith/deploy.fish`

## GM SoundFont bake

`lith/scripts/gm-bake.mjs` is a one-shot baker that renders a FOSS SoundFont
(default: [GeneralUser GS v1.471](https://schristiancollins.com/generaluser.php))
into per-instrument MP3 packs covering the full GM Level 1 bank — analogous to
how BDF fonts get hosted on `assets.aesthetic.computer`. Requires `fluidsynth`
and `ffmpeg` on PATH (`brew install fluid-synth ffmpeg` on macOS,
`apt install fluidsynth ffmpeg` on Linux). Run it with
`node lith/scripts/gm-bake.mjs` — the SF2 is downloaded into `lith/cache/` and
output lands at `lith/scripts/out/gm/<NNN>/<NoteName>.mp3` plus
`drum-000/<midi>.mp3` for the Standard Kit, with a `manifest.json` and
`LICENSE.txt` alongside. The script is re-runnable (skips existing MP3s),
defaults to every-third-semitone over A0..C8 (override with `--note-step=N`,
`--only=0,1,24`, `--skip-drums`, `--out=...`, or `--dry-run`). When the bake
finishes, publish with `npm run assets:sync:up` to push the tree to
`assets.aesthetic.computer/gm/`.
