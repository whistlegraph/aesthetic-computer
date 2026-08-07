# Klokkentales

Short storybook dispatches from the public `laer-klokken` / `clock` chat.

The source is community conversation, not a script to read verbatim. Each
episode is selected, abridged, attributed where a person or work matters, and
reviewed before synthesis. Private contact details, addresses, health details,
and logistics that no longer help the story are excluded.

## Cast

- **Jeffrey** carries dates, places, and the factual thread through the existing
  consented `jeffrey-pvc` voice.
- **Prutti** interrupts and turns the record into folklore. The first production
  uses a consented Instant Voice Clone created through our ElevenLabs API
  account. Until then, `--placeholder-prutti` uses a generic premade voice and
  marks the output as a casting draft. A later PVC still requires Prutti's
  ElevenLabs verification.

Every public episode and show note must state that it contains synthetic voices.
Never present generated speech as a live recording.

## Episode flow

```text
public clock chat -> dated source snapshot -> human story edit
                  -> two-voice synthesis -> listen + speech QA
                  -> private Buzzsprout stage -> approval -> publish
                  -> assets.aesthetic.computer/klokkentales/index.json
                  -> aesthetic.computer/klokkentales
```

The AC piece reads the public catalog directly. Buzzsprout is the directory and
RSS distribution lane; it does not replace the AC player.

## Commands

```sh
cd marketing/klokkentales

# Fetch a review-only snapshot. `out/` is ignored.
node bin/fetch-chat.mjs --since 2026-06-01 --until 2026-08-08 \
  --out out/summer-so-far-2026.source.json

# Validate and inspect the episode without spending synthesis credits.
node bin/produce.mjs summer-so-far-2026 --dry-run

# Inspect account, approved source, and voice state.
node bin/voice.mjs status

# Cut an approved clean source into the private vault, then listen to it fully.
node bin/voice.mjs prepare /path/to/approved-prutti-audio.mp3 \
  --start 0 --duration 120

# Create the IVC on our account. This flag is a declaration that Prutti approved
# the clone and that every recording supplied is authorized for this use.
node bin/voice.mjs create-ivc --confirm-rights-and-consent
node bin/voice.mjs sample

# Casting draft: Jeffrey PVC + a generic, explicitly non-Prutti voice.
node bin/produce.mjs summer-so-far-2026 --placeholder-prutti

# Final review render. Credentials and the Prutti voice ID load from the vault.
node bin/produce.mjs summer-so-far-2026

# Build the AC catalog and podcast RSS.
node bin/feed.mjs

# Review first; public release always requires the explicit second command.
node bin/buzzsprout.mjs summer-so-far-2026 --private
node bin/buzzsprout.mjs publish summer-so-far-2026
```

## Release gates

1. Confirm the date window and evidence snapshot.
2. Read the script for attribution, safety, and expired logistics.
3. Listen to the entire mastered MP3; inspect the SRT and synthetic-voice label.
4. Obtain Prutti's approval of both his clone and performance.
5. Stage privately on the dedicated Klokkentales Buzzsprout show.
6. Publish only after both narrators approve.
