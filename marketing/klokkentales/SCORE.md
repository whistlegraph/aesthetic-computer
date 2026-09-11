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

# Pruttivox: one-off read-aloud of a community text in the Prutti IVC.
node bin/pruttivox.mjs "teksten her" --from @snakes --publish

# A long text comes from a file and is spoken in stitched pieces.
node bin/pruttivox.mjs --file interview.txt --slug interview --from @prutti
```

## Pruttivox

Community members send a text; Prutti's clone reads it. Every clip ends with a
spoken "Pruttivox. Syntetisk stemme." tag, and each render logs its text and
requester to the vault. Listen to the whole clip before sharing. Never render a
text that has the voice make real-world commitments — payments, meetings,
endorsements, claims about other people; the clone reads performances, it does
not speak for Prutti. Clips are shared in the clock channel where Prutti
participates and can veto any clip.

The chat lane (`/api/pruttivox` + the "vox" chip in chat/laklok) reads only
messages @prutti himself typed — the text comes from the database by message
id, never from the caller — and caches each render on the CDN with word
timings for the karaoke highlight.

The same chip, in blue, plays a sound *linked* in any message (mp3, wav, ogg,
m4a, webm — the formats `net.preload` decodes). That one renders nothing and
speaks for nobody; it just plays the file at the url. A message carrying a
link plays the link, not its words. This is the working lane for linked audio:
`sfx <url>` from the prompt still kills the renderer on a long remote file
(reproduced three times on a 174-second mp3, cause not yet found), so send
people to the chip, not to the piece.

## Release gates

1. Confirm the date window and evidence snapshot.
2. Read the script for attribution, safety, and expired logistics.
3. Listen to the entire mastered MP3; inspect the SRT and synthetic-voice label.
4. Obtain Prutti's approval of both his clone and performance.
5. Stage privately on the dedicated Klokkentales Buzzsprout show.
6. Publish only after both narrators approve.
