# Prutti voice source audit

The first production can create a Prutti Instant Voice Clone through our
ElevenLabs API account after he explicitly approves the clone and the supplied
recordings. Public recordings are only candidates for review; publication is
not a license grant. A later Professional Voice Clone still requires Prutti's
ElevenLabs verification.

## Best starting source

**The Official Mort Aux Vaches Ekstra Extra Walkthrough** — 1:21:10, published
by Goodiepal / Gæoudjiparl on his own SoundCloud account.

- Source: https://soundcloud.com/goodiepal-gaeoudjiparl/the-official-mort-aux-vaches-ekstra-extra-walkthrough
- Strength: sustained solo English explanation in a characteristic narrative
  register; a transcript exists.
- Work needed: listen end to end, remove music/noise, and retain only clean
  single-speaker passages. Do not assume all 81 minutes are training-ready.

## Supplemental candidates

The Danish Composers' Society's Goodiepal page indexes the Lær Klokken archive
and identifies the UbuWeb audio replies:
https://komponistbasen.dk/node/8812

| File | Length | Editorial note |
| --- | ---: | --- |
| `goodiepal_audio_svar_25.mp3` | 25:08 | Travel-disco episode; likely Prutti-led, but contains archival/phone material. |
| `goodiepal_audio_svar_49.mp3` | 21:13 | Night-train episode; useful Danish register if clean solo passages dominate. |
| `goodiepal_audio_svar_68.mp3` | 1:12:23 | Carl Georg Rasmussen conversation; requires speaker separation. |
| `goodiepal_audio_svar_18.mp3` | 50:21 | Jørgen Rud/Ragnhild May episode; multiple voices and performance audio. |
| `goodiepal_audio_svar_26.mp3` | 5:34 | Audio postcard; too short alone, potentially useful as supplemental color. |
| `goodiepal_audio_svar_77.mp3` | 5:30:40 | Jan Sneum interview; mostly unsuitable unless Prutti's interviewer turns are isolated. |

The 2017 documentary *The Goodiepal Equation* is also long, but its location
sound, edits, music, and other speakers make it a secondary source rather than
the dataset backbone: https://www.youtube.com/watch?v=0DLTtclY-U4

## Dataset target

For the first IVC, isolate 60–180 seconds of clean, single-speaker material and
listen to the entire cut before upload. More archival audio does not improve an
IVC once the short reference is representative and clean.

The first consented IVC uses 1:00–3:00 of the official walkthrough. The
two-minute cut contains 404 transcribed words from one diarized speaker, no
tagged music or other audio events, no clipping, and no silence longer than a
sentence pause. The full source and prepared cut live only in the private vault.

ElevenLabs recommends at least 30 minutes of clean speech and preferably two to
three hours for a Professional Voice Clone. Use Danish samples for Danish lines
and English samples for English lines. Keep the delivery consistent with the
storybook role. Aim for 192 kbps MP3 or better, one speaker, little room echo,
no music, and stable level.

The cleanest result may be faster to obtain by recording Prutti reading a
purpose-written 45–60 minute bilingual storybook script. Archival clips can then
supplement the live recording instead of carrying the model.

## Required handoff

1. Prutti approves the source list, recordings, and intended Klokkentales use.
2. Approved audio is staged under the private vault, never the repository.
3. `bin/voice.mjs create-ivc --confirm-rights-and-consent` creates the voice on
   our ElevenLabs account and stores its ID in the vault.
4. Prutti listens to and approves the voice check and complete episode.
5. A future PVC is verified by Prutti before it replaces the IVC.
