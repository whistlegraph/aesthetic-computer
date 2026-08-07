# MODEM — dial-up sample collection

20 dial-up modem sounds from Freesound — 56k handshakes, RTTY/BPSK carriers,
a 1200-baud carrier, dial sequences, data-click bursts, and a disconnect —
with per-sample pitch analysis so tracks can paste them in **pitch-matched to
an E-rooted composition** (FEM bells, guitar power chords on E).

- **Fetch:** `node fetch.mjs` (re-runnable; dedupes by Freesound id; pruned
  ids stay pruned via the `EXCLUDE` list).
- **Analyze:** `node analyze.mjs` → regenerates `manifest.json` (Goertzel scan
  80–3600 Hz in 4 Hz steps, up to 3 s windows, first 0.3 s skipped; long
  handshakes get 2–3 windows since they change character over time).
- **License/attribution:** all samples are CC0 or CC-BY previews; full
  attribution records (author, license URL, source URL) live in the vault at
  `~/aesthetic-computer-vault/personal/pop/freesound-cache/_attributions.json`,
  keyed by the numeric id prefix of each filename. CC-BY samples need credit
  in release notes.

## Samples

| File | Dur | Tags | Dominant tone | Best E-world rates |
|---|---|---|---|---|
| 109143-bpsk125_ogg.mp3 | 4.6s | carrier | 1012 Hz · B5 +42¢ | E5 ×0.6514 |
| 109145-bpsk31_ogg.mp3 | 8.0s | carrier | 1016 Hz · B5 +49¢ | E5 ×0.6489 |
| 109147-rtty_45_1000hz_ogg.mp3 | 9.0s | carrier | 1008 Hz · B5 +35¢ | E5 ×0.6540 |
| 158621-dialup_call.mp3 | 25.8s | mixed, dialing, handshake | 440 Hz · A4 +0¢ | E4 ×0.7492 · B4 ×1.1225 · E5 ×1.4983 |
| 16475-dialup_mp3.mp3 | 27.6s | handshake, dialing | 440 Hz · A4 +0¢ | E4 ×0.7492 · B4 ×1.1225 · E5 ×1.4983 |
| 188828-modem_dial_wav.mp3 | 18.6s | handshake, dialing | 1300 Hz · E6 −24¢ | E5 ×0.5071 |
| 397079-digitalradio_noise4_wav.mp3 | 13.2s | clicks | 900 Hz · A5 +39¢ | B4 ×0.5488 · E5 ×0.7325 |
| 42996-alienfaxstereo_wav.mp3 | 15.0s | carrier, mixed | 768 Hz · G5 −36¢ | G4 ×0.5104 · B4 ×0.6431 · E5 ×0.8584 |
| 454649-modem_3_aif.mp3 | 17.8s | handshake | 120 Hz · B2 −49¢ | E2 ×0.6868 · B2 ×1.0289 · E3 ×1.3734 |
| 454650-modem_2_aif.mp3 | 16.6s | handshake | 480 Hz · B4 −49¢ | E4 ×0.6867 · B4 ×1.0289 · E5 ×1.3735 |
| 454651-modem_1_aif.mp3 | 21.8s | handshake | 440 Hz · A4 +0¢ | E4 ×0.7492 · B4 ×1.1225 · E5 ×1.4983 |
| 49608-dialup_login_dec_2001_24_bit_wav.mp3 | 22.5s | handshake, dialing | 924 Hz · A♯5 −16¢ | B4 ×0.5345 · E5 ×0.7135 |
| 586442-fake_dial_up_modem_sound_tape_recorded.mp3 | 20.5s | mixed | 432 Hz · A4 −32¢ | E4 ×0.7630 · B4 ×1.1432 · E5 ×1.5261 |
| 591324-vara_fm_hello_world_wav.mp3 | 26.6s | carrier, handshake | 1780 Hz · A6 +20¢ | B5 ×0.5549 · E6 ×0.7407 |
| 62843-modem_mp3.mp3 | 28.6s | handshake | 852 Hz · G♯5 +44¢ | B4 ×0.5797 · E5 ×0.7738 |
| 658932-dial_up_sound_mp3_flac.mp3 | 19.3s | handshake, dialing | 440 Hz · A4 +0¢ | E4 ×0.7492 · B4 ×1.1225 · E5 ×1.4983 |
| 78657-modem1200_wav.mp3 | 5.6s | carrier | 2064 Hz · C7 −24¢ | E6 ×0.6388 |
| 8037-modem_1_53_wav.mp3 | 2.2s | clicks | 1200 Hz · D6 +37¢ | E5 ×0.5494 |
| 8055-modem_1_97_wav.mp3 | 2.6s | clicks (noisy) | — | none needed |
| 844723-at_t_internet_gateway_modem…mp3 | 11.3s | disconnect, clicks | 644 Hz · E5 −41¢ | E5 ×1.0237 (nearly in tune!) |

Notes on what the numbers mean: the classic US dial tone (350+440 Hz) shows
up as an exact **A4** in four of the handshakes, the V.25 answer-tone region
puts several carriers near **B5**, and the RTTY/BPSK trio sits tightly at
~1010 Hz — one rate (≈0.65) drops all three onto E5. The AT&T disconnect
already rings 41¢ flat of E5; a 1.0237 rate tunes it exactly.

## How to use

Paste a sample into a composition by **rate-resampling** so its dominant tone
lands on a scale tone of the E-rooted world (rate = target Hz ÷ peak Hz —
resample the buffer by that factor, or play it back at that rate). The
`rates` map in `manifest.json` is precomputed from the strongest stable peak,
kept within [0.5, 2.0] so material never smears more than an octave:

- **Tonal carriers** (RTTY/BPSK, modem1200, VARA FM) hold a steady pitch —
  tune them to E or B and they read as synth drones/pads that chord with the
  FEM bells. Carriers above ~1.3 kHz get fallback targets G5/B5/E6, still
  E-minor chord tones.
- **Handshakes** change pitch over time (dial tone → answer tone → carrier);
  the manifest records 2–3 analysis windows so you can pick which *phase*
  to tune (e.g. tune the 440 Hz dial-tone phase to E4 with ×0.7492, or slice
  the carrier phase out and tune that instead).
- **Clicks/noisy bursts** (8055, and the click layers of 397079/844723) need
  no tuning — use them raw as percussion: data-burst hats, negotiation-chirp
  fills, disconnect as a stop/fill hit.

Example (any /pop C or JS engine): to put the BPSK31 carrier on E5, resample
`109145-bpsk31_ogg` by 0.6489 (its 1016 Hz peak → 659.26 Hz). Stack the RTTY
at ×0.6540 for a phasing unison. Drop `454649-modem_3` at ×0.6868 for an E2
sub-layer under the power chords.
