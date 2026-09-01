# Pop mastering

The house target for an energetic single is **−10 ±1 LUFS integrated**, **≤−2 dBTP**, **LRA 4–8 LU**, and **PLR 8–11 dB**. A wider narrative mix may land at −11 to −12.5 LUFS and LRA 7–10; sleep and ambient work keeps its intentional range.

Spotify's −14 LUFS figure is a playback-normalization level, not a requirement to master every record at −14. Spotify turns louder tracks down; its own guidance asks masters louder than −14 LUFS to stay below −2 dBTP. Apple Sound Check also normalizes playback, and Apple recommends delivering the highest native-resolution 24-bit source and checking the AAC encode rather than upsampling.

## Release gate

- Shape loudness in the arrangement and buses. The final pass is measured static gain into an oversampled true-peak limiter, never a dynamic loudness normalizer riding the song.
- Keep ordinary limiter reduction near 0–2 dB and rare hits below 4 dB. If more is needed, control drums, bass, vocals, and dense effects upstream.
- Keep bass mono below roughly 120 Hz, high-pass non-musical subsonics, and give bass/kick audible harmonics above 180 Hz.
- The loudest eight seconds must lose no more than 5 dB through the mono 180 Hz–8 kHz phone proxy and no more than 1.5 dB on an unfiltered mono fold, unless the failure is an explicit compositional effect.
- Check raw level and a −14 LUFS playback-matched A/B. The matched comparison decides clarity; the raw comparison catches players with normalization disabled or absent.
- Deliver 24-bit WAV/FLAC at the native project rate when the distributor permits it. Test the decoded AAC for overs and distortion before upload.

## 2026-09-01 reference audit

Eleven official 30-second Apple previews were compared with thirteen released, delivered, or staged AC masters. Preview LUFS describes only the supplied section, not the complete commercial master. Decoded AAC true peaks may exceed 0 dBTP even when the source master did not; the preview files remain unversioned under `tmp/mastering-references/`.

| Reference preview | LUFS-I | PLR | Phone loss |
|---|---:|---:|---:|
| Skrillex — Bangarang | −4.3 | 7.3 | −4.6 dB |
| Skrillex / Fred again.. / Flowdan — Rumble | −8.3 | 9.0 | −8.2 dB |
| Skrillex / Noisia / Dylan Brady — Supersonic | −6.5 | 7.5 | −9.2 dB |
| Dylan Brady — Key of C | −7.3 | 7.9 | −6.5 dB |
| Dylan Brady — Of Course I Still Love You | −11.6 | 11.4 | −3.8 dB |
| 100 gecs — 757 | −5.8 | 5.7 | −5.4 dB |
| Thoom — December Forever | −10.3 | 11.4 | −6.4 dB |
| Thoom — Feral | −6.5 | 7.3 | −14.1 dB |
| Olivia Rodrigo — bad idea right? | −5.9 | 6.5 | −2.8 dB |
| Olivia Rodrigo — good 4 u | −7.5 | 7.4 | −3.8 dB |
| Olivia Rodrigo — vampire | −6.2 | 7.3 | −3.2 dB |

The reference-section median is −6.5 LUFS with roughly 7.5 dB PLR. Their loudness comes from dense, controlled choruses and deliberate midrange—not peak height.

| AC master | LUFS-I | dBTP | PLR | Phone loss |
|---|---:|---:|---:|---:|
| americomputadora | −9.0 | −1.5 | 7.5 | −1.5 dB |
| Wattajetta Stone Club | −10.3 | −1.2 | 9.1 | −13.9 dB |
| wannadash | −12.1 | −1.0 | 11.1 | −4.1 dB |
| Femrag++ | −13.5 | −1.1 | 12.4 | −8.5 dB |
| lonerclub v4pid | −13.4 | −1.7 | 11.7 | −13.6 dB |
| helpabeach | −13.4 | −1.0 | 12.4 | −3.6 dB |
| fluttabap360 CDN print | −8.6 | +0.7 | 9.3 | −1.4 dB |
| amaythingra | −14.0 | −1.2 | 12.8 | −12.0 dB |
| trancenwaltz | −13.5 | −1.1 | 12.4 | −4.9 dB |
| trancepenta | −14.5 | −1.1 | 13.4 | −3.3 dB |
| marimbaba | −14.4 | −1.5 | 12.9 | −2.5 dB |
| hellsine | −13.6 | −1.9 | 11.7 | −58.1 dB at the sub climax |
| momabobasheep | −14.3 | −1.0 | 13.3 | −6.4 dB |

The catalog median is −13.5 LUFS, about 4–5 dB below the new house target on players without normalization. `americomputadora` already has chart-like density and excellent small-speaker translation. `wannadash` translates well but is more open than the chart references. `Femrag++`, `lonerclub v4pid`, and `Wattajetta` need more upper-bass/midrange information rather than indiscriminate master gain. `hellsine` intentionally becomes almost pure sub at its climax and should keep that exception documented.

The CDN `fluttabap360.mp3` measures −8.6 LUFS and +0.7 dBTP after decode, while the release ledger describes a −14 LUFS delivery master. Resolve the exact DistroKid asset before any catalog-wide remaster decision.

## Audit

```sh
node pop/bin/master-audit.mjs path/to/master.wav path/to/reference.m4a
```

`phone_loss` is the level change in the loudest eight-second window after mono summing and filtering to 180 Hz–8 kHz. It is a repeatable stress test, not a loudspeaker model.

## Graphic review meter

`pop/viz/loudness_meter.py` supplies reusable 10 Hz EBU R128 telemetry for graphic MP4 review renders. The overlay shows live short-term and integrated LUFS, current true peak, the creative target band, and the gain a −14 LUFS playback-normalized service would apply.

```sh
pop/.venv/bin/python pop/cult/viz/review-score.py \
  --audio pop/cult/out/wannadash-competitive-master.wav \
  --out pop/cult/out/wannadash-competitive-review-2560x1920.mp4 \
  --lufs-target -10.5 --tp-ceiling -2.0
```

Sources: Spotify loudness normalization, <https://support.spotify.com/artists/article/loudness-normalization/>; Apple Digital Masters, <https://www.apple.com/apple-music/apple-digital-masters/>; Apple Sound Check, <https://support.apple.com/guide/iphone/change-the-way-music-sounds-iph5643d2c85/ios>.
