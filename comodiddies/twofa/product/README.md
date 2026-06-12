# 2FA Brush

a security brush.

## what it is

an electric toothbrush that doubles as a hardware security key — a
yubikey you brush your teeth with. charges over usb-c on the bottom,
and that same port plugs into your laptop to authenticate and store
credentials.

## why

the security key problem is a habit problem. keys get lost, left in
drawers, forgotten on other keychains. but a toothbrush has the
strongest habit loop of any object you own — you hold it twice a day,
every day, and it lives in exactly one place. the 2FA brush piggybacks
identity on hygiene: the one device you never forget to touch.

also funny — possession-factor auth meets the most intimate possession
there is. nobody borrows your toothbrush.

## how it might work

- secure element in the handle (same class as yubikey / fido2 keys) —
  webauthn, totp, ssh keys, pgp
- usb-c on the bottom: drops into a charging dock at night, plugs
  straight into the laptop when you need to auth
- brushing itself as a liveness check — the accelerometer knows your
  brushing signature; two minutes of brushing could even *unlock* the
  day's credentials, so a stolen brush is just a brush
- tap-to-auth via nfc for phones, so it never needs to leave the
  bathroom for mobile 2fa
- heads are replaceable; the identity lives in the handle

## form factor

a normal good electric brush — matte body, single button, weighty in
a reassuring way. the only tell is a tiny led near the base that
breathes when it's holding an active session. bottom face is a flush
usb-c port ringed in steel.

## open questions

- water + usb-c — fully sealed port, or charging contacts on the dock
  with usb-c only for data?
- do credentials travel with the brush or pair to it? (lost-brush
  recovery story)
- family bathroom problem — four identical brushes, four identities;
  color rings? brushing-signature disambiguation?
- name: 2FA Brush vs. security brush vs. something brushier

## story video

a ~29s instagram-story cut (1080×1920) with a jeffrey voiceover lives
in `../story/`:

- `vo.txt` — the narration (jeffrey-pvc via `pop/bin/say.mjs
  --timestamps`; cuts and captions are timed from the elevenlabs
  word alignment, nothing hand-tuned). captions carry no commas —
  YWFT Processing's comma reads as a stray period.
- `gen-motion.mjs` — Seedance 2.0 motion pass (one shot per slide:
  brushing → usb-c plug-in → coding → brush close-up → POV reading
  the sheet at the sink) via the `pop/lib/motion-pipeline.mjs`
  harness. `--dry-run` to price, `--only NAME --force` re-rolls.
- `build.mjs` — cuts the motion takes against the VO timeline,
  burns in the YWFT Processing captions, then `chrome.mjs` adds the
  Twofa side stamps. slides with no take zoompan-fall back.

rebuild: `node gen-motion.mjs` (if takes are missing), then
`node build.mjs` from `story/`.
