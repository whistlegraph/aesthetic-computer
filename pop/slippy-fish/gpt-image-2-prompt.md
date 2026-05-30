# slippy fish — gpt-image-2 cover prompt (DRAFT — do not auto-run)

Stage 2 of the cover pipeline. Stage 1 produced a 3000×3000 software-raycast
render of a koi fish inside a glass orb:

- `~/Documents/Shelf/slippy-fish/slippy-fish-render-3000.jpg` (working copy)
- `~/Desktop/slippy-fish-render-3000.jpg` (Desktop copy)

This render is the **structure / style reference** for the final cover. The
image-to-image pass keeps the fish's pose, silhouette, and palette, and pushes
the rendering toward a richer, painterly glass-orb-in-deep-water finish.

## oskie identity (from Step 0)
- **oskie** is an AC artist — prior tracks "oskie - tokyo 90 bpm" and
  "oskie - zzzZWAP" (the zzzZWAP locator visualizer lives in `wipppps.mjs`).
- **Sonic identity:** a 4-bar swung bounce, ~101 BPM, fixed E-major pedal;
  bottom-up sine-additive voices (bass / pad / bells / hat) chopped and
  granularly resampled from one gritty source recording. Warm, hypnotic,
  loop-locked, a little aquatic and rubbery — "slippy."
- **zzzZWAP visual palette** (carry as accent energy, not literal): electric
  **lime green**, **magenta**, soft **pink**, neutral grays on near-black.
- The render's own koi palette — vivid **orange flank**, pale belly, dark warm
  back — is the hero color. Keep it dominant; let lime/magenta live only as
  faint caustic glints and rim light so the cover still feels like "oskie."

## Final cover prompt (paste as the image-gen prompt)

> A single plump koi-goldfish suspended inside a clear glass orb, swimming
> gently upward to the right, seen broadside at a three-quarter angle with one
> round black eye and a tiny white catch-light. Glossy wet skin in vivid
> tangerine orange fading to a pale cream belly and a deep maroon back; soft
> flowing dorsal and fan tail fins. The orb is full of glowing aqua-to-deep-blue
> water with slow caustic light ripples; a bright soft specular highlight blooms
> on the upper glass like a captured sun, with faint lime-green and magenta
> color fringes glinting along the rim and in the water. Smooth painterly
> shading, soft volumetric light, dreamy and hypnotic, a touch rubbery and
> "slippy," clean negative space in the lower-left water. Square album-cover
> composition, no text, no lettering, no logos, no watermark, no border.

## Honor pop cover rules
- **No real-brand wordmarks** anywhere; no Apple/PALS/whistlegraph marks.
- ID3 / album context for the single is **"pixsies"** (used in metadata later —
  not printed on the art).
- **Never composite text or patches onto a gen.** If the model bakes in stray
  text or a wrong detail, fix the prompt and **regenerate** — do not paste a
  correction over the output.
- Any wordmark ("slippy fish" / "oskie") is added later as a proper typographic
  layer in the cover build step, never asked of the model here.

## Ready-to-run command (DO NOT RUN — the user will run this themselves)

Reference-image (image-to-image) edit against gpt-image-2. Fill in the API key
and run manually when ready. This spends money on the OpenAI Images API.

```bash
# DRAFT — run manually. Costs money (OpenAI Images API). Not executed by the agent.
REF="$HOME/Desktop/slippy-fish-render-3000.jpg"
OUT="$HOME/Documents/Shelf/slippy-fish/slippy-fish-cover-3000.png"

curl -s https://api.openai.com/v1/images/edits \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F model="gpt-image-2" \
  -F size="1024x1024" \
  -F quality="high" \
  -F image[]="@$REF" \
  -F prompt="A single plump koi-goldfish suspended inside a clear glass orb, swimming gently upward to the right, seen broadside at a three-quarter angle with one round black eye and a tiny white catch-light. Glossy wet skin in vivid tangerine orange fading to a pale cream belly and a deep maroon back; soft flowing dorsal and fan tail fins. The orb is full of glowing aqua-to-deep-blue water with slow caustic light ripples; a bright soft specular highlight blooms on the upper glass like a captured sun, with faint lime-green and magenta color fringes glinting along the rim and in the water. Smooth painterly shading, soft volumetric light, dreamy and hypnotic, a touch rubbery and slippy, clean negative space in the lower-left water. Square album-cover composition, no text, no lettering, no logos, no watermark, no border." \
  | python3 -c "import sys,json,base64; d=json.load(sys.stdin); open('$OUT','wb').write(base64.b64decode(d['data'][0]['b64_json'])); print('wrote $OUT')"

# Then upscale to the 3000x3000 delivery size:
# node -e "require('/Users/jas/aesthetic-computer/ac-electron/node_modules/sharp')('$OUT').resize(3000,3000,{kernel:'lanczos3'}).png().toFile('${OUT/.png/-3000.png}')"
```

Notes:
- `gpt-image-2` returns base64 in `data[0].b64_json` (no URL), hence the decode step.
- Generate at `1024x1024` (square) then upscale; the API square size keeps the
  koi centered with the same framing as the reference render.
- Chosen render camera params (for reproducibility): `time=1.2 yaw=0.5 zoom=3.3`,
  rendered at 6000² step:1 and downscaled 2× to 3000² for anti-aliasing.
