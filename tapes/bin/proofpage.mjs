// assemble the evidence into one page, images inlined so it stands alone.

import { readFileSync, writeFileSync } from "node:fs";

const uri = (p) =>
  `data:image/png;base64,${readFileSync(`tapes/out/${p}`).toString("base64")}`;

const plate = ({ id, claim, verdict, img, w, h, fit, caption }) => `
<figure class="plate" id="${id}">
  <figcaption class="plate-head">
    <p class="claim">${claim}</p>
    <p class="verdict">${verdict}</p>
  </figcaption>
  <div class="well${fit ? " well-fit" : ""}">
    <img src="${uri(img)}" width="${w}" height="${h}" alt="${claim}">
  </div>
  <p class="caption">${caption}</p>
</figure>`;

const html = `<title>Tapes — visual proof</title>
<style>
  :root {
    --bg: #f6f4f7; --surface: #fff; --ink: #16131a; --muted: #6b6473;
    --rule: #e3dee7; --accent: #b8127a; --pass: #17703a; --fail: #ab241d;
    --well: #0b0b10; --well-ink: #cfc9d6;
    --mono: ui-monospace, "SF Mono", SFMono-Regular, Menlo, Consolas, monospace;
    --sans: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    --col: 68ch; --wide: 1180px;
  }
  @media (prefers-color-scheme: dark) {
    :root:not([data-theme="light"]) {
      --bg: #0d0d12; --surface: #16151d; --ink: #ece8f0; --muted: #928b9d;
      --rule: #2b2734; --accent: #f050b0; --pass: #7ee08a; --fail: #f2726f;
      --well: #08080c;
    }
  }
  :root[data-theme="dark"] {
    --bg: #0d0d12; --surface: #16151d; --ink: #ece8f0; --muted: #928b9d;
    --rule: #2b2734; --accent: #f050b0; --pass: #7ee08a; --fail: #f2726f;
    --well: #08080c;
  }

  body {
    background: var(--bg); color: var(--ink);
    font-family: var(--sans); font-size: 17px; line-height: 1.65;
    margin: 0; padding: 0 24px 96px;
    -webkit-font-smoothing: antialiased;
  }
  .wrap { max-width: var(--wide); margin: 0 auto;
          display: flex; flex-direction: column; gap: 40px; }
  .col { max-width: var(--col); }
  p { margin: 0 0 1em; } p:last-child { margin-bottom: 0; }
  strong { font-weight: 650; }
  code { font-family: var(--mono); font-size: .88em;
         background: color-mix(in srgb, var(--accent) 10%, transparent);
         padding: .12em .38em; border-radius: 3px; }
  a { color: var(--accent); }

  header { padding: 72px 0 8px; display: flex; flex-direction: column; gap: 18px; }
  .eyebrow { font-family: var(--mono); font-size: 12px; letter-spacing: .16em;
             text-transform: uppercase; color: var(--accent); margin: 0; }
  h1 { font-family: var(--mono); font-size: clamp(30px, 5vw, 52px);
       letter-spacing: -.035em; font-weight: 600; line-height: 1.04;
       margin: 0; text-wrap: balance; }
  .standfirst { font-size: 20px; color: var(--muted); max-width: var(--col); margin: 0; }

  .readout { display: flex; flex-wrap: wrap; gap: 0;
             border: 1px solid var(--rule); border-radius: 4px;
             background: var(--surface); overflow: hidden; }
  .cell { flex: 1 1 190px; padding: 16px 20px;
          border-right: 1px solid var(--rule); }
  .cell:last-child { border-right: 0; }
  .cell dt { font-family: var(--mono); font-size: 11px; letter-spacing: .13em;
             text-transform: uppercase; color: var(--muted); margin: 0 0 6px; }
  .cell dd { font-family: var(--mono); font-size: 26px; font-weight: 600;
             margin: 0; font-variant-numeric: tabular-nums;
             letter-spacing: -.02em; }

  h2 { font-family: var(--mono); font-size: 13px; letter-spacing: .15em;
       text-transform: uppercase; color: var(--muted); font-weight: 500;
       margin: 0 0 14px; padding-bottom: 10px; border-bottom: 1px solid var(--rule); }

  .plate { margin: 0; display: flex; flex-direction: column; gap: 14px; }
  .plate-head { display: flex; flex-direction: column; gap: 4px; }
  .claim { font-size: 21px; font-weight: 600; margin: 0;
           letter-spacing: -.012em; text-wrap: balance; }
  .verdict { font-family: var(--mono); font-size: 13px; margin: 0;
             color: var(--pass); font-variant-numeric: tabular-nums; }
  .verdict.bad { color: var(--fail); }
  .well { background: var(--well); border: 1px solid var(--rule);
          border-radius: 4px; overflow-x: auto; padding: 14px; }
  .well img { display: block; image-rendering: pixelated; }
  .well-fit img { max-width: 100%; height: auto; margin: 0 auto; }
  .caption { font-size: 15px; color: var(--muted); max-width: var(--col); margin: 0; }
  .hint { font-family: var(--mono); font-size: 11px; letter-spacing: .1em;
          text-transform: uppercase; color: var(--muted); margin: 0; }

  .note { border-left: 2px solid var(--accent); padding: 2px 0 2px 20px;
          max-width: var(--col); }
  pre { font-family: var(--mono); font-size: 13px; line-height: 1.7;
        background: var(--surface); border: 1px solid var(--rule);
        border-radius: 4px; padding: 16px 18px; overflow-x: auto; margin: 0; }
  footer { color: var(--muted); font-size: 15px; max-width: var(--col); }
</style>

<div class="wrap">
  <header>
    <p class="eyebrow">tapes · evidence</p>
    <h1>A picture, carried inside audio, read back out</h1>
    <p class="standfirst">Four plates. Each states one claim you can check by eye,
      and each is reproducible from the repo with a single command.</p>
  </header>

  <dl class="readout">
    <div class="cell"><dt>clean round trip</dt><dd>bit&#8209;exact</dd></div>
    <div class="cell"><dt>good deck</dt><dd>31.0 dB</dd></div>
    <div class="cell"><dt>walkman</dt><dd>21.2 dB</dd></div>
    <div class="cell"><dt>through a walkman</dt><dd>213 B/s</dd></div>
  </dl>

  <section>
    <h2>Plate one · the picture is really in the waveform</h2>
    ${plate({
      id: "spectrogram",
      claim: "A spectrogram of the encoded audio, rendered without the codec's help.",
      verdict: "507 columns × 1024 bins · 0–22.1 kHz",
      img: "proof-spectrogram.png",
      w: 507,
      h: 1024,
      fit: false,
      caption:
        `This plot does not use the decoder, its plan, or its pilot tones — it is a plain
         STFT of the waveform with every bin drawn, so anything visible here is genuinely
         in the audio. The test card reads straight off it: the ramp, the resolution
         wedges, the grey stairs, the checkerboard and its diagonal. Three other things
         are visible and worth naming. The bright diagonal streak at the far left is the
         sync chirp sweeping 400&nbsp;Hz to 11&nbsp;kHz. The two unbroken horizontal lines
         bounding the image are the pilot tones. And everything above 14&nbsp;kHz is
         black — the band is deliberately empty, which is why a cassette's rolloff and a
         codec's lowpass take nothing away.`,
    })}
  </section>

  <section>
    <h2>Plate two · a photograph down a simulated cassette</h2>
    ${plate({
      id: "photo",
      claim: "The same photograph after each tape grade, decoded back to pixels.",
      verdict: "good deck 32.5 dB · walkman 22.2 dB · worn 15.4 dB",
      img: "proof-photo.png",
      w: 1246,
      h: 190,
      fit: true,
      caption:
        `A real photograph rather than a synthetic card, so the failure modes are the ones
         you would actually see. Degradation arrives as vertical streaking and noise in the
         shadows first — black sits at the bottom of the 48&nbsp;dB range, which is exactly
         where the channel's noise floor is. Nothing cliffs; the worn tape is still plainly
         the same photograph. Note the last panel: dubbing to tape <em>and then</em>
         uploading an MP3 of it costs essentially nothing beyond the tape itself.`,
    })}
  </section>

  <section>
    <h2>Plate three · a bug I reported as fixed, and the actual fix</h2>
    <div class="note col">
      <p>I originally reported this sync problem as solved. It was not. I had measured it
        <strong>once</strong>, and the simulated tape hiss is randomised per run — so that
        was a single draw of a random variable presented as a result. Re-measured across
        repeated draws, the first fix failed most of the time.</p>
    </div>
    ${plate({
      id: "sync",
      claim: "Eight independent hiss draws through the same route, before and after.",
      verdict: "full-band PHAT: sync lost 8/8   →   banded PHAT: sync lost 0/8",
      img: "proof-sync.png",
      w: 2010,
      h: 394,
      fit: false,
      caption:
        `Top row is the original correlator; bottom row is the fix. Every panel is the same
         picture through the same channel, differing only in the random hiss. "Lost" does
         not mean noise — it means the decoder locked onto the wrong offset, so the image
         is read from the wrong place and blows out as it runs past the end of the data.
         The cause was whitening the correlation across the whole spectrum, which amplifies
         noise in the bands the sync chirp never occupied — precisely the bands a cassette
         rolls off and a codec then discards. Whitening only where the chirp has energy,
         and lengthening the sweep from 0.15&nbsp;s to 0.4&nbsp;s, gives the bottom row.
         The fixed row also holds 21.2&nbsp;dB across every draw, which is the other half
         of the claim: not just working, but <em>consistently</em> working.`,
    })}
    <p class="hint">↔ scroll the plate to see all eight draws</p>
  </section>

  <section>
    <h2>Plate four · every route, worst of three draws</h2>
    ${plate({
      id: "degrade",
      claim: "Seventeen delivery routes, from untouched file to worn tape re-encoded as MP3.",
      verdict: "no route loses sync · worst case 13.7 dB",
      img: "degrade.png",
      w: 1088,
      h: 810,
      fit: true,
      caption:
        `Each panel reports the <em>worst</em> of three hiss draws, not the best — the
         direct consequence of the mistake in plate three. Any route that lost sync in any
         draw would be flagged in red; none are. The spread runs from bit-exact on an
         untouched file down to 13.7&nbsp;dB on a worn tape, and the test card stays
         readable across the whole range.`,
    })}
  </section>

  <section class="col">
    <h2>Check it yourself</h2>
    <pre>node tapes/bin/proof.mjs      # plates one, two, three
node tapes/bin/degrade.mjs    # plate four
node tapes/bin/roundtrip.mjs  # clean round trip
node tapes/bin/datatest.mjs   # bytes through a cassette</pre>
    <p class="caption" style="margin-top:14px">Hiss is seeded by default, so these
      reproduce exactly. <code>channel.seed(null)</code> opts back into real randomness and
      <code>channel.trials(n, fn)</code> runs across draws.</p>
  </section>

  <footer>
    <p><strong>What this does not prove.</strong> The cassette here is a simulation —
      band limit, wow and flutter, hiss, and tape saturation applied in software. It is
      calibrated to plausible figures for real decks, but it is not a real deck. The
      measurement that settles this is a physical tape played back through an audio
      interface, and it has not been made yet.</p>
  </footer>
</div>`;

writeFileSync("tapes/out/proof.html", html);
console.log(`📄 tapes/out/proof.html — ${(html.length / 1e6).toFixed(2)} MB`);
