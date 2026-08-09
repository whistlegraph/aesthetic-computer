#!/usr/bin/env python3
"""Build the CultureHub LA two-work program brief as printable HTML.

Run `python3 build.py`, then print it with headless Chrome:

    chrome --headless --disable-gpu --no-pdf-header-footer \\
      --print-to-pdf=culturehub-brief.pdf culturehub-brief.html

There is no xelatex on this Mac, so this brief deliberately avoids the
packet's LaTeX toolchain while still using its fonts and palette.
Regenerate `media/` with `prep-media.sh` if the source assets change.
"""
import base64, pathlib

SP = pathlib.Path(__file__).resolve().parent
MED = SP / "media"
PKT = SP.parent / "packet" / "assets"


def b64(p, mime):
    return f"data:{mime};base64," + base64.b64encode(pathlib.Path(p).read_bytes()).decode()


img = {n: b64(MED / f"{n}.jpg", "image/jpeg") for n in
       ["cover", "spatial", "gscore", "circular", "header",
        "head-main", "head-green", "head-seated",
        "mn-block", "mn-trio", "mn-menubar", "mn-program",
        "canon-colorway", "canon-trackdrum", "canon-keymap", "canon-menuband"]}
fbold = b64(PKT / "ywft-processing-bold.ttf", "font/ttf")
flight = b64(PKT / "ywft-processing-light.ttf", "font/ttf")


SQUARE = {"cover", "gscore", "circular", "canon-trackdrum"}


def plate(key, title, meta, caption, status, tone="ok"):
    sq = " sq" if key in SQUARE else ""
    return f"""
    <figure class="plate{sq}">
      <img src="{img[key]}" alt="">
      <figcaption>
        <div class="pt"><span class="ptitle">{title}</span><span class="badge {tone}">{status}</span></div>
        <div class="pmeta">{meta}</div>
        <p>{caption}</p>
      </figcaption>
    </figure>"""


HTML = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>Whistlegraph presents — CultureHub LA 2026 — Program Brief</title>
<style>
@font-face {{ font-family:'YWFTBold'; src:url({fbold}) format('truetype'); }}
@font-face {{ font-family:'YWFTLight'; src:url({flight}) format('truetype'); }}
:root {{
  --pink:#B44887; --purple:#7850B4; --blue:#1559A6; --cyan:#168F91;
  --dark:#312B38; --gray:#66636A; --pale:#F4F1F5;
  --green:#2D7650; --orange:#A85D24; --red:#8E2942;
}}
@page {{ size: letter; margin: 0.62in 0.72in 0.68in; }}
* {{ box-sizing:border-box; }}
body {{ font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; color:var(--dark);
  font-size:10pt; line-height:1.5; margin:0; }}
h1,h2,h3 {{ margin:0; }}
a {{ color:var(--purple); text-decoration:none; }}
code {{ font-family:'SF Mono',Menlo,monospace; font-size:0.88em; color:var(--blue); }}
em.w {{ font-style:italic; }}

.rule {{ height:1.15pt; background:var(--pink); margin:.28em 0 .5em; }}
.eyebrow {{ font-size:8pt; letter-spacing:.13em; text-transform:uppercase; color:var(--pink); font-weight:700; }}
.title {{ font-family:'YWFTBold'; font-size:31pt; line-height:1.04; color:var(--dark); }}
.sub {{ font-family:'YWFTLight'; font-size:14pt; color:var(--pink); line-height:1.25; }}
h2 {{ font-family:'YWFTBold'; font-size:16.5pt; color:var(--dark); margin:1.5em 0 .1em; }}
h2 .c {{ float:right; font-family:'Helvetica Neue',sans-serif; font-size:8pt;
  letter-spacing:.1em; text-transform:uppercase; color:var(--gray); font-weight:600; padding-top:.9em; }}
h3 {{ font-size:10.5pt; margin:1.1em 0 .12em; color:var(--dark); }}
p {{ margin:.42em 0; }}

.banner {{ background:var(--red); color:#fff; padding:.5em .8em; border-radius:3px;
  font-size:8.6pt; letter-spacing:.05em; font-weight:700; margin-bottom:1.1em; }}
.banner span {{ font-weight:400; opacity:.9; }}

.lede {{ font-size:11pt; line-height:1.55; border-left:2.5pt solid var(--pink);
  padding-left:.85em; margin:.9em 0 0; color:#413a48; }}

.grid2 {{ display:grid; grid-template-columns:1fr 1fr; gap:.85em; margin-top:.6em; }}
.card {{ border:.6pt solid #d8d2dc; border-radius:4px; padding:.7em .8em; background:var(--pale);
  break-inside:avoid; }}
.card h3 {{ margin-top:0; color:var(--pink); font-family:'YWFTBold'; font-size:12.5pt; }}
.card .k {{ font-size:8pt; letter-spacing:.08em; text-transform:uppercase; color:var(--gray); font-weight:600; }}

table {{ width:100%; border-collapse:collapse; margin-top:.5em; font-size:9.2pt; }}
th {{ text-align:left; font-size:7.8pt; letter-spacing:.1em; text-transform:uppercase;
  color:var(--gray); border-bottom:.8pt solid var(--dark); padding:.3em .5em .25em 0; }}
td {{ padding:.36em .5em .36em 0; border-bottom:.4pt solid #ded8e2; vertical-align:top; }}
td:first-child {{ white-space:nowrap; font-weight:600; }}

.badge {{ font-size:7pt; letter-spacing:.07em; font-weight:700; color:#fff;
  padding:.16em .45em; border-radius:2px; text-transform:uppercase; white-space:nowrap; }}
.ok {{ background:var(--green); }} .hold {{ background:var(--orange); }}
.need {{ background:var(--red); }} .info {{ background:var(--blue); }}

.plate {{ margin:0 0 .8em; border:.6pt solid #d8d2dc; border-radius:4px; overflow:hidden;
  break-inside:avoid; background:#fff; }}
.plate img {{ display:block; margin:0 auto; max-width:5.25in; width:100%; height:auto; }}
.plate.sq img {{ max-width:3.55in; }}
.amp {{ font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; font-style:normal;
  font-size:.82em; color:var(--pink); padding:0 .12em; }}
.keep {{ break-inside:avoid; }}
.plate figcaption {{ padding:.5em .7em .6em; border-top:.6pt solid #ded8e2; }}
.pt {{ display:flex; justify-content:space-between; align-items:center; gap:.5em; }}
.ptitle {{ font-weight:700; font-size:9.6pt; }}
.pmeta {{ font-size:7.6pt; color:var(--gray); font-family:'SF Mono',Menlo,monospace; margin:.12em 0 .25em; }}
.plate p {{ margin:0; font-size:8.5pt; line-height:1.42; color:#4a434f; }}

ul {{ margin:.35em 0; padding-left:1.15em; }}
li {{ margin:.22em 0; }}
.warn {{ border-left:2.5pt solid var(--orange); padding-left:.8em; background:#fdf6ef;
  padding:.55em .8em; border-radius:0 3px 3px 0; margin:.5em 0; }}
.warn strong {{ color:var(--orange); }}
.foot {{ margin-top:1.6em; padding-top:.5em; border-top:.6pt solid #ded8e2;
  font-size:7.8pt; color:var(--gray); }}
.pagebreak {{ break-before:page; }}
.avoid {{ break-inside:avoid; }}
</style></head><body>

<div class="banner">⛔ EMBARGOED — INTERNAL <span>· Not for publication before CultureHub's formal resident announcement, expected late August / early September 2026.</span></div>

<div class="eyebrow">CultureHub Los Angeles · Residency 2026 · September 16–25</div>
<div class="title">Whistlegraph<br>presents.</div>
<div class="rule"></div>
<div class="sub"><em class="w">Special Sign</em><span class="amp">&nbsp;+&nbsp;</span><em class="w">MacNeoPolitan</em></div>

<p class="lede">Two works for laptop ensemble. Six salvaged laptops pulled back from
e-waste, and three brand-new MacBook Neos. It is the same instrument either way —
and that is the argument.</p>

<h2>The program<span class="c">two works · nine laptops</span></h2>
<div class="rule"></div>

<div class="grid2">
  <div class="card">
    <div class="k">Work one · live spatial version</div>
    <h3>Special Sign</h3>
    <p>A composition for twelve moving sound bodies, expanded live from a fixed
    1:41 recording. Six salvaged laptops, booted into <strong>AC Native</strong> and running
    <strong>notepat</strong>, become one distributed instrument inside the 5:1 Kalio surround.
    From a live control surface Jeffrey rotates the whole field, changes its speed
    and trajectory, and redistributes its voices among the computers.</p>
    <p>It can go differently every time it is played.</p>
  </div>
  <div class="card">
    <div class="k">Work two · new · a spatial audio play</div>
    <h3>MacNeoPolitan</h3>
    <p>Three MacBook Neos — indigo, citrus, blush — each played through
    <strong>Menu Band</strong>, the menu-bar piano. Three flavors in a block.</p>
    <p>The keyboard sits at the top of the screen beside the clock, the WiFi, and
    the battery. That placement is the whole argument: an instrument should be a
    built-in facility of a computer, not an application you go out and get.</p>
  </div>
</div>

<h3>Why they pair</h3>
<p>One work runs on machines people threw away. One runs on machines bought new off
the shelf. <strong>notepat</strong> and <strong>Menu Band</strong> share a lineage and a keymap — Menu Band is
the accessible descendant. Putting them on one program turns a single expanded
track into an argument about hardware indifference, and it gives the residency's
e-waste premise something to push against.</p>

<div class="warn"><strong>Technical note that governs all copy.</strong> AC Native is x86_64 UEFI
only — it boots <code>EFI/BOOT/BOOTX64.EFI</code> and cannot run on Apple Silicon.
<em>MacNeoPolitan</em> therefore runs <strong>Menu Band</strong>, a shipping universal macOS app, and no
port is required. <strong>Never write that AC Native runs on Apple hardware.</strong></div>

<div class="keep">
<h2>Public programs<span class="c">proposed · pending confirmation</span></h2>
<div class="rule"></div>
<table>
  <tr><th>When</th><th>What</th><th>Notes</th></tr>
  <tr><td>Sat Sep 19<br>2:00–3:30 pm</td><td><strong>Menu Band Jam</strong></td>
      <td>Bring a Mac and install Menu Band — free, open source, about a minute. Bring any
      other laptop and boot AC Native from a USB stick, which changes nothing on the disk.
      No programming or musical experience required; loaners available.</td></tr>
  <tr><td>Thu Sep 24<br>7:00–8:00 pm</td><td><strong>Performance &amp; conversation</strong></td>
      <td>Both works performed inside the surround field, audience seated in the score.
      ~40 minutes of music, then a conversation. Livestream requested; platform TBD.</td></tr>
</table>
</div>

<div class="keep">
<h2>Media<span class="c">contact sheet · approval needed</span></h2>
<div class="rule"></div>
<p>Everything currently available to the campaign, with its status. Two slots are
still open: the two-work event image, and the final portrait selection.</p>
</div>

{plate("cover", "Special Sign — release cover", "special-sign-cover-3000.jpg · 3000×3000 · canonical", "The <em>Attic Gremlin</em> colored-pencil cover: Jeffrey mid-leap with a green laptop beside the spatial sine globe, cables running to it across an attic studio. Establishes the house illustration style — visible hatching, paper tooth, no wash. Note the green laptop, which rhymes with the citrus MacBook Neo.", "APPROVED")}

{plate("spatial", "3D spatial overview — still", "special-sign-live-spatial-3x2.jpg · 1800×1200 · frame @ 1:12", "Twelve translucent wireframe bodies rise as glowing columns from a dark field, threaded by note lanes and starburst markers, with a looping petal-shaped path around them. The white dot labelled LISTENER sits at centre. Currently the interim event-page image.", "INTERIM", "hold")}

{plate("gscore", "Graphic score — moving", "special-sign-graphic-score.mp4 · 2160² · frame @ 1:12", "Thirteen named lanes — bass, boom, pad-l/r, melody, echo, hats, noses, gong, air, <em>jeffrey vowels</em> — over spatial rotation, kick gravity, and dynamic arc. Section IV, “Constellation · counterpoint and sine garden.” The clearest single image of how the piece is actually built.", "APPROVED")}

{plate("circular", "Circular score — moving", "special-sign-circular-score.mp4 · frame @ 0:50", "The radial score under a fixed receiver needle, reading 70% WET at the Super-Spin section. Shows the speed-to-wetness mapping that the live version puts under hand control — the single best illustration of what “played in real time” means here.", "APPROVED")}

{plate("header", "Notepat Jam — showroom illustration", "notepat-jam-3x2.png = header.png · 3:2 · live on the public page", "Six salvaged laptops ringed on plywood tables, lids turned screens-away, colored glow on every face, audience seated low inside the ring. Still accurate for <em>Special Sign</em> and still the hero on the public page — but it predates <em>MacNeoPolitan</em> and shows no MacBook Neos.", "SUPERSEDE", "hold")}

<div class="keep">
<h2>MacNeoPolitan<span class="c">new art · august 9</span></h2>
<div class="rule"></div>
<p>Three illustrations for the new work, in the house colored-pencil style.
Colorways verified against Apple; the twelve notepat semitone keys and the
TrackDrum zone map are drawn from the source, not from memory.</p>
</div>

{plate("mn-block", "The Neapolitan block", "gens/block.png · 1536×1024 · gpt-image-2", "Three MacBook Neos overhead — indigo, citrus, blush. All twelve notepat semitone keycaps are coloured on each machine: Q W R T Y U O P across the top row, S and the apostrophe on the home row, Z and V below. E and I stay white naturals. Trackpads carry the TrackDrum instrument map — hi-hat, snare, tom, kick. A note passes machine to machine and one long arc closes the circle.", "APPROVED")}

{plate("mn-trio", "Jeffrey inside the ring", "gens/trio.png · 1536×1024 · gpt-image-2", "The three machines turned inward around a seated listener, personified by posture alone. Every screen carries the real Menu Band interface. The chalk ring on the floorboards is the shared downbeat — three separate computers, no cable between them, one pulse.", "APPROVED")}

{plate("mn-menubar", "A piano beside the clock", "gens/menubar.png · 1536×1024 · gpt-image-2", "The argument in one frame: WiFi, battery, a small segmented keyboard, and the time — all the same size in the same row on a blush machine. The instrument is a built-in facility, not an application.", "APPROVED")}

<div class="keep">
<h2>The two-work image<span class="c">event page · both works</span></h2>
<div class="rule"></div>
<p>The whole program in one frame — the image the event page runs. Colored pencil,
matching the <em>Special Sign</em> cover and the existing public-page header rather
than the MacNeoPolitan house style, because it has to sit across both works.</p>
</div>

{plate("mn-program", "Nine laptops", "gens/program.png · 1536×1024 · gpt-image-2", "Three new MacBook Neos open across the front — indigo, citrus, blush — screens toward the viewer with the lit menu-bar keyboard strip visible on each. Six salvaged laptops ring behind, mismatched and visibly older, screens full of coloured pads, PALS stickers on the lids. A listener sits between the two groups. The old/new contrast is the pitch, and this is the only image that carries both halves.", "APPROVED")}

<div class="keep">
<h2>Canon<span class="c">what the art is checked against</span></h2>
<div class="rule"></div>
<p>The four sources every <em>MacNeoPolitan</em> image is verified against. None of
these are drawn from memory — the colourway comes from Apple, the drum and the
keymap come out of the source code, and the interface is a real screenshot.</p>
</div>

{plate("canon-colorway", "The four colourways", "Apple Newsroom press photo · © Apple", "MacBook Neo shipped March 2026 in <em>blush, indigo, silver</em> and <em>citrus</em> — there is no “blueberry” and no “rose.” Measured inside the lids: blush #dfc7c7, indigo #495369, citrus #d8d680. The citrus test is R ≈ G: real citrus is R216 G214, a true yellow. Anything where green leads red has drifted to chartreuse, which is the mistake every early take made.", "REFERENCE", "info")}

{plate("canon-trackdrum", "TrackDrum", "rendered from Sources/TrackDrumIcon.swift", "Not a membrane with ripples — an outside-to-centre instrument map: sage hi-hat, terracotta snare with diagonal wires, ochre tom, dark umber kick. The two touch dots and tether were removed from the source at Jeffrey's request; nothing rests on the zones. Re-render with <code>render-icon.sh</code> after any change to that file.", "REFERENCE", "info")}

{plate("canon-keymap", "The twelve semitones", "notepat keymap · derived from labelByMidiNotepat", "Semitones are <code>midi % 12 ∈ {{1,3,6,8,10}}</code>: z=A♯3, v=C♯4, s=D♯4, w=F♯4, r=G♯4, q=A♯4, t=C♯5, y=D♯5, u=F♯5, o=G♯5, p=A♯5, ’=C♯6. Twelve, not eleven — the apostrophe sits at the top of the range and was missing from the first four takes. E and I are naturals and stay unpainted.", "REFERENCE", "info")}

{plate("canon-menuband", "Menu Band", "Mac App Store screenshot · the real interface", "The menu-bar item is a small multi-segment keyboard beside the clock. The popover carries a staff with coloured note-dots, chord chips, the layout picker, and the three modes — Pointer, Notepat, Ableton. <em>MacNeoPolitan</em> runs the Notepat layout, which is why the machines carry the notepat twelve and not the popover's own compact map.", "REFERENCE", "info")}

<div class="avoid">
<h2>Portrait options<span class="c">one must be chosen</span></h2>
<div class="rule"></div>
</div>

{plate("head-main", "Headshot — plain portrait", "jeffrey-alan-scudder-headshot-3x2.jpg · animated .webp/.gif also prepared", "Close portrait against deep blue, broad smile; the animated WebP cuts from a sideways smile to this one. Warmest as a portrait, but it shows him beside a computer rather than playing one. Now offered as the fallback if CultureHub wants a plain headshot.", "ALTERNATE", "info")}

{plate("head-green", "Headshot — green laptop", "…-3x2-green-laptop.jpg", "Holding an open, heavily stickered laptop lit green from the screen. Reads as “person who plays computers” rather than “portrait,” and the sticker-covered lid tells the salvage story instantly. <strong>Chosen for the artist page.</strong>", "SELECTED")}

{plate("head-seated", "Headshot — seated", "…-3x2-seated.jpg", "Seated against a plaster wall in daylight, red glasses and a yellow pencil on the shirt, bag over the shoulder. Calmest and most editorial; best if CultureHub's page runs portraits large.", "ALTERNATE", "info")}

<h2 class="pagebreak">Copy — what exists<span class="c">written and ready</span></h2>
<div class="rule"></div>
<table>
  <tr><th>File</th><th>Contents</th><th>State</th></tr>
  <tr><td><code>ARTIST-PAGE.md</code></td><td>Bio, two-work project statement, links, headshot metadata and alt text</td><td>Ready to send</td></tr>
  <tr><td><code>EVENT-PAGE.md</code></td><td>Public-program copy, both works, audience interaction, proposed dates</td><td>Ready; dates provisional</td></tr>
  <tr><td><code>ANNOUNCE.md</code></td><td>Press blurb, one-liner, four social posts, calendar listings</td><td>Written, embargoed</td></tr>
  <tr><td><code>POSTER.md</code></td><td>Art direction, generation prompt, crop sizes, typography</td><td>Brief only — art not made</td></tr>
  <tr><td><code>TECH-RIDER.md</code></td><td>Both works, two patches, clock sync, power, changeover, risks</td><td>Ready to send</td></tr>
  <tr><td><code>web-draft/</code></td><td>Rewritten public page, keeps the submitted proposal as record</td><td>Held for embargo</td></tr>
  <tr><td><code>packet/*.tex</code></td><td>Artist page, event page, rider, send manifest</td><td>Stale — needs rebuild</td></tr>
</table>

<h2>Open items<span class="c">what is actually blocking</span></h2>
<div class="rule"></div>
<table>
  <tr><th>By</th><th>Item</th><th>Note</th></tr>
  <tr><td>Aug 11</td><td>Artist-page materials due</td><td>Copy and headshot are ready. Only the portrait choice is outstanding.</td></tr>
  <tr><td>Aug 16</td><td>Technical rider</td><td>Rewritten for both works. Send once CultureHub names a technical contact.</td></tr>
  <tr><td>—</td><td>Clock sync</td><td>Needs outbound NTP and client-to-client ssh on the house network. Verify at the technical rehearsal — a smeared downbeat still plays, it just sounds wrong.</td></tr>
  <tr><td>—</td><td>Workshop Wi-Fi</td><td>Menu Band Jam participants install the app on the day; guest Wi-Fi must allow downloads.</td></tr>
  <tr><td>—</td><td>Two-work event image</td><td>Brief is written; art not yet generated.</td></tr>
  <tr><td>—</td><td>Third MacBook Neo (blush)</td><td>Not purchased. Do not describe its finish in copy until it is in hand.</td></tr>
  <tr><td>—</td><td><em>MacNeoPolitan</em> score</td><td>New work. Not yet written or rehearsed on all three machines.</td></tr>
  <tr><td>—</td><td>Packet PDFs</td><td>No <code>xelatex</code> on this Mac — rebuild on <code>neo</code> or in the devcontainer.</td></tr>
  <tr><td>—</td><td>CultureHub confirmations</td><td>Corrected LOA, event dates, equipment inventory.</td></tr>
</table>

<div class="warn"><strong>Production risk.</strong> One of the three MacBook Neos is <code>neo</code>, the
working dev machine, which is documented as exhausting its 8&nbsp;GB of RAM and hanging
under load. A hang during <em>MacNeoPolitan</em> would stop the piece. Decide now whether
<code>neo</code> performs from a clean boot with nothing else running, or whether a fourth
machine stands by — not on September 23.</div>

<div class="foot">
Whistlegraph presents: <em>Special Sign</em> &amp; <em>MacNeoPolitan</em> · CultureHub Los Angeles,
September 16–25, 2026 · Jeffrey Alan Scudder · Internal program brief, prepared 9 August 2026 ·
Embargoed until CultureHub's formal resident announcement.
</div>

</body></html>"""

out = SP / "culturehub-brief.html"
out.write_text(HTML)
print(f"wrote {out} ({len(HTML)/1024/1024:.2f} MB)")
