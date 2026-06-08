# Serpentine FAE — Video Introduction (≤3 min) — MOCK / STORYBOARD

> Built on the **recap / "aesthetic 24"** pipeline (`recap/cli.mjs` +
> `recap/audience/*.mjs`) — the same segmented, markered, narrated machine
> behind the daily 24-hour update videos. Voice: **jeffrey-pvc** (ElevenLabs,
> via `/api/say`). Illustration: AC recap chrome + per-segment illustrations.
>
> Runnable config: `recap/audience/serpentine.mjs`.
> Render: `cd recap && node cli.mjs build serpentine`  *(heavy — see §Render.)*
>
> **Form prompt it answers:** "Introduce yourself, your background, and your
> practice. Include how you work and what brings you to this Fellowship."
> Their note: *clarity and sincerity, not production value.* So: your voice,
> your face, your machines — not a slick promo.

## Shape

~2:50, nine narrated segments + a tail. Narration is one continuous
jeffrey-pvc take (lowercase, warm, a little funny); markers cut the visuals to
it. The argument is the proposal's spine, spoken: **personal computing is being
pulled into the data center → it needs an OS revolution → AC is a creative OS +
commons + laptop orchestra → language models are both the pull into the data
center and a faculty AC can run locally, on its own terms → here's the six
months.**

## Segment board

| # | ~t | marker (verbatim in narration) | beat | visual — AC illustration style |
|---|----|--------------------------------|------|-------------------------------|
| 01 | 0:00 | `i'm jeffrey scudder` | who you are | Title card: "aesthetic computer" kicker, **commons** huge, AC palette; a hand-illustrated jeffrey at a citrus-green Neo laptop |
| 02 | 0:18 | `here's the thing` | the problem | Illustration: a laptop dissolving into a glowing data-center hall; thin wire tethering it — "thin client" |
| 03 | 0:40 | `home computing needs` | OS revolution | Terminal-green title: "a revolution at the OS layer"; an old beige PC re-lit green |
| 04 | 0:52 | `so that's what i built` | the creative OS | Screen-capture vibe: a surplus ThinkPad cold-booting to the AC instrument in ~7s, name on screen |
| 05 | 1:18 | `and it's a commons` | commons + orchestra | A grid of surplus laptops played together as a planetary orchestra (PLOrk lineage); their screens show the range AC holds — a JS piece, a KidLisp piece, a painting, a tape, a chat — never all the same |
| 06 | 1:48 | `language models change` | LLMs: pull + faculty | Split illustration: data-center pull vs. a small local model living *inside* the laptop, on your terms |
| 07 | 2:10 | `can answer that` | AC can answer | The whole AC stack as one legible diagram — kernel → languages → community → archive |
| 08 | 2:24 | `with the fellowship` | the six months | Three-panel: harden the OS · local AI faculty · grow the orchestra (NELA), all "documented in the open" |
| 09 | 2:42 | `that's my art and convergence` | outro | Back to the title card; jeffrey + Neo; the question on screen |
| 10 | 2:50 | `__END__` | tail | hold + AC wordmark, 3s |

## Narration (verbatim — this is what gets spoken & POSTed to /api/say)

> hey, i'm jeffrey scudder. i'm a painter, and for six years i've been writing
> software and patterning for my own computer — a place to make things and give
> them away. it's called aesthetic computer.
>
> here's the thing i can't stop thinking about. as the demand for ai compute
> pulls everything toward the data center, your laptop is slowly turning into a
> thin client — a window onto someone else's machine, rented by the month. the
> consumer operating system already keeps your identity in a cloud account, and
> that finishes the job. the personal computer, the one you actually own, is
> quietly disappearing.
>
> i think home computing needs a revolution at the operating-system layer. not
> another app — a different ground to stand on. i've spent six years building
> one.
>
> so that's what i built. aesthetic computer native os boots a surplus laptop —
> a fifty-dollar thinkpad the upgrade cycle threw away — straight into a
> creative instrument, in about seven seconds. your identity lives in the boot
> partition, not a cloud login, so it greets you by name.
>
> and it's a commons. you make pieces in javascript or in kidlisp, a tiny
> language built for it, and the same system holds paintings, tapes, chats, and
> everything else people make — each living at its own web address. twenty years
> ago the princeton laptop orchestra proved a room of laptops could be a real
> instrument, but at fifteen hundred dollars a seat it stayed locked inside
> wealthy universities. on surplus hardware, that orchestra can be planetary,
> and held in common.
>
> now, language models change all of this. they're what's pulling computing into
> the data center — but they're also the reason one artist and a small community
> can now build and tend a whole operating system that once took a company. so
> the question i want to live inside for six months is this: can a creative os
> take language models in as local, owned faculties — running on your own
> machine, on your own terms — instead of turning your computer into a tenant of
> the cloud?
>
> i think aesthetic computer can answer that, because it already holds the whole
> stack — the kernel, the languages, the community, the archive.
>
> with the fellowship, i'd harden the os into a daily-usable personal computer,
> prototype local ai as a faculty rather than a landlord, and grow the laptop
> orchestra with my community here in los angeles — documenting all of it in the
> open, the way aesthetic computer already publishes everything.
>
> that's my art and convergence question: as computing converges on the data
> center, what does it take to keep a computer personal — and held in common.
> thanks for watching.

**~445 words ≈ 2:45–2:55 at jeffrey-pvc's pace.** (Trim the orchestra or
six-months sentence if it runs past 3:00.)

## Per-segment illustration prompts (AC style, if using gpt-image-2 / jeffrey-photos)

Consistent with the recap illustration rules (candid iphone-snapshot tone, no
motion blur, citrus-green MacBook Neo or plain black ThinkPad, green-on-black
terminals, jeffrey head engaged with the screen, never recursive screens):

1. jeffrey at a desk in LA, citrus-green MacBook Neo open (a torn white-paper scrap over the apple logo with a hand-penned whistlegraph butterfly), green-on-black terminal, warm morning light, head angled into the screen.
2. a single laptop on a plain table, a thin glowing cable tethering it to a distant data-center hall behind glass; cool blue server light vs. warm room.
3. a retired beige PC tower re-lit in terminal green, hand-lettered "OS" on a paper scrap.
4. close on a refurbished black ThinkPad screen mid-boot, a name appearing, AC pink boot splash.
5. a long table of mismatched surplus laptops, different people playing them together like an ensemble; each screen shows something different — a few lines of JavaScript, a KidLisp piece, a painting, a tape still, a chat — a planetary orchestra holding many kinds of work.
6. split scene: left, a vast data center pulling light inward; right, a small friendly model glowing *inside* the laptop on the desk.
7. a clean hand-drawn diagram on paper: kernel → languages → community → archive, jeffrey's hand pointing.
8. triptych on a studio wall: a booting laptop, a small local-AI sketch, the NELA Computer Club room full of people.
9. jeffrey closing the citrus-green Neo, half-smile, the question hand-lettered above.

*(Or skip gpt-image entirely and run text-only AC-palette slides — faster, free,
and honestly closer to "sincerity not production value.")*

## Render

```bash
cd recap
node cli.mjs build serpentine
# pipeline: tts (jeffrey-pvc) → transcribe (whisper) → align (markers) →
#           [jeffrey-photos illustrations] → slides → subtitles → compose (ffmpeg)
# output: recap/out/recap.mp4
```

**Cost / weight (8 GB machine — run sequentially, nothing else heavy alongside):**
- ElevenLabs `/api/say` once for the narration (cached by text hash).
- whisper-cli once (~90s) for word timing.
- ffmpeg compose (libass required).
- *If* using gpt-image illustrations: 9 generations (cap 3 parallel per the
  machine rule) — the only real API cost. **Recommend starting text-only** to
  see timing/cut, then add illustrations on a second pass.

**⟨confirm before I run it⟩** — it hits the ElevenLabs API and (optionally)
gpt-image. Say "render it" and I'll do a **text-only first pass** so we can
check the cut cheaply, then decide on illustrations.
