# Harness factory — a 90-minute workshop

By the end, every person has written a **harness**: a small tool that takes
something in (words, a photo, a file, or several at once) and makes one kind
of media, named by its MIME type. They have run it, changed it after watching
it fail, and posted what it made to [mime.ac](https://mime.ac) under their
@handle.

That is also the idea of Aesel itself: you are always making *a kind of
media*, and the harness says which. Claude Code, Aesel and every other agent
tool are the same shape, only bigger: a model, a prompt that says what it
knows, inputs, an output format, and checks.

## What a harness is here

```
my-harness/
  harness.json   { "name": "my-harness", "inputs": ["text", "image"], "output": "image/svg+xml" }
  prompt.md      what it knows and how it decides (the part you write)
  check.mjs      optional: export default ({ text }) => [problems]
  runs/          every run: inputs, the model's answer, the file it made
```

| inputs | what you pass |
| --- | --- |
| ✎ `text` | words on the command line |
| 🖼 `image` | `--image photo.jpg` (png, jpg, heic, webp; sent at 768 px max) |
| 📄 `file` | `--file notes.md` (any text file up to 64 KB) |

| output goal | becomes | how |
| --- | --- | --- |
| ✎ `text/plain` · 📝 `text/markdown` · 📊 `text/csv` · 🧾 `application/json` | words, documents, tables, data | the model writes it |
| 🌐 `text/html` | a web page that runs from disk | the model writes it |
| 🖼 `image/svg+xml` | a vector picture | the model writes it |
| 🎨 `text/x-lisp` | a KidLisp piece | the model writes it; paste it into aesthetic.computer |
| 🎮 `text/javascript` | an Aesthetic Computer piece | the model writes it; `ac publish` it |
| 🖼 `image/png` | a painted picture | the model **art-directs**, an image model paints |
| 🔊 `audio/wav` | a sound | the model **composes a score**, your laptop synthesizes it |

The last two are the lesson inside the lesson. A language model can't emit
pixels or audio samples. A harness gets there by choosing an intermediate
form the model *can* write, then turning it into the media.

## Before class (instructor)

- [ ] Everyone has an aesthetic.computer account with a claimed @handle.
      Hosted inference is paid in braincells on each handle; check that the
      daily allowance covers ~20 runs of Claude Sonnet 5.5 per person, or top up.
- [ ] `image/png` harnesses get **5 hosted images per handle per day**. Demo
      `sticker` yourself; let students choose it knowingly.
- [ ] Try the room's Wi-Fi with `ac harness run` before class. Each run makes
      one request; a dropped connection is retried once.
- [ ] Put a few test photos on the class drive for anyone without one.

## The route

**0:00 — Install and sign in (10 min)**

```sh
curl -fsSL https://prompt.ac/aesel.sh | sh
ac login
ac whoami            # → @yourhandle
```

**0:10 — Run someone else's harness (10 min)**

```sh
mkdir workshop && cd workshop
ac harness list                         # the starters, inputs → output goal
ac harness new poster --from poster
ac harness run poster "rooftop karaoke, friday, 9pm"
open poster/runs/*/output.svg
```

Everyone runs the same starter on their own event. Put three on the
projector. Same harness, same model, different results: why?

**0:20 — Read a harness (10 min)**

Open `poster/prompt.md`, then look at what actually went to the model:

```sh
ac harness run poster "rooftop karaoke" --dry
```

The dry run shows the full system prompt (your `prompt.md`, then the
contract the output goal adds) and spends nothing. Point at the seam: *you*
write taste and judgement; the factory adds the format.

**0:30 — Change one thing (15 min)**

Edit `prompt.md`: a new rule, a palette, a constraint, a persona. Run it
again. Compare the two `runs/` folders. Each keeps `answer.md`, the model's
full reply.

Pair up: swap event descriptions, run each other's harness.

**0:45 — Make your own (25 min)**

Pick an input × output pair that interests you and that no starter does:

```sh
ac harness new receipt-poem --in image --out text/plain
ac harness new mood-chord   --in text  --out audio/wav
ac harness new fridge-recipe --in image,text --out text/html
ac harness new diary-chart  --in file  --out image/svg+xml
```

`new` writes a `prompt.md` skeleton with the inputs and goal filled in.
Write it, run it, and run it again with an input meant to break it.

Starters to borrow from:

| starter | in → out |
| --- | --- |
| `haiku-cam` | 🖼 image + ✎ text → ✎ text/plain (has a `check.mjs`) |
| `poster` | ✎ text → 🖼 image/svg+xml |
| `zine` | ✎ text + 🖼 image + 📄 file → 🌐 text/html (multimodal) |
| `hum` | ✎ text → 🔊 audio/wav |
| `kidlisp` | ✎ text → 🎨 text/x-lisp |
| `sticker` | ✎ text + 🖼 image → 🖼 image/png |

**1:10 — Add a check (10 min)**

A check is how a harness gets stricter without retraining anything. Copy
`haiku-cam/check.mjs`:

```js
export default function check({ text }) {
  return text.length > 280 ? ["Keep it under 280 characters."] : [];
}
```

Whatever it returns goes back to the model as a problem, and the model gets
one try to fix it. The terminal shows `repairing: …` when that happens.
Every output goal already has built-in checks (JSON parses, SVG is complete,
KidLisp's parentheses balance, a score plays).

**1:20 — Post and show (10 min)**

```sh
ac harness post my-harness                    # newest run → mime.ac
ac harness post my-harness "caption" --run 2026-…
```

The post goes on the board for its MIME type, so the class's SVGs end up
together, the WAVs together, and so on. Open mime.ac on the projector and
walk the boards.

## Going further

- `--model` tries the same harness on another hosted model
  (`deepseek/deepseek-v4.1-flash` is much cheaper; compare the results).
- An `image` block in `harness.json` picks the image model:
  `{ "provider": "openai", "model": "gpt-image-2", "quality": "medium" }`.
- A harness is just files: put it in git, share it, fork a classmate's.
- Aesel's own harness lives in `aesel/harness/*.md`, the same idea at full
  size, and `ac run --init-harness` copies it for editing
  (see `aesel/docs/harness2.md`).

## Limits worth saying out loud

- Hosted inference reads words and pictures. Audio, video and PDFs can't go
  *in* yet; a `file` input takes text.
- `image/png` outputs count against 5 hosted images a day.
- A `.wav` is at most 60 seconds; the synth is plain (sine, square, saw,
  triangle, noise) on purpose, so the score carries the music.
