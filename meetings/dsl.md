# whistlepop DSL

The grammar by which jeffrey's whistles, mid-conversation, instruct the
`meetings/` pipeline. The pipeline strips whistlepops from the conversation
transcript and applies them to the surrounding context when building the PDF.

## Primitives

### Lone whistle — punctuation

A single whistle with no matching partner within `pairWindowSec` (default 8s)
acts as **structural punctuation**:

| Form     | Effect                                        |
|----------|-----------------------------------------------|
| short    | section break (`\section{}` with auto title)  |
| medium   | subsection break                              |
| long     | "beat" — extra vertical space, no header      |

Auto-titles for section breaks come from the next 6–10 transcribed words
(WhisperX timestamps) trimmed at the first noun phrase. Override with a
paired whistlepop containing `section <name>`.

### Whistlepop — paired

Two whistles within `pairWindowSec`, with speech in between, form a
**whistlepop**. The speech inside the brackets is a *directive*, not
conversation. It is parsed against the grammar below.

```
…regular talk… [WHISTLE] highlight [WHISTLE] …regular talk…
```

The renderer strips the bracketed directive from the body, and applies the
directive to the *surrounding* context (typically the sentence immediately
before or after the open whistle — see "anchor" below).

## Directive grammar

Directives are case-insensitive. The parser tokenizes the directive text and
matches the first token against the verb table; remaining tokens are
verb-specific arguments.

| Verb              | Anchor       | Effect                                              |
|-------------------|--------------|-----------------------------------------------------|
| `highlight`       | prev sent.   | wraps the previous sentence in a callout box        |
| `decision`        | prev sent.   | pull-quote with "Decision" label + margin glyph     |
| `action <person>` | prev sent.   | "Action item — <person>" callout                    |
| `quote`           | prev sent.   | render as block-quote with margin treatment         |
| `section <name>`  | here         | `\section{<name>}` starting at this point           |
| `subsection <n>`  | here         | `\subsection{<n>}`                                  |
| `note <text>`     | margin       | margin note attached to the open-whistle anchor     |
| `skip`            | bracketed    | mark surrounding ±10s as off-record (omit from PDF) |
| `redact <span>`   | bracketed    | as `skip`, but for a named span / topic             |
| `start`           | open         | begin a highlight range                             |
| `end`             | close        | end the highlight range opened by a prior `start`   |
| *(unmatched)*     | margin       | render the directive text as a freeform margin note |

### Anchor semantics

- **`prev sent.`** — verb applies to the sentence whose end timestamp is
  closest to (but before) the open-whistle timestamp. Falls back to the
  next sentence if no prior sentence is within `anchorWindowSec` (3s).
- **`here`** — applies at the open-whistle timestamp, splitting the
  transcript at that point.
- **`margin`** — attached at the open-whistle anchor but rendered in the
  margin without altering body flow.
- **`bracketed`** — applies to the time range between the two whistles
  (and an optional `± padSec` slop).

### `start` / `end` ranges

A `start` directive opens a highlight range that continues until the next
matching `end` (or `end <name>` for named ranges). Useful for "highlight
this whole next paragraph" instead of one-sentence callouts:

```
[WHISTLE] start [WHISTLE]   …a couple of minutes of important talk…   [WHISTLE] end [WHISTLE]
```

Renderer shades the bracketed range with a soft tint and a margin bracket.

## Detector → parser handoff

`meetings/detect-whistlepops.mjs` emits:

```json
[
  { "t_start": 12.34, "t_end": 12.49, "kind": "short",   "confidence": 0.91 },
  { "t_start": 15.02, "t_end": 15.21, "kind": "short",   "confidence": 0.88 }
]
```

`meetings/parse-directives.mjs` does two passes:

1. **Pair pass** — group events within `pairWindowSec`. Each pair becomes a
   whistlepop with `directiveText` taken from the WhisperX transcript span
   between the two events. Unmatched events become `lone` punctuation.

2. **Verb pass** — tokenize each directive's text against the verb table,
   resolve anchors, emit a normalized `directives.json`:

   ```json
   [
     { "type": "highlight", "anchor": "prev", "t_open": 12.34, "t_close": 12.49 },
     { "type": "section",   "name": "Budget", "t": 84.10 },
     { "type": "freeform",  "text": "ask alex about the timeline", "t": 142.7 }
   ]
   ```

## Open questions (v2)

- Should `rising` / `falling` two-tone whistles get distinct verbs
  (rising = `open`, falling = `close`) so pairing is explicit instead of
  inferred by proximity? The corpus already includes them.
- Should `quiet` whistles be treated as private margin notes (different
  visual style from regular whistlepops)?
- Should whistlepops nest? (`[WHISTLE] start [WHISTLE] talk [WHISTLE] decision [WHISTLE] more talk [WHISTLE] end [WHISTLE]`)
