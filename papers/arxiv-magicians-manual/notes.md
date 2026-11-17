# AC '26 Magician's Manual — notes

Consulted on 2026-11-16 against core revision 7dc6a12d2b (working tree).

## Occasion
A new handle asked in `chat` (2026-11-17 00:40 UTC) whether a tutorial exists.
Nobody had answered. @jeffrey asked for "an AC '26 Magicians Manual". The
quotation in section 1 is that public chat message with two typos corrected
and no handle attached.

## Platter and prior papers
- papers/SCORE.md (request contract; title-page model from arxiv-keymaps).
- papers/VOICE.md.
- papers/arxiv-dool-geography (preamble and source-bundle pattern reused).
- papers/arxiv-ac (the system paper this manual sits beside).
- papers/arxiv-kidlisp-reference.

## Sources read
- USER-GUIDE.md (three recipes; return keys; share; sign-up steps).
- system/netlify/functions/docs.js, `prompts` and `pieces` blocks: 230 entries
  not marked hidden. Every spell in the tables comes from this list.
- lib/text.mjs `validateHandle`: `^[a-z0-9]+([._][a-z0-9]+)*$`, max 16.
- lib/mime-sheet.mjs header: curtain swipe, two-finger scroll, down arrow.
- lib/dool-sheet.mjs header: crosshair tap; Escape, corner block or Back to leave.
- disks/prompt.mjs: `@ $ # !` sigil autocomplete; tape slugs.
- shared chat capabilities (the chat MCP's syntax table) for sigils in chat.

## Not verified
- Nobody followed the manual start to finish in a browser.
- The wrong-word helper was read in prompt.mjs and not run.
- `(wipe blue) (ink yellow) (circle 64 64 30)` was read against kidlisp.mjs
  (functions exist) and not run.
- `tone:sine 440`, `word hello`, `fill blue`, `resize 128`, `scream hello`
  take their parameter shapes from docs.js signatures and USER-GUIDE.md, not
  from a run.
- The Firefox remark is one newcomer's public chat report about one old phone.

## What people open (added 2026-11-17 UTC, after @jeffrey asked whether the manual follows what people type)
- analytics handle_usage, 840 h ending 2026-11-17 01:02 UTC, studio scope: 131 handle
  rows, 113 with recorded account activity. Table 1 counts, per piece, how many of
  those handles opened it at least once (`activity.pieces`). `sotce` (32) is another
  product and is left out; `support` (8), `handle` (11), `camera` (4) were cut for space.
- analytics piece_landings, same window, aesthetic.computer: laer-klokken 2,240 loads,
  laklok 308, r8dio 60, notepat 50 (35 anonymous), handle 43, list 37. Not in the paper.
  starfield (2,885 anonymous loads, all referred from the site itself) and roughly
  100 to 180 anonymous loads each for pieces starting with a digit or "a" look
  automated; that is my reading, not a measured fact.
- The section order was turned around on this evidence: people before paint and play.

## Wrong words
- disks/prompt.mjs: an entry that matches nothing falls through to the help
  character (`before` program, lines 47-76): "Did you mean 'command'?", a one-sentence
  answer to a question, or a pointer to `list` or `chat`. Read, not run. Whether it
  answers signed-out visitors was not checked.
- lib/disk.mjs ~12897: backtick, Enter, Backspace and Escape return to the prompt,
  except in chat, laklok, laer-klokken, mail and a few others.
- disks/danish.mjs and sisters: translator pieces.
