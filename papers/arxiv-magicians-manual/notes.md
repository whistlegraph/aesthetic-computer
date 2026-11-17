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

## Second edition: Aesthetic Spells (2026-11-17 UTC)
- @jeffrey asked for a new manual built on the manual-platter research and retitled it
  "Aesthetic Spells (and other typings for use on prompt.ac)" after trying "AC Spells"
  and "the Aesthetic Spellbook".
- Form: 12 pages at 5.5 x 8.5 in, one column with an outer margin. Borrowings are
  listed in the book's own table and in ../manual-platter/digest.md.
- New facts read in code for this edition: Tab at the empty prompt opens dool
  (prompt.mjs, `keyboard:down:tab`); Escape stops a tape before leaving (disk.mjs);
  404.mjs paints "Piece Not Found" on teal; laklok began as the Danish room
  (docs: "Learn the 'clock'!"), stated from the room's own chat.
- From working knowledge, not checked: that chat needs a handle to post; that tape
  clips and finished paintings get a `!code` / `#code`; that `profile` shows "what a
  handle has made and said"; juke as "every pop track".
- The directory and site name keep `magicians-manual`; renaming them moves the
  published URL.
- Not committed. The one-sheet first edition is what is live.

## A to Z edition and the browser test (2026-11-17 UTC)
- @jeffrey: "the spellbook now should be organized by spell", "with an index", "we should be
  testing all these flows", "e2e", clips to watch, pixel density lower, audio recorded,
  a spells listing "so we can approve them".
- spells.json is the source; gen-entries.mjs writes entries.tex, index.tex, tally.tex and
  review.html. e2e-summary.json is the trimmed result of tests/browser/spellbook.e2e.mjs.
- Three runs on panda (poorslice was down), signed out, against production:
  two at 1100 x 760 and one at 660 x 460 at 2x with video and Web Audio taped. All three
  agreed on every address and return key.
- Read by eye from screenshots and the paint clip, not by the script: the helper's
  replies, every painting step, the tape landing in /video, the 404 page.
- Findings the book now states: notepat keeps Esc, Backspace and backtick; edit leaves
  for the VS Code Marketplace; signed out, profile, me, mood, handle and bye show nothing;
  rect opens as "box"; tape ends in the video piece; the helper answered notepta with
  'notifs'; the down arrow did not open the mime feed in any run.
- Not cast: done, publish, delete-erase-and-forget-me (they publish or delete), !code
  (no code known). print, mug and scream are not in the book.
- Not tested at all: anything signed in; dark and light (no visible check); the corner
  tap as the way out of notepat.
- clips/ (72 mp4, 18 MB) and review.html are local and gitignored.
