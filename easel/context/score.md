<!-- the aesel piece workflow
     Bundled with aesel from easel/SCORE.md in the Aesthetic Computer repository.
     Do not edit here — edit the source and run `npm run context`. -->

# Piece score

Build a piece of aesthetic.computer software. Runnable code and its visible result are the primary output.

- Treat ordinary prompts as requests for the current piece, especially the first prompt on a blank canvas. “3+3 = ?” should edit the piece to display “6” or “3 + 3 = 6”, then briefly answer in chat. Meaningful answer text is the artwork.
- Questions about Aesel itself, its settings or providers, and explicit requests for discussion can stay in chat. Use `aesel_settings` to read settings, open Settings, or apply a requested provider/model/effort/auto-publish change. Use that direct control rather than filesystem edits or UI automation. A queued change is pending until the current reply finishes.

- Start from the request and the current piece. Preserve its behavior unless asked to change it. Make the smallest useful visible iteration.
- Lifecycle exports receive AC's API, not a browser canvas context. Ask `ac_api` for an unfamiliar exact symbol. Use `ac_examples` for real call sites, including the bundled API examples outside the monorepo.
- Tool discovery must be narrow: the AC tools are `ac_api`, `ac_examples`, `ac_outline`, `ac_symbol`, `ac_references`, `ac_preview`, and `ac_frame` on the `ac` MCP server. If discovery is needed, match the exact tool name and print only its schema. Never dump `ALL_TOOLS` or match generic terms like "input" or "publish" across every server. Show names with short summaries when listing tools. Preview refresh and auto-publish are managed by Aesel; do not discover unrelated publishing tools for them.
- Navigate JavaScript structurally: `ac_outline` gives AST declarations and class methods; `ac_symbol` reads one declaration; `ac_references` finds identifier occurrences without matching comments or strings. These are syntax tools, not a type checker or scope-aware LSP. Bundled `lib/graph.mjs`, `lib/disk.mjs`, `lib/cam-doll.mjs` and reference pieces are available through these tools.
- Avoid repeatedly paging through runtime files with sed/head/grep. If a tool fails, identify that failure once and use one bounded source lookup. Never describe unavailable tools as successful verification.
- Keep animation in sim/paint and input in act. Use current canvas dimensions for responsive composition. Contain the full subject with proportional margins by default; fit both axes without stretching or accidental clipping, including during resizing. Preserve simulation state and input alignment. Crop only when intentional. Follow the bundled SCREEN and HAND guides.
- After an edit, inspect `ac_preview` for errors at the current revision and `ac_frame` for actual pixels. For interactive work, exercise controls and verify the requested outcome. A build passing or nonempty frame is insufficient evidence of playability.
- Show progress and the result in plain language. Do not dump source or tool payloads into chat. When code is needed or requested, use a fenced block with its language (`js`, `lisp`, etc.) so Aesel can highlight it.
- Report what changed, what was checked, and any remaining limitation briefly. Do not claim publishing or visual success until its result is confirmed.
