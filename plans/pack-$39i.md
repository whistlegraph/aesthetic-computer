# $39i OBJKT Pack Follow-ups

## 🔧 Next
- [] The generated GIF / offline headless background color is weird.
- [ ] Fix this font_1 glyph url https://aesthetic.computer/disks/drawings/font_1/false.json form being loaded for some reason?
- [ ] Track down the zoom buffer size mismatch warning (`288x265`) and align the renderer buffers.
- [ ] Let the Reframe background color follow 'first word' (once (wipe color)) shortcut kidlisp background color or be transparent per piece. (remove the hardcoded purple fallback).

## Later
- [ ] Investigate CSP/connect-src blockers that prevent runtime fetches during `ac-pack` and patch the packer to inline or proxy required assets.
- [ ] Add a regression check so future $piece headers keep multiline KidLisp formatting.

## Done
- [x] Format the KidLisp source block in `headers.mjs` to insert readable newlines and indentation.
  - [!] There are extra new line sbeing added and the original spacing is not respected... (I dont want a 'reformatter')
- [x] Remove the noisy "Found OBJKT cached code" console log from the KidLisp runtime.
- [x] Suppress repetitive MatrixChunky8 glyph load errors in the packaged viewer (console error + window error filters).
- [x] Ensure all MatrixChunky8 glyph JSON files ship inside the bundle so the viewer never issues 404s.
- [x] Trim or gate remaining asset/glyph debug logging behind a quiet or verbose flag in `ac-pack`.