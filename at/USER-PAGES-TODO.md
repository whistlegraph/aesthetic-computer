# AT User Pages - Enhancement Tracker

## ✅ Recently Fixed
- [x] KidLisp module loading (multi-path fallback)
- [x] Fix `sourceMap` scope error in KidLisp previews
- [x] Add local dev server and hosts helper
- [x] Proxy `/media/*` to avoid CORS on piece source fetching

## 🔜 Planned Enhancements
- [ ] KidLisp preview parity with kidlisp.com editor
  - [ ] Blink timing tokens in previews (sync to timer state)
  - [ ] Add execution-flash styling for timing expressions
  - [ ] Add webp preview thumbnails (like kidlisp.com)
  - [ ] Match token colors & font sizing to kidlisp.com
- [ ] Piece source panel
  - [ ] Support embedded `value.source` and `value.sourceCode`
  - [ ] Robust fallbacks when remote source is missing
  - [ ] Improve error surface with a “view source” link
- [ ] Performance polish
  - [ ] Debounce highlight updates for long sources
  - [ ] Cache highlighted output by record rkey

## Notes
- local dev server: `npm run dev:user-pages`
- optional hosts mapping: `./scripts/hosts-helper.fish <handle>`
