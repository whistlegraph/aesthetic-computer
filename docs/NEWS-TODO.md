# Aesthetic News TODO

## UI/UX Improvements

### "Report a Story" (Submit Redesign)
- [ ] Rename "submit" to "report a story"
- [ ] Combine submit page and guidelines into one unified page
- [ ] Shorter, AC-style copy for posting guidelines
- [ ] Less Hacker News vibe, more aesthetic.computer voice

### Comment Guidelines
- [ ] Split current guidelines into two pieces:
  1. Story submission guidelines (shown on "report a story" page)
  2. Comment guidelines (shown when leaving a comment)
- [ ] Keep both short and in AC voice

### Commits Link → Modal
- [ ] Commits link should open a modal instead of navigating away
- [ ] Use same modal pattern as `/at` user page links
- [ ] Modal shows commit URL content

---

## Copy Direction

Current guidelines feel too "Hacker News". Rewrite to match AC tone:
- Playful but clear
- Concise
- Community-focused
- Less corporate/formal

---

## Reference

- Modal pattern example: `/at` user pages (see `system/public/aesthetic.computer/disks/at.mjs`)
- News API: `system/netlify/functions/news-api.mjs`
- News frontend: served via extension webview at `news.aesthetic.computer`
