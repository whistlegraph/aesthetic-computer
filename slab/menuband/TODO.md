# Menu Band — TODO

## Always-on recording + a "Reel" menu (idea — not yet implemented)

Instead of the explicit record gesture + count-in, **always be recording the
focused session by default.** The tape already runs a rolling buffer, so we're
"basically always recording anyway" — lean into it:

- Capture every focused session automatically into the rolling tape buffer.
- **Silence-gate it:** when there's a long stretch of silence, pause the
  recording (don't accumulate dead air), and resume on the next note. A take
  that ends up being mostly silence never gets built into a DMG.
- Only **build a take DMG when there's real content** (enough non-silent
  material), rather than on an explicit stop.
- Add a **"Reel" menu** in the menubar that lists all the recordings so far —
  a running shelf of the session's takes you can browse / grab / drag out.

Open questions to resolve when we build it:
- Silence threshold + min-content duration before a take is "worth" a DMG.
- Where takes live while pending (temp) vs. promoted to the Desktop shelf.
- How the Reel menu relates to the on-Desktop DMGs (mirror? source of truth?).
- Privacy/consent framing for always-on mic vs. synth-only capture (mic is
  already opt-in; keep always-on to synth-only unless explicitly armed).

Context: raised 2026-07-15 alongside the take-DMG cover-art / metadata /
countdown work.
