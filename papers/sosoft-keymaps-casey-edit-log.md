# Casey Reas copy-edit log — *The Keymap Is the Score*

This log records the source changes made from Casey Reas’s anchored Google Doc
comments before republishing the Social Software article. The reviewed document
was titled **“Hi — I’m Jeffrey.”** The final source is
[`sosoft-keymaps-doc.md`](sosoft-keymaps-doc.md).

## Comment-by-comment changes

| # | Casey’s concern | Source change | Why it resolves the note |
|---:|---|---|---|
| 1 | “Surfaces” was too niche in the opening implementation description. | Changed it to “software and hardware platforms.” | Uses ordinary language while still covering the browser, native OS, and menu-bar versions. |
| 2 | “I didn’t port it so much as carry it, the way you’d carry a tune” felt too LLM-written. | Replaced the flourish with two direct sentences: each implementation changed while the keymap stayed the same. | Keeps the portability idea without decorative analogy. |
| 3 | “The program I’m writing this from” could be confused with a computer program. | Changed it to “the Social Software initiative at UCLA, where this essay began.” | Names the institutional context without overloading *program*. |
| 4 | Ableton Live, GarageBand, and Logic Pro needed context for general readers. | Introduced them as “three popular audio apps.” | A reader no longer needs prior product knowledge to understand the category. |
| 5 | “It eats both hands” should be more literal. | Changed it to “It requires both hands to cover a single octave.” | States the usability problem plainly. |
| 6 | The desire-path comparison conflicted with the example’s negative framing. | Removed the desire-path and *Calf-Path* passage. Replaced it with a short account of repetition and learned inertia. | Avoids using a usually productive bottom-up design pattern as a metaphor for a bad convention. |
| 7 | AWSED’s relationship to QWERTY ergonomics needed attention. | Added the staggered rows, handedness, and available chord shapes to the list of physical information AWSED ignores. | Makes the critique about the actual board and hands, not only printed letters. |
| 8 | Vim needed a short introduction. | Changed “Vim has both” to “The Vim text editor has both.” | Defines the example at first use in the argument. |
| 9 | “The music-app staircase” was an unclear back-reference. | Replaced it with the named object: “AWSED has the rule but no good way to pass it on.” | Uses the essay’s own term consistently. |
| 10 | “And not only people read these days” could be read ambiguously. | Changed it to “People aren’t the only ones reading these days.” | Preserves the transition to machine readers with a clear subject. |
| 11 | “This page” was vague. | Changed it to “This essay accompanies the first numbered print edition…” | Names the object precisely. |
| 12 | Calling the edition “its own little act of social software” needed explanation. | Reframed the paragraph around the shared agreement: every participant made sixty-four copies, met in one room, and assembled sixty-four complete publications. | Shows the social mechanism instead of merely labeling it. |
| 13 | The interactive keymap needed an instruction. | Added: “Press the letter keys in the keyboard below to hear the layout for yourself.” | Gives the embedded keyboard a direct prompt. |

## Direct suggestion audit

The Docs API exposed twelve suggestion records in addition to the thirteen
comments. The final source includes Casey’s suggested “publication,” “at UCLA,”
“owns this decision,” Social Software Cycle introduction, “I didn’t change…
already a score,” parenthesis removal, “single publication,” and contributor
title corrections. Later source verification superseded only wording that
needed a fuller resolution or official title casing.

## Additional source corrections made in the same pass

- Kept the user’s requested slogan as **“AWSED is AWFUL.”**
- Kept the AWSED description explicit about stagger, chord shapes, and
  handedness.
- Used **The Getty Scores Project** in full and linked it directly.
- Corrected the edition roster to match the project catalog:
  - Em Lugo — *Cues for Losing Direction*
  - Darlyn Phan — *Line Piece 1*
  - Banyi Huang — *A Cosmographic Score for Folding Back into the Kernel*
- Removed the inaccurate generic “play it for yourself below” sign-off and
  replaced it with Casey’s concrete keyboard instruction.
- Added durable links for notepat, Aesthetic.Computer, the paper, Vim, the
  three audio apps, Social Software, Casey Reas, Lauren Lee McCarthy,
  *do it*, and The Getty Scores Project.

## Walkthrough-video feedback incorporated alongside the text

- Corrected Æther Cavendish’s *Vigil Score* from the inaccurate “black square
  over white printed pages” description to “a matte-black folded packet,
  closed with a small circular silver seal.”
- Replaced the limiting “every contribution asks the same question” close with
  “Together, the contributions open many paths through a question…”
- Replaced the provisional “receipt-like sheets” description of Jordan
  Silver’s *Sonic Architecture* with the observed single-sheet layout,
  including printed columns, large numbers, typewritten commands, and the
  looping spiral diagram.

## Publication checks

- [x] All thirteen Google Doc comments and twelve suggestion records accounted
  for in source.
- [x] Obsolete phrases absent from source.
- [x] Edition roster reconciled with the project catalog.
- [x] Markdown and video-recovery scripts pass syntax/whitespace checks.
- [x] Framer article updated from the final source through the Server API.
- [x] Public URL checked after production deployment `ab007e478`.

## Publication evidence

- Staging deployment: `58ce73b28`
- Corrected staging deployment after full suggestion audit: `bfb0a89bd`
- Corrected production deployment: `48c12600d`
- Published URL: <https://sosoft.arts.ucla.edu/keymaps-as-social-software/>
- The production HTML contains every required Casey-derived phrase and corrected
  contributor title; all obsolete phrases in the audit are absent.
- The Framer staging page rendered correctly in visual QA. The UCLA proxy’s
  previously documented blank headless-render behavior remains, so the public
  deployment was additionally verified from its returned HTML.
- Drive API receipt: all 13 Casey comments now have a change reply and are
  resolved; the API reports 13 resolved and 0 unresolved.
- The document’s 12 suggestion records were audited through accepted/rejected
  preview views. API acceptance is currently limited to Google’s Workspace
  Developer Preview and was not available to this project, so no hidden or
  unsupported browser automation was substituted.

## Casey review handoff

- Review video: <https://drive.google.com/file/d/17jMTkVX7OCekLxKq8X1-4iRLrWzMA0E5/view>
- Timecoded screenplay receipt: <https://docs.google.com/document/d/1SaHuqqetIFoDhfx3YT3aqoxYSBSh2VVHLhBjZAO3_u4/edit>
- Final article source + this edit log: <https://docs.google.com/document/d/1hNzUm3SmsEBRtM3zWhcQqsYvsoRf4ZioFIQMFndlwXY/edit>
- Drive API access verified for `reas@ucla.edu`: commenter on both Docs and
  reader on the review video. Google’s automatic notification email was
  intentionally disabled so the Signal handoff remains the single notification.
