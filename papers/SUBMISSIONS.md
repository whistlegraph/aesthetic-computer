# Submissions Ledger

A running record of what has actually been sent where. SCORE.md tracks
draft state; this file tracks submission state. Update both when status
changes.

## Status legend

- **DRAFT** — text exists, not submitted
- **READY** — formatted for the venue, not submitted
- **SUBMITTED** — uploaded, awaiting decision (record date + confirmation #)
- **ACCEPTED** / **REJECTED** / **WITHDRAWN** — terminal states
- **MISSED** — deadline passed without submission; track for next year

## 2026

### Needs immediate verification — deadline already passed

These two had drafts ready before the deadline but submission was never
confirmed. Check both before deciding what's next.

#### ACM C&C 2026 Demos — deadline 2026-04-16

- [ ] Log into the ACM C&C 2026 demos submission portal (PCS or
      whatever the track uses) and confirm whether `cc-demo-2026/demo.tex`
      was submitted.
- [ ] If submitted: record the submission ID + date here.
- [ ] If not submitted: mark **MISSED** and move target to 2027 row in
      SCORE.md.
- [ ] Either way: update the SCORE.md status column from
      "DRAFT READY ... submission status unconfirmed" to a definite state.

#### ICCC 2026 Short Papers — deadline 2026-04-24 (23:59 AoE)

- [ ] Log into EasyChair and check the ICCC 2026 short-paper track for
      a submission of `iccc-kidlisp/iccc.pdf`.
- [ ] If submitted: record the EasyChair paper ID + submission date.
- [ ] If not submitted: mark **MISSED**. ICCC also has an Early Career
      Symposium track with a 2026-05-15 deadline that may be a viable
      pivot — see SCORE.md.
- [ ] Update SCORE.md status column.

### Open windows

| Venue | Track | Deadline | State | Directory |
|---|---|---|---|---|
| SIGGRAPH Asia 2026 | Tech Papers (full) | 2026-05-05 form / 2026-05-12 paper / 2026-05-13 upload | SCAFFOLD | `siggraph-asia-2026-tech/` |
| ICCC 2026 | Early Career Symposium | 2026-05-15 | CANDIDATE | (none yet) |
| ArtsIT 2026 | Full Papers | 2026-06-01 | NEW | (none yet) |
| SIGGRAPH Asia 2026 | Art Papers | 2026-06-08 | NEW | (none yet) |
| SIGGRAPH Asia 2026 | Art Gallery / ET / XR | 2026-06-18 | NEW | (none yet) |
| SIGGRAPH Asia 2026 | Posters | 2026-07-31 | NEW | (none yet) |
| SIGGRAPH Asia 2026 | Real-Time Live! | 2026-08-07 | NEW | (none yet) |
| JOSS | Software paper | rolling | DRAFTS | `joss-ac/`, `joss-kidlisp/` |
