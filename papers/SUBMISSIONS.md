# Submissions Ledger

The ledger is [`submissions.json`](submissions.json). It drives the status
column on papers.aesthetic.computer, so the page and the record cannot drift.
SCORE.md tracks draft state; `submissions.json` tracks where a paper was sent.

To record a change:

1. Edit `submissions.json` — add an entry under `papers.<dir>` for a paper on
   the site, or under `venueOnly` for a venue edition with no public PDF.
2. `node papers/cli.mjs index`
3. Commit and deploy (`ac-deploy`).

Statuses: `draft`, `ready`, `submitted`, `accepted`, `rejected`, `missed`,
`withdrawn`, `unconfirmed`. A paper with no entry is self-published.

Record the evidence in `note` — a confirmation email, a submission ID, or the
absence of one. Submission systems (EasyChair, PCS, Linklings, Fourwaves)
always email a receipt; search mail for the venue before marking `missed`.
