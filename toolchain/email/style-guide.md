# Email Style Guide

default_signature: @jeffrey
force_lowercase: true
append_signature_if_missing: true

## Defaults

- keep subjects and body copy all lowercase by default
- URLs are never lowercased — Drive file ids and similar are
  case-sensitive, and a lowercased link is a dead link
- sign emails as `@jeffrey`
- append signature automatically if missing

## Overrides

- set `preserve_case: true` in `mail_send` when casing must be preserved
- set `signature` in `mail_send` to override the default sign-off for a specific email

## Register — when to override the lowercase default

The lowercase voice is for **internal, personal, peer, and Fia threads.**
Switch to proper caps + punctuation when the recipient is a **professional
collaborator** — external research lab, client, vendor, institution,
grant officer, deliverable-bearing first impression.

In the proper-caps register:

- Sentences start with capitals; proper nouns capitalized (Jeffrey, Dask,
  Linux, Allen, Google Drive).
- Tech / product / project names keep canonical casing (brainreg,
  atlas.roi — software stays as its authors style it).
- Acronyms uppercase (MIPs, ROI, OME-XML, AP/ML/DV).
- Code identifiers in backticks stay verbatim.
- Sign with the capitalized first name ("Jeffrey"), not the lowercase
  handle, unless the thread has already established the lowercase voice.

Set `preserve_case: true` in `mail_send` for these.

## Closers — no motivation, no hype

Cut motivational and enthusiasm-filler lines from the draft. Examples to
strip before sending:

- "excited about this"
- "looking forward"
- "this is going to be great"
- "happy to help"
- "let me know if that works for you" / "let me know your thoughts" /
  any permission-seeking or confirmation-fishing tag — state the fact and
  stop; if a real question exists, ask it directly
- generic trailing "thanks!" when there's nothing being thanked for

End on the last functional sentence + signature. Concrete next-step
phrasing ("send the JSON back") is information, not hype — keep that.
The substance carries the energy; performed enthusiasm reads as filler
and undercuts the work.

## Drafts and authorization

Sending is the irreversible step, so it has two lanes. Revised by
@jeffrey 2026-09-11: the previous rule demanded a second "send it" for
every email, including ones he had just asked for by name, which turned
a one-line request into three messages.

### Send it — when all of these hold

- @jeffrey asked for **this** email in this conversation, naming the
  recipient and the purpose ("email Cody about the workshop").
- It is ordinary correspondence: an invitation, an announcement, a
  logistics note, a reply on a thread already underway, a one-liner.
- No attachments.
- Nothing that commits money, accepts or negotiates terms, carries an
  apology or bad news, or speaks for anyone other than @jeffrey.
- One recipient group — not a list, not a blast.

Then send, and print the full headers and body in the reply. A send you
cannot show afterward is a send you should not have made.

### Draft and wait for explicit authorization ("send it" / "go ahead")

- **You thought of the email.** @jeffrey did not ask for it.
- First contact with a person or organization he has no prior thread
  with — the opening line of a relationship is his to write.
- Anything with an attachment.
- Money, contracts, legal terms, apologies, bad news, or a negotiation.
- Any list or mass send.

When unsure which lane applies, draft. Drafting costs one message; a
wrong send costs a relationship.

### Always

Report what was sent — recipients, subject, body — whichever lane it
took. Never invent a commitment on @jeffrey's behalf: a date, a price,
a deliverable, or a yes he has not actually given.
