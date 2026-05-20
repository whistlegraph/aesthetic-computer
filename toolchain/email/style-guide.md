# Email Style Guide

default_signature: @jeffrey
force_lowercase: true
append_signature_if_missing: true

## Defaults

- keep subjects and body copy all lowercase by default
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
- generic trailing "thanks!" when there's nothing being thanked for

End on the last functional sentence + signature. Concrete next-step
phrasing ("send the JSON back") is information, not hype — keep that.
The substance carries the energy; performed enthusiasm reads as filler
and undercuts the work.

## Drafts and authorization

Do NOT auto-send applications, pitches, outreach, or first-deliverable
emails. Compose the draft, show the headers + body + attachment paths
in the conversation, and wait for explicit authorization ("send it" /
"go ahead") before calling `mail_send`. Replies in a long-running
already-active thread can be sent without the explicit step IF the
prompt makes the intent unambiguous, but when in doubt — draft first.
