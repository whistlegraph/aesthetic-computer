# Email Style Guide

default_signature: @jeffrey
force_lowercase: true
append_signature_if_missing: true

## Defaults

- keep subjects and body copy all lowercase by default
- URLs are never lowercased — Drive file ids and similar are
  case-sensitive, and a lowercased link is a dead link
- sign every email `@jeffrey`, in every register — the mail tool renders it
  in the handle's per-character colours and links it to the AC profile; the
  plain-text part carries the bare handle
- never type the sign-off into the body; the tool appends it

## Overrides

- set `preserve_case: true` on the draft when casing must be preserved
- set `signature` on the draft to override the sign-off for a specific email;
  an override that is not an `@handle` goes out as plain text

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
- The sign-off is still `@jeffrey` (revised by @jeffrey 2026-11-13; the
  earlier rule signed these threads "Jeffrey").

Set `preserve_case: true` on the draft for these.

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

Every email is a draft first. Revised by @jeffrey 2026-11-13: the earlier
"send it if he asked for it by name" lane let an email leave while he was
still editing it, and he never saw the wrapped form before it went.

1. **Draft.** `mail_draft` for a new thread, `mail_reply` to answer a
   message (it fills in threading, reply-all recipients and the answering
   account). Nothing is sent. The draft also appears in Gmail Drafts.
2. **Show it.** Give @jeffrey the headers and body, and open the preview
   image the tool returns — that image is the email as the recipient will
   see it.
3. **Change it.** Any edit is a new draft made with `revises`, with a new
   id and a new preview. The old id can no longer be sent.
4. **Send.** `mail_send` takes the draft id and nothing else, and only
   after @jeffrey has said to send that draft. A yes for one draft does not
   carry to its revision.

Clear out drafts that will not be sent with `mail_discard`; `mail_drafts`
lists what is waiting.

### Always

Report what was sent — recipients, subject, body. Never invent a commitment
on @jeffrey's behalf: a date, a price, a deliverable, or a yes he has not
actually given. If the draft needs one, leave it marked and ask.
