#!/usr/bin/env python3
"""otp-mail — read Iris's Fuser login code out of her mailbox, over IMAP.

    python3 otp-mail.py mark                     -> {"INBOX": 42, "[Gmail]/Spam": 3}
    python3 otp-mail.py wait --after '<json>' --wait 120

Why python and not node: panda has no mail client and no npm tree — no hermes,
no mbsync, no himalaya, nothing to `npm i`. It DOES have /usr/bin/python3, whose
stdlib ships `imaplib`. So the whole mailbox side of the login is 120 lines of
standard library and zero dependencies, on a machine we never have to provision.

The interesting part is `mark`. A login code is only good for the request that
asked for it, so reading the *newest* Fuser mail is not enough — a code from ten
minutes ago is still the newest one until the new mail lands, and typing it gets
"This login code is incorrect" and burns the attempt. That is a real failure, not
a theoretical one.

We could compare timestamps, but that trusts two clocks (ours and Gmail's) to
agree. IMAP already hands us something better: UIDNEXT — the UID the *next*
message to arrive will get. So we mark UIDNEXT before pressing Continue, and
afterwards accept only a message whose UID is >= that mark. "Arrived after we
asked" becomes an integer comparison, and no clock is involved.

Spam is polled too: Fuser's own dialog tells you to check your spam folder, and
an unattended run cannot.

The password comes in through the environment (IMAP_PASSWORD), never argv —
argv is world-readable in `ps`. Nothing here ever prints it.
"""

import email
import imaplib
import json
import os
import re
import sys
import time
from email.header import decode_header

BOXES = ["INBOX", "[Gmail]/Spam"]
SENDER = os.environ.get("OTP_FROM", "fuser.studio")
CODE_RE = re.compile(r"\b(\d{6})\b")


def connect():
    host = os.environ.get("IMAP_HOST", "imap.gmail.com")
    port = int(os.environ.get("IMAP_PORT", "993"))
    user = os.environ["IMAP_USER"]
    password = os.environ["IMAP_PASSWORD"]
    M = imaplib.IMAP4_SSL(host, port)
    M.login(user, password)
    return M


def uidnext(M, box):
    """The UID the next message into `box` will be given. Our high-water mark."""
    typ, data = M.status(f'"{box}"', "(UIDNEXT)")
    if typ != "OK":
        return None
    m = re.search(rb"UIDNEXT (\d+)", data[0])
    return int(m.group(1)) if m else None


def subject_of(msg):
    raw = msg.get("Subject", "")
    out = []
    for part, enc in decode_header(raw):
        if isinstance(part, bytes):
            out.append(part.decode(enc or "utf-8", errors="replace"))
        else:
            out.append(part)
    return "".join(out)


def body_of(msg):
    chunks = []
    for part in msg.walk():
        if part.get_content_type() in ("text/plain", "text/html"):
            try:
                chunks.append(
                    part.get_payload(decode=True).decode(
                        part.get_content_charset() or "utf-8", errors="replace"
                    )
                )
            except Exception:
                pass
    return "\n".join(chunks)


def look(M, box, after):
    """Newest Fuser mail in `box` with UID >= after, and the code in it."""
    typ, _ = M.select(f'"{box}"', readonly=True)
    if typ != "OK":
        return None
    # `UID after:*` is an IMAP idiom that always returns at least the highest
    # UID in the box — even when that UID is below `after` — so the filter
    # below is load-bearing, not belt-and-braces.
    typ, data = M.uid("SEARCH", None, f"UID {after}:*", "FROM", f'"{SENDER}"')
    if typ != "OK" or not data or not data[0]:
        return None
    uids = [int(u) for u in data[0].split() if int(u) >= after]
    if not uids:
        return None

    for uid in sorted(uids, reverse=True):  # newest first
        typ, d = M.uid("FETCH", str(uid), "(INTERNALDATE RFC822)")
        if typ != "OK" or not d or not isinstance(d[0], tuple):
            continue
        msg = email.message_from_bytes(d[0][1])
        subject = subject_of(msg)
        # The code is in the subject line ("Your Fuser login code is 123456",
        # services/auth/realEmailSender.ts), which is why we never have to parse
        # the React-rendered HTML body. Kept as a fallback in case it moves.
        hit = CODE_RE.search(subject) or CODE_RE.search(body_of(msg))
        if hit:
            return {
                "code": hit.group(1),
                "uid": uid,
                "box": box,
                "subject": subject,
                "from": msg.get("From", ""),
                "date": msg.get("Date", ""),
            }
    return None


def main():
    cmd = sys.argv[1] if len(sys.argv) > 1 else "wait"
    args = sys.argv[2:]

    def arg(name, default=None):
        return args[args.index(name) + 1] if name in args else default

    M = connect()
    try:
        if cmd == "mark":
            print(json.dumps({box: uidnext(M, box) for box in BOXES}))
            return

        after = json.loads(arg("--after", "{}"))
        deadline = time.time() + float(arg("--wait", "120"))
        every = float(arg("--every", "3"))

        while True:
            for box in BOXES:
                mark = after.get(box)
                if mark is None:
                    continue
                hit = look(M, box, mark)
                if hit:
                    print(json.dumps(hit))
                    return
            if time.time() >= deadline:
                print(json.dumps({"error": "no login code arrived in time"}))
                sys.exit(2)
            time.sleep(every)
    finally:
        try:
            M.logout()
        except Exception:
            pass


if __name__ == "__main__":
    main()
