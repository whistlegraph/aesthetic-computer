#!/usr/bin/env python3
"""mail-mcp helper — everything that has to happen on the machine that holds
the mail (jasellite): the draft store, the preview render, the send, and the
Gmail IMAP verbs mu and mbsync have no word for.

  helper.py draft-store            JSON bundle on stdin -> files, preview PNG, Gmail draft
  helper.py draft-send <id>        send the stored bytes, exactly once
  helper.py draft-list             pending drafts, one JSON object per line
  helper.py draft-show <id>        one draft's record and plain text
  helper.py draft-discard <id>
  helper.py archive <account> <message-id>...
  helper.py html-text <path>       text of a message that has only an HTML body

Credentials come from ~/.mbsyncrc; nothing here prints them.
"""
import base64, email, glob, hashlib, html, imaplib, json, os, re, shutil, subprocess, sys, time

DRAFTS = os.environ.get("AC_MAIL_DRAFTS") or os.path.expanduser("~/.mail-drafts")
GMAIL_DRAFTS = '"[Gmail]/Drafts"'


def imap(account, mailbox):
    cfg = open(os.path.expanduser("~/.mbsyncrc")).read()
    blocks = re.split(r"\n(?=IMAPAccount )", cfg)
    blk = next(b for b in blocks if b.startswith(f"IMAPAccount {account}\n"))
    user = re.search(r"^User (.+)$", blk, re.M).group(1).strip()
    pw = re.search(r"^Pass \"?([^\"\n]+)\"?$", blk, re.M).group(1).strip()
    M = imaplib.IMAP4_SSL("imap.gmail.com")
    M.login(user, pw)
    typ, _ = M.select(mailbox)
    if typ != "OK":
        raise RuntimeError(f"cannot open {mailbox} on {account}")
    return M


def remove_by_id(account, mailbox, mids):
    """Flag + expunge. In INBOX that drops the label (the message stays in All
    Mail); in Drafts it deletes the draft."""
    M = imap(account, mailbox)
    out = []
    for mid in mids:
        mid = mid.strip("<>")
        _, data = M.search(None, "HEADER", "Message-ID", f"<{mid}>")
        uids = data[0].split()
        for u in uids:
            M.store(u, "+FLAGS", r"(\Seen \Deleted)")
        out.append((mid, len(uids)))
    M.expunge()
    M.logout()
    return out


def path(draft_id, ext):
    if not re.fullmatch(r"d[0-9a-z]+", draft_id):
        raise SystemExit(f"bad draft id: {draft_id}")
    return os.path.join(DRAFTS, f"{draft_id}.{ext}")


def load(draft_id):
    try:
        return json.load(open(path(draft_id, "json")))
    except FileNotFoundError:
        raise SystemExit(f"no such draft: {draft_id}")


def save(meta):
    tmp = path(meta["id"], "json") + ".tmp"
    json.dump(meta, open(tmp, "w"))
    os.replace(tmp, path(meta["id"], "json"))


def drop_gmail_draft(meta):
    if not meta.get("gmailDraft"):
        return
    try:
        remove_by_id(meta["account"], GMAIL_DRAFTS, [meta["messageId"]])
    except Exception:
        pass  # a leftover draft in Gmail is an annoyance, not a failure


def render(page_html, draft_id, height):
    chrome = shutil.which("google-chrome") or shutil.which("chromium") or shutil.which("chromium-browser")
    if not chrome:
        return None, "no chrome on this machine"
    src, png = path(draft_id, "html"), path(draft_id, "png")
    open(src, "w").write(page_html)
    try:
        subprocess.run(
            [chrome, "--headless=new", "--no-sandbox", "--disable-gpu", "--hide-scrollbars",
             "--force-device-scale-factor=2", f"--window-size=700,{int(height)}",
             f"--screenshot={png}", f"file://{src}"],
            capture_output=True, timeout=60, check=True)
        return base64.b64encode(open(png, "rb").read()).decode(), None
    except Exception as e:
        return None, f"render failed: {e}"


def draft_store():
    b = json.load(sys.stdin)
    meta = b["meta"]
    os.makedirs(DRAFTS, mode=0o700, exist_ok=True)
    raw = base64.b64decode(b["eml"])
    if hashlib.sha256(raw).hexdigest() != meta["sha256"]:
        raise SystemExit("message bytes do not match their hash")
    with open(os.open(path(meta["id"], "eml"), os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600), "wb") as f:
        f.write(raw)
    open(path(meta["id"], "txt"), "w").write(b["text"])
    result = {"id": meta["id"]}

    old_id = meta.get("revises")
    if old_id:
        old = load(old_id)
        if old["status"] != "pending":
            raise SystemExit(f"{old_id} is {old['status']}; it cannot be revised")
        old["status"] = "superseded"
        old["supersededBy"] = meta["id"]
        save(old)
        drop_gmail_draft(old)

    png, why = render(b["previewHtml"], meta["id"], b["previewHeight"])
    result["png"] = png
    if why:
        result["previewError"] = why

    try:
        M = imap(meta["account"], GMAIL_DRAFTS)
        M.append(GMAIL_DRAFTS, r"(\Draft)", None, raw)
        M.logout()
        meta["gmailDraft"] = True
    except Exception as e:
        meta["gmailDraft"] = False
        result["gmailError"] = str(e)
    result["gmailDraft"] = meta["gmailDraft"]
    save(meta)
    print(json.dumps(result))


def draft_send(draft_id):
    meta = load(draft_id)
    if meta["status"] != "pending":
        raise SystemExit(f"{draft_id} is {meta['status']}; not sending")
    raw = open(path(draft_id, "eml"), "rb").read()
    if hashlib.sha256(raw).hexdigest() != meta["sha256"]:
        raise SystemExit(f"{draft_id} changed on disk since it was previewed; not sending")
    # Claim the draft before handing it to msmtp, so a retry after a lost
    # response can never send it twice.
    meta["status"] = "sending"
    save(meta)
    p = subprocess.run(["msmtp", "-a", meta["account"], "-t"], input=raw, capture_output=True, timeout=600)
    if p.returncode != 0:
        meta["status"] = "pending"
        save(meta)
        raise SystemExit(f"msmtp exited {p.returncode}: {p.stderr.decode(errors='replace').strip()}")
    meta["status"] = "sent"
    meta["sentAt"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    save(meta)
    drop_gmail_draft(meta)
    print(json.dumps({"meta": meta, "text": open(path(draft_id, "txt")).read()}))


def draft_list():
    for f in sorted(glob.glob(os.path.join(DRAFTS, "*.json"))):
        meta = json.load(open(f))
        if meta["status"] == "pending":
            print(json.dumps(meta))


def draft_show(draft_id):
    print(json.dumps({"meta": load(draft_id), "text": open(path(draft_id, "txt")).read()}))


def draft_discard(draft_id):
    meta = load(draft_id)
    if meta["status"] != "pending":
        raise SystemExit(f"{draft_id} is {meta['status']}; nothing to discard")
    meta["status"] = "discarded"
    save(meta)
    drop_gmail_draft(meta)
    print(json.dumps({"meta": meta}))


def archive(account, mids):
    for mid, n in remove_by_id(account, "INBOX", mids):
        print(f"{'archived' if n else 'not in inbox'} {account} {mid}")


def html_text(p):
    m = email.message_from_binary_file(open(p, "rb"))
    for part in m.walk():
        if part.get_content_type() != "text/html":
            continue
        t = part.get_payload(decode=True).decode(part.get_content_charset() or "utf-8", "replace")
        t = re.sub(r"(?is)<(style|script|head).*?</\1>", "", t)
        t = re.sub(r'(?is)<a\s[^>]*href="([^"]+)"[^>]*>(.*?)</a>',
                   lambda k: f"{k.group(2)} [{k.group(1) if len(k.group(1)) < 120 else 'link'}]", t)
        t = re.sub(r"(?i)<(br|/p|/div|/li|/tr|/h\d)[^>]*>", "\n", t)
        t = html.unescape(re.sub(r"<[^>]+>", "", t))
        t = re.sub(r"[ \t‌ ]+", " ", t)
        print(re.sub(r"\n\s*\n+", "\n\n", t).strip())
        return
    print("[no html body either]")


if __name__ == "__main__":
    cmd, args = (sys.argv[1] if len(sys.argv) > 1 else ""), sys.argv[2:]
    if cmd == "draft-store": draft_store()
    elif cmd == "draft-send": draft_send(args[0])
    elif cmd == "draft-list": draft_list()
    elif cmd == "draft-show": draft_show(args[0])
    elif cmd == "draft-discard": draft_discard(args[0])
    elif cmd == "archive": archive(args[0], args[1:])
    elif cmd == "html-text": html_text(args[0])
    else: raise SystemExit(__doc__)
