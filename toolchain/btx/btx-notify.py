#!/usr/bin/env python3
"""Send an email from jasellite via Gmail SMTP. Usage: btx-notify.py "subject" "body"
Creds read from ~/.config/btx-notify.env (SMTP_SERVER, SMTP_USER, SMTP_PASS, NOTIFY_TO)."""
import sys, os, ssl, smtplib
from email.message import EmailMessage

env = {}
for line in open(os.path.expanduser("~/.config/btx-notify.env")):
    line = line.strip()
    if not line or line.startswith("#") or "=" not in line:
        continue
    k, v = line.split("=", 1)
    env[k] = v

subject = sys.argv[1] if len(sys.argv) > 1 else "BTX notice"
body = sys.argv[2] if len(sys.argv) > 2 else ""

msg = EmailMessage()
msg["From"] = env["SMTP_USER"]
msg["To"] = env.get("NOTIFY_TO", env["SMTP_USER"])
msg["Subject"] = subject
msg.set_content(body)

with smtplib.SMTP(env.get("SMTP_SERVER", "smtp.gmail.com"), 587, timeout=30) as s:
    s.starttls(context=ssl.create_default_context())
    s.login(env["SMTP_USER"], env["SMTP_PASS"])
    s.send_message(msg)
print("sent:", subject)
