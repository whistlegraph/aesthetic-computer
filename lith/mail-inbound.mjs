// mail-inbound — the door where the outside world's letters come in.
//
// Google Workspace holds the MX for aesthetic.computer. A routing rule there
// catches every address that isn't a real mailbox and hands the message to
// this host over SMTP (Apps → Gmail → Default routing → route to host
// inbound.aesthetic.computer:25). We take letters only from Google's own
// relays, turn the envelope recipient into a handle, and file the letter as
// an Amail through the same backend the prompt and the piece use.
//
//   jeffrey@aesthetic.computer         → the handle
//   ac25namuc@aesthetic.computer       → the permahandle
//   amail+ac25namuc@aesthetic.computer → a reply to a letter we sent (phase 2)
//
// Runs as its own unit (lith-mail.service) so lith itself never listens on
// 25. STARTTLS is offered when Caddy has minted the host's certificate.
//
//   node mail-inbound.mjs                  # :25, Google relays only
//   node mail-inbound.mjs --port 2525 --open   # any sender — local testing

import { SMTPServer } from "smtp-server";
import { simpleParser } from "mailparser";
import { BlockList } from "node:net";
import { resolveTxt } from "node:dns/promises";
import { existsSync, readFileSync } from "node:fs";

const HOST = process.env.AMAIL_INBOUND_HOST || "inbound.aesthetic.computer";
const args = process.argv.slice(2);
const flag = (name) => args.includes(name);
const opt = (name, fallback) => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : fallback;
};
const PORT = Number(opt("--port", process.env.AMAIL_INBOUND_PORT || 25));
const OPEN = flag("--open") || process.env.AMAIL_INBOUND_OPEN === "1";
const MAX_SIZE = 1_000_000; // a letter, not an attachment service

// 🏷️ Which mailbox an envelope recipient means. Pure, so it can be tested
// without a server: returns { local, tag, domain } or null when the address
// isn't ours.
export function parseRecipient(address, domains) {
  const at = (address || "").lastIndexOf("@");
  if (at < 1) return null;
  const domain = address.slice(at + 1).toLowerCase();
  if (!domains.includes(domain)) return null;
  let local = address.slice(0, at).toLowerCase();
  let tag = null;
  const plus = local.indexOf("+");
  if (plus >= 0) {
    tag = local.slice(plus + 1) || null;
    local = local.slice(0, plus);
  }
  // `amail+<handle>@` is the post office's own return address — the tag says
  // whose letter it answers. Bare `amail@` and `mail@` are Google's boxes and
  // never reach us; if they do, nobody here can take them.
  if (local === "amail") return tag ? { local: tag, tag, domain, reply: true } : null;
  if (local === "mail" || local === "postmaster" || local === "abuse") return null;
  return { local, tag, domain };
}

// 📮 Google publishes the relays it sends from as SPF netblocks. Walk the
// includes once, keep the ranges in a BlockList, refresh every hour.
export async function googleRelays(root = "_spf.google.com") {
  const list = new BlockList();
  const seen = new Set();
  async function walk(name) {
    if (seen.has(name)) return;
    seen.add(name);
    const txt = (await resolveTxt(name)).map((parts) => parts.join("")).join(" ");
    for (const token of txt.split(/\s+/)) {
      if (token.startsWith("include:")) await walk(token.slice(8));
      else if (token.startsWith("ip4:") || token.startsWith("ip6:")) {
        const family = token.startsWith("ip4:") ? "ipv4" : "ipv6";
        const [ip, prefix] = token.slice(4).split("/");
        list.addSubnet(ip, Number(prefix ?? (family === "ipv4" ? 32 : 128)), family);
      }
    }
  }
  await walk(root);
  return list;
}

function isRelay(list, remote) {
  if (!list) return false;
  const ip = remote.startsWith("::ffff:") ? remote.slice(7) : remote;
  return list.check(ip, ip.includes(":") ? "ipv6" : "ipv4");
}

// The body of the letter as plain text — the text part if there is one,
// otherwise the HTML with its tags knocked off.
export function letterText(parsed) {
  if (parsed.text?.trim()) return parsed.text.trim();
  const html = parsed.html || parsed.textAsHtml || "";
  return html
    .replace(/<style[\s\S]*?<\/style>/gi, "")
    .replace(/<br\s*\/?>/gi, "\n")
    .replace(/<\/(p|div|li|h\d)>/gi, "\n")
    .replace(/<[^>]+>/g, "")
    .replace(/&nbsp;/g, " ")
    .replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/\n{3,}/g, "\n\n")
    .trim();
}

// Caddy holds a Let's Encrypt cert for the host (the Caddyfile serves it for
// exactly this reason). Offer STARTTLS with it when it exists.
function tlsFor(host) {
  const dir = `/var/lib/caddy/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/${host}`;
  const key = `${dir}/${host}.key`;
  const cert = `${dir}/${host}.crt`;
  if (existsSync(key) && existsSync(cert)) {
    return { key: readFileSync(key), cert: readFileSync(cert) };
  }
  return null;
}

// 🚪 Build the server. `lookup(local) → sub | undefined` and
// `file({ sub, rcpt, parsed }) → result` are handed in so a test can run the
// whole SMTP conversation against a fake mailbox.
export function createInbound({ lookup, file, domains, relays = null, open = false, tls = null, log = console.log }) {
  const server = new SMTPServer({
    name: HOST,
    banner: "Amail — aesthetic.computer",
    size: MAX_SIZE,
    disabledCommands: ["AUTH"],
    authOptional: true,
    hideSTARTTLS: !tls,
    ...(tls || {}),

    onConnect(session, cb) {
      if (open || isRelay(relays, session.remoteAddress)) return cb();
      log(`✋ refused ${session.remoteAddress} — not a Google relay`);
      cb(new Error("Only Google Workspace delivers here"));
    },

    async onRcptTo(address, session, cb) {
      const who = parseRecipient(address.address, domains);
      if (!who) return cb(Object.assign(new Error("No such mailbox"), { responseCode: 550 }));
      try {
        const sub = await lookup(who.local);
        if (!sub) return cb(Object.assign(new Error(`No handle ${who.local}`), { responseCode: 550 }));
        session.amail = session.amail || new Map();
        session.amail.set(address.address, { sub, ...who });
        cb();
      } catch (err) {
        log("🔴 lookup failed:", err?.message || err);
        cb(Object.assign(new Error("Try again later"), { responseCode: 451 }));
      }
    },

    async onData(stream, session, cb) {
      try {
        const parsed = await simpleParser(stream, { skipImageLinks: true });
        if (stream.sizeExceeded) {
          return cb(Object.assign(new Error("Letter too large"), { responseCode: 552 }));
        }
        const results = [];
        for (const rcpt of session.amail?.values() || []) {
          results.push(await file({ ...rcpt, parsed, remote: session.remoteAddress }));
        }
        const summary = results
          .map((r) => (r.duplicate ? `${r.toHandle} (again)` : r.toHandle))
          .join(", ");
        log(`📬 ${parsed.from?.value?.[0]?.address || "?"} → ${summary || "nobody"}`);
        cb();
      } catch (err) {
        log("🔴 letter failed:", err?.message || err);
        cb(Object.assign(new Error("Could not file that letter"), { responseCode: 451 }));
      }
    },
  });
  return server;
}

// 🏁 Wire the door to the real mailbox and open it.
async function main() {
  const { connect } = await import("../system/backend/database.mjs");
  const {
    subFromAddress,
    deliverFromOutside,
    clean,
    INBOUND_DOMAINS,
    OUTSIDE_TEXT_LENGTH,
    MAX_SUBJECT_LENGTH,
  } = await import("../system/backend/mail.mjs");

  const database = await connect();

  let relays = null;
  if (!OPEN) {
    relays = await googleRelays();
    setInterval(async () => {
      try {
        relays = await googleRelays();
      } catch (err) {
        console.log("🟡 could not refresh Google relays:", err?.message || err);
      }
    }, 60 * 60 * 1000).unref();
  }

  const tls = tlsFor(HOST);
  const server = createInbound({
    domains: INBOUND_DOMAINS,
    relays,
    open: OPEN,
    tls,
    lookup: (local) => subFromAddress(local, database),
    file: async ({ sub, parsed, reply }) => {
      const sender = parsed.from?.value?.[0] || {};
      return deliverFromOutside(
        {
          to: sub,
          fromEmail: (sender.address || "").toLowerCase(),
          fromName: sender.name || "",
          subject: clean(reply ? `re: ${parsed.subject || ""}` : parsed.subject, MAX_SUBJECT_LENGTH),
          text: clean(letterText(parsed), OUTSIDE_TEXT_LENGTH) || "(an empty letter)",
          messageId: parsed.messageId || null,
        },
        database,
      );
    },
  });

  server.on("error", (err) => console.log("🔴 smtp:", err?.message || err));
  server.listen(PORT, () => {
    console.log(
      `📮 Amail inbound on :${PORT} for ${INBOUND_DOMAINS.join(", ")} — ` +
        `${OPEN ? "OPEN to any sender" : "Google relays only"}, ${tls ? "STARTTLS" : "no TLS (no cert yet)"}`,
    );
  });

  // Caddy mints the certificate on its own clock, usually within a minute of
  // the host first appearing in the Caddyfile. Google's route requires TLS,
  // so a door without a cert is shut in practice: watch for the files and
  // step out the moment they land — systemd (Restart=always) brings the door
  // back up dressed for STARTTLS.
  if (!tls) {
    setInterval(() => {
      if (tlsFor(HOST)) {
        console.log("🔐 certificate arrived — restarting to offer STARTTLS");
        server.close(() => process.exit(0));
        setTimeout(() => process.exit(0), 3000).unref();
      }
    }, 30 * 1000).unref();
  }

  for (const signal of ["SIGINT", "SIGTERM"]) {
    process.on(signal, () => server.close(() => process.exit(0)));
  }
}

if (process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href) {
  main().catch((err) => {
    console.error("🔴 mail-inbound failed to start:", err);
    process.exit(1);
  });
}
