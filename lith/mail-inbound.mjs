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

import { mailTrace, recordMailEvent, flushMailEvents } from "../system/backend/mail-events.mjs";
import { SMTPServer } from "smtp-server";
import { simpleParser } from "mailparser";
import { BlockList } from "node:net";
import { resolveTxt } from "node:dns/promises";
import { existsSync, readFileSync } from "node:fs";
import { mailErrorCode } from "../shared/mail-privacy.mjs";
import { incomingAttachments, MAX_MAIL_WIRE_BYTES } from "../system/backend/mail-media.mjs";

const HOST = process.env.AMAIL_INBOUND_HOST || "inbound.aesthetic.computer";
const args = process.argv.slice(2);
const flag = (name) => args.includes(name);
const opt = (name, fallback) => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : fallback;
};
const PORT = Number(opt("--port", process.env.AMAIL_INBOUND_PORT || 25));
const OPEN = flag("--open") || process.env.AMAIL_INBOUND_OPEN === "1";
const MAX_SIZE = MAX_MAIL_WIRE_BYTES;

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

// The host's Let's Encrypt cert, for STARTTLS. certbot keeps it under
// /etc/letsencrypt (see the Caddyfile for why Caddy can't mint this one);
// Caddy's own store is checked second in case that ever changes.
function tlsFor(host) {
  const places = [
    // lith-mail-renew.sh copies the pair here for the unprivileged user.
    ["/etc/lith-mail/privkey.pem", "/etc/lith-mail/fullchain.pem"],
    [`/etc/letsencrypt/live/${host}/privkey.pem`, `/etc/letsencrypt/live/${host}/fullchain.pem`],
    [
      `/var/lib/caddy/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/${host}/${host}.key`,
      `/var/lib/caddy/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/${host}/${host}.crt`,
    ],
  ];
  for (const [key, cert] of places) {
    if (existsSync(key) && existsSync(cert)) {
      return { key: readFileSync(key), cert: readFileSync(cert) };
    }
  }
  return null;
}

// 🛂 What Google's gate found out about the sender. Google stamps
// Authentication-Results on every letter it accepts; we read spf/dkim/dmarc
// and call the letter verified when DMARC passed, or SPF and DKIM both did.
// A DMARC failure the sender's own policy let through (p=none) is refused
// here — a letter that claims to be from a domain that disowns it.
export function authFrom(parsed) {
  const raw = parsed.headers?.get?.("authentication-results");
  const lines = Array.isArray(raw) ? raw : raw ? [raw] : [];
  const out = {};
  for (const line of lines) {
    const text = typeof line === "string" ? line : line?.value || "";
    for (const m of text.matchAll(/\b(spf|dkim|dmarc)=(\w+)/gi)) {
      out[m[1].toLowerCase()] ??= m[2].toLowerCase();
    }
  }
  // DMARC passing is the full word; a DKIM signature that checks out is the
  // sender's own domain vouching, which is enough as long as nothing else
  // failed outright (mail from inside the same Workspace arrives dkim=pass,
  // spf=none, with no DMARC line at all).
  out.verified =
    out.dmarc === "pass" ||
    (out.dkim === "pass" && out.spf !== "fail" && out.dmarc !== "fail");
  return out;
}

// 🚦 Rate limits, in memory (the door is one process). A sender→box pair
// gets `perPairPerHour` letters and `pushPerPair` buzzes an hour; the door
// as a whole takes `perMinute`. Over the pair's share is a 550 (Google
// bounces it to the sender); over the door's is a 451 (Google tries later).
export function makeLimiter({ perPairPerHour = 20, pushPerPair = 3, perMinute = 60 } = {}, now = Date.now) {
  const pairs = new Map();
  const minute = [];
  const prune = (arr, span, t) => {
    while (arr.length && t - arr[0] > span) arr.shift();
  };
  function takeMany(keys) {
    const t = now();
    prune(minute, 60_000, t);
    if (minute.length + keys.length > perMinute) return { ok: false, code: 451, reason: "rate_global", why: "The door is busy, try later" };
    const planned = new Map();
    for (const pair of keys) {
      const hits = planned.get(pair) || [...(pairs.get(pair) || [])];
      prune(hits, 3_600_000, t);
      if (hits.length >= perPairPerHour) return { ok: false, code: 550, reason: "rate_pair", why: "Too many letters to this box this hour" };
      hits.push(t);
      planned.set(pair, hits);
    }
    for (const [key, hits] of planned) pairs.set(key, hits);
    for (const key of keys) minute.push(t);
    if (pairs.size > 5000) for (const [k, v] of pairs) if (!v.length || t - v[v.length - 1] > 3_600_000) pairs.delete(k);
    let released = false;
    return {
      ok: true,
      gates: keys.map((key) => ({ quiet: planned.get(key).length > pushPerPair })),
      release() {
        if (released) return;
        released = true;
        for (const key of keys) {
          const hits = pairs.get(key);
          const index = hits?.lastIndexOf(t) ?? -1;
          if (index >= 0) hits.splice(index, 1);
          const globalIndex = minute.lastIndexOf(t);
          if (globalIndex >= 0) minute.splice(globalIndex, 1);
        }
      },
    };
  }
  return {
    takeMany,
    take(pair) { const result = takeMany([pair]); return result.ok ? { ok: true, ...result.gates[0] } : result; },
  };
}

// 🚪 Build the server. `lookup(local) → sub | undefined` and
// `file({ sub, rcpt, parsed, auth, quiet }) → result` are handed in so a
// test can run the whole SMTP conversation against a fake mailbox. `secret`
// is the value Google's routing rule stamps into X-Amail-Route: with it set,
// a letter that reached us some other way — another tenant's route, say —
// is refused even though it came from a Google relay.
export function createInbound({
  lookup,
  file,
  domains,
  relays = null,
  getRelays = () => relays,
  open = false,
  tls = null,
  secret = null,
  limiter = makeLimiter(),
  log = console.log,
  record = (fields) => recordMailEvent(null, fields, log),
}) {
  const event = (session, name, fields = {}) => {
    session.mailTrace ||= mailTrace();
    record({ ...fields, event: name, trace: session.mailTrace, transport: "smtp-in" });
  };
  const sessions = new Map();
  const ignore = () => {};
  const server = new SMTPServer({
    // smtp-server can refuse SIZE/protocol commands before our callbacks.
    // Consume ONLY response status digits; discard its verbose protocol logs.
    logger: {
      trace: ignore, info: ignore, warn: ignore, error: ignore, fatal: ignore,
      debug(meta, label, payload) {
        if (meta?.tnx !== "send" || label !== "S:" || typeof payload !== "string") return;
        const status = Number(payload.match(/^([45]\d\d)\b/)?.[1]);
        if (status) event(sessions.get(meta.cid) || {}, "smtp_response", { status });
      },
    },
    name: HOST,
    banner: "Amail — aesthetic.computer",
    size: MAX_SIZE,
    disabledCommands: ["AUTH"],
    authOptional: true,
    hideSTARTTLS: !tls,
    ...(tls || {}),

    onConnect(session, cb) {
      sessions.set(session.id, session);
      if (open || isRelay(getRelays(), session.remoteAddress)) { event(session, "connected"); return cb(); }
      event(session, "rejected", { reason: "relay", status: 554 });
      cb(Object.assign(new Error("Only Google Workspace delivers here"), { responseCode: 554 }));
    },

    onMailFrom(address, session, cb) {
      // SMTP connections can carry several independent messages, including RSET.
      session.amail = new Map();
      session.mailTrace = mailTrace();
      sessions.set(session.id, session);
      event(session, "started");
      cb();
    },

    onClose(session) {
      // Includes protocol/SIZE rejections made by smtp-server before callbacks.
      const status = Number(String(session.error || "").match(/^([45]\d\d)\b/)?.[1]);
      event(session, "disconnected", { ...(status ? { status } : {}) });
      sessions.delete(session.id);
    },

    async onRcptTo(address, session, cb) {
      const who = parseRecipient(address.address, domains);
      if (!who) { event(session, "rejected", { reason: "recipient", status: 550 }); return cb(Object.assign(new Error("No such mailbox"), { responseCode: 550 })); }
      try {
        const sub = await lookup(who.local);
        if (!sub) { event(session, "rejected", { reason: "recipient", status: 550 }); return cb(Object.assign(new Error("No such mailbox"), { responseCode: 550 })); }
        session.amail = session.amail || new Map();
        session.amail.set(sub, { sub, ...who });
        event(session, "recipient", { recipients: session.amail.size });
        cb();
      } catch (err) {
        event(session, "deferred", { reason: "lookup", status: 451, error: mailErrorCode(err) });
        cb(Object.assign(new Error("Try again later"), { responseCode: 451 }));
      }
    },

    async onData(stream, session, cb) {
      let reservation;
      try {
        // Drain an oversized message without buffering it or passing it to
        // mailparser. SMTP's advertised SIZE alone doesn't bound parser memory.
        const chunks = [];
        let size = 0;
        for await (const chunk of stream) {
          size += chunk.length;
          if (size <= MAX_SIZE) chunks.push(chunk);
          else chunks.length = 0;
        }
        if (size > MAX_SIZE || stream.sizeExceeded) {
          event(session, "rejected", { reason: "wire_size", status: 552, bytes: size });
          return cb(Object.assign(new Error("Letter too large"), { responseCode: 552 }));
        }
        const parsed = await simpleParser(Buffer.concat(chunks), { skipImageLinks: true });
        const sender = (parsed.from?.value?.[0]?.address || "?").toLowerCase();

        // Only letters that came through OUR routing rule carry the stamp.
        if (secret && parsed.headers?.get?.("x-amail-route") !== secret) {
          event(session, "rejected", { reason: "route", status: 550 });
          return cb(Object.assign(new Error("Not our route"), { responseCode: 550 }));
        }

        const auth = authFrom(parsed);
        if (auth.dmarc === "fail") {
          event(session, "rejected", { reason: "dmarc", status: 550 });
          return cb(Object.assign(new Error("Sender's domain disowns this letter"), { responseCode: 550 }));
        }

        // Inline MIME images and regular attachments take the same private
        // path. Validate the entire letter before filing it for any recipient.
        const attachments = incomingAttachments(parsed.attachments);

        const recipients = [...(session.amail?.values() || [])];
        // Check and reserve ALL recipients before storing ANY copy. A DATA 250
        // must never acknowledge a message whose recipients were silently skipped.
        reservation = limiter.takeMany(recipients.map((rcpt) => `${sender}→${rcpt.sub}`));
        if (!reservation.ok) {
          event(session, reservation.code === 451 ? "deferred" : "rejected", { reason: reservation.reason, status: reservation.code, recipients: recipients.length });
          return cb(Object.assign(new Error(reservation.why), { responseCode: reservation.code }));
        }
        const results = [];
        for (const [i, rcpt] of recipients.entries()) {
          results.push(await file({ ...rcpt, parsed, attachments, auth, quiet: reservation.gates[i].quiet, trace: session.mailTrace, remote: session.remoteAddress }));
        }
        event(session, "accepted", {
          status: 250, bytes: size, attachments: attachments.length,
          recipients: results.length,
          duplicates: results.filter((r) => r.duplicate).length,
          verified: auth.verified === true,
        });
        cb();
      } catch (err) {
        // A temporary storage outage must not turn retries into quota bounces.
        reservation?.release?.();
        if (err.responseCode === 552) {
          event(session, "rejected", { reason: "attachments", status: 552 });
          return cb(Object.assign(new Error("At most 10 files and 8 MiB of attachments per letter"), { responseCode: 552 }));
        }
        event(session, "deferred", { reason: "storage", status: 451, error: mailErrorCode(err) });
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
  const record = (fields) => recordMailEvent(database, { transport: "smtp-in", ...fields });

  let relays = null;
  if (!OPEN) {
    relays = await googleRelays();
    setInterval(async () => {
      try {
        relays = await googleRelays();
        record({ event: "relays_refreshed" });
      } catch (err) {
        record({ event: "relays_failed", error: mailErrorCode(err) });
      }
    }, 60 * 60 * 1000).unref();
  }

  const tls = tlsFor(HOST);
  const secret = process.env.AMAIL_ROUTE_SECRET || null;
  if (!secret) console.log("🟡 AMAIL_ROUTE_SECRET is unset — any Google tenant's route would be accepted");
  const server = createInbound({
    domains: INBOUND_DOMAINS,
    getRelays: () => relays,
    record,
    open: OPEN,
    tls,
    secret,
    lookup: (local) => subFromAddress(local, database),
    file: async ({ sub, parsed, attachments, reply, auth, quiet, trace }) => {
      const sender = parsed.from?.value?.[0] || {};
      return deliverFromOutside(
        {
          trace,
          to: sub,
          fromEmail: (sender.address || "").toLowerCase(),
          fromName: sender.name || "",
          subject: clean(reply ? `re: ${parsed.subject || ""}` : parsed.subject, MAX_SUBJECT_LENGTH),
          text: clean(letterText(parsed), OUTSIDE_TEXT_LENGTH) || "(an empty letter)",
          messageId: parsed.messageId || null,
          auth,
          quiet,
          attachments,
        },
        database,
      );
    },
  });

  server.on("error", (err) => record({ event: "smtp_error", error: mailErrorCode(err) }));
  server.listen(PORT, () => {
    record({ event: "ready", tls: !!tls, routeSecret: !!secret, open: OPEN });
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
    process.on(signal, () => server.close(async () => { await flushMailEvents(); process.exit(0); }));
  }
}

if (process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href) {
  main().catch((err) => {
    console.error("mail.inbound.start.error", mailErrorCode(err));
    process.exit(1);
  });
}
