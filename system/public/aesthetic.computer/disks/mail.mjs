// Mail, 2026.2.12 → 2026.10.14 (`amail` for a day; that path still aliases here)
// The post of aesthetic.computer. `mail @handle words...` sends from the
// prompt; `mail` lands here; `mail~@handle` lands in compose, addressed.
// One list of letters in and out, newest first, between two bars: the top
// one holds the box's state, the bottom one is where writing happens. Tap a
// letter to select it and the bottom bar answers it; replies carry the
// thread's subject on their own. Tema and language follow laklok's saved
// choice (`mail~skov`, `mail~da` pin one). Public media codes work like chat;
// outside attachments stay in the mailbox.

import {
  LAK_THEMES,
  realtimeTick,
  pickTema,
  restoreTema,
  strings,
  pickLang,
  restoreLang,
  paintEnvelope,
  ENVELOPE,
  paintBadge,
  badgeWidth,
  paintChip,
  chipWidth,
  CHIP_H,
  CHIP_FONT,
} from "./common/laklok-tema.mjs";
import { MailMedia } from "./common/mail-media.mjs";

const mediaView = new MailMedia();
const WRITE = [110, 230, 165];
const PERMA = /^ac\d\d[a-z]{5}$/;

let status = "loading"; // loading · loaded · error · noauth
let mail = null;
let letters = []; // inbox and sent, merged, newest first
let composing = false;
let fields;
let note = null; // what went wrong with the last send
let thread = null; // { who, subject } while answering a letter
let sending = false;
let busy = false; // mark-all-read in flight
let pendingTo = null;
let expandedId = null;
let notificationLetter = null;
let scrollToLetter = null;
const reading = new Set();
let hits = [];
let rows = [];
let mediaHits = [];
let mediaNote = null;
let scroll = 0;
let listTop = 0;
let listBottom = 0; // the bottom bar's top edge — taps below it are the bar's
let dragged = false;
let tema = "ler";
let lang = "en";
let ellipsisTicker;

// 🧪 Test hook — a BroadcastChannel the browser e2e listens on
// (tests/browser/mail-journey.test.mjs, mail-opening.test.mjs): a snapshot
// after every change, and a few messages that do what the controls do. Only
// when booted with `?test=1`.
let testChannel = null;
let testApi = null;
function installTestHook(api) {
  testChannel?.close();
  testChannel = null;
  if (!api.query?.test || typeof BroadcastChannel === "undefined") return;
  testApi = api;
  testChannel = new BroadcastChannel("ac-mail-test");
  testChannel.onmessage = async ({ data }) => {
    const api = testApi;
    if (!data?.type || !api || status !== "loaded") return;
    if (data.type === "compose") {
      write(api, data.to || "");
      thread = data.subject ? { who: data.to, subject: data.subject, quote: "" } : null;
      await send(api, { to: data.to || "", body: data.text || "" });
    } else if (data.type === "reply") {
      const letter = letters.find((l) => l.id === data.id);
      if (letter) {
        expandedId = letter.id;
        write(api, addressOf(letter), letter.subject || "", letter.text || "");
      }
    } else if (data.type === "read") await markAllRead(api);
    else if (data.type === "refresh") await refresh(api);
    else if (data.type === "view" && composing) close(api);
    api.needsPaint();
    tell();
  };
}
function tell() {
  if (!testChannel) return;
  testChannel.postMessage({
    ready: true,
    piece: "mail",
    status,
    view: composing ? "compose" : "inbox",
    unread: mail?.unread ?? 0,
    total: mail?.inbox?.length ?? 0,
    addresses: mail?.addresses ?? [],
    inbox: (mail?.inbox ?? []).map(({ id, from, subject, text, read }) => ({ id, from, subject, text, read })),
    sent: (mail?.sent ?? []).map(({ id, to, subject, text }) => ({ id, to, subject, text })),
    composeTo: composing ? fields.values.to : null,
    composeSubject: composing ? thread?.subject ?? null : null,
    composeNote: note,
  });
}

const T = () => LAK_THEMES[tema];
const S = () => strings(lang);

function meta() {
  return {
    title: "Mail — aesthetic.computer",
    desc: "Your aesthetic.computer mail — letters between handles.",
  };
}

function makeFields(api) {
  const s = S();
  const c = T().chat;
  fields = new api.ui.TextFields(
    api,
    [
      { name: "to", label: s.to, placeholder: "@handle or email" },
      { name: "body", label: s.say, lines: 5, placeholder: "…" },
    ],
    (letter) => send(api, letter),
    {
      scheme: {
        text: c.messageText,
        background: [...T().stripeA, 220],
        block: c.handle,
        highlight: 0,
        guideline: [...c.lines.slice(0, 3), 128],
      },
    },
  );
}

// 🥾 Boot
async function boot(api) {
  const { user, gizmo, hud, store, colon, params } = api;
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();
  composing = false;
  note = null;
  thread = null;
  expandedId = null;
  notificationLetter = typeof api.query?.letter === "string" && /^[a-f0-9]{24}$/i.test(api.query.letter)
    ? api.query.letter.toLowerCase() : null;
  scrollToLetter = null;
  scroll = 0;
  mediaView.clear();
  installTestHook(api);

  const tokens = [...(colon || []), ...(params || [])];
  const picked = pickTema(tokens, store);
  tema = picked.name;
  restoreTema(store, picked.pinned, (saved) => {
    if (saved) {
      tema = saved;
      makeFields(api);
    }
  });
  const spoken = pickLang(tokens, store, "en");
  lang = spoken.name;
  restoreLang(store, spoken.pinned, (saved) => {
    if (saved && !composing) {
      lang = saved;
      makeFields(api);
    }
  });
  makeFields(api);

  pendingTo = tokens.find((t) => /^@\w/.test(t) || PERMA.test(t)) || null;
  if (!user) {
    status = "noauth";
    return tell();
  }
  await refresh(api);
  if (notificationLetter && status === "loaded") {
    const letter = letters.find((item) => item.id === notificationLetter && !item.mine);
    if (letter) {
      expandedId = letter.id;
      scrollToLetter = letter.id;
      await markRead(api, letter);
    }
    notificationLetter = null;
  }
  if (pendingTo && status === "loaded") write(api, pendingTo);
  tell();
}

async function refresh({ net }) {
  try {
    const res = await net.userRequest("GET", "/api/mail");
    if (res.status !== 200) throw new Error(`${res.status}`);
    mail = res;
    letters = [
      ...res.inbox.map((l) => ({ ...l, mine: false, who: l.from || l.fromEmail, email: l.fromEmail })),
      ...res.sent.map((l) => ({ ...l, mine: true, read: true, who: l.to || l.toEmail, email: l.toEmail })),
    ].sort((a, b) => new Date(b.when) - new Date(a.when));
    status = "loaded";
  } catch {
    status = "error";
  }
  tell();
}

async function send(api, { to, body }) {
  const s = S();
  const text = (body || "").trim();
  if (!to?.trim()) return void (note = s.whoTo);
  if (!text) return void (note = s.nothingToSay);
  // The fields stay filled until the server answers; one letter at a time.
  if (sending) return;
  sending = true;
  let res;
  try {
    res = await api.net.userRequest("POST", "/api/mail", {
      to: to.trim(),
      subject: thread?.subject || "",
      text,
    });
  } catch {
    note = s.couldntSend; // Request errors may embed content; keep them out of logs.
    return;
  } finally {
    sending = false;
  }
  if (res.status === 200) {
    fields.reset(api);
    close(api);
    expandedId = null;
    scroll = 0;
    await refresh(api);
  } else if (res.status === 404) {
    note = `${s.noOne} ${to.trim()}. ${s.fullHandle}`;
  } else {
    note = s.couldntSend;
  }
  api.needsPaint();
  tell();
}

// The other side of a letter — who a reply to it goes to.
function addressOf(letter) {
  if (letter.mine) return letter.to || letter.toEmail;
  return letter.from?.startsWith("@") || PERMA.test(letter.from || "") ? letter.from : letter.fromEmail;
}

// Open compose; a reply lands on the body with its address filled.
function write(api, to, subject, quote = "") {
  composing = true;
  note = null;
  // Only a reply is a thread; an address from the URL is just an address.
  thread = to && subject !== undefined
    ? { who: to, subject: subject ? (/^re:/i.test(subject) ? subject : `Re: ${subject}`) : "", quote }
    : null;
  fields.input.mute = false;
  fields.focus(to ? 1 : 0, api);
  if (to) fields.values.to = to;
  tell();
}

function close(api) {
  composing = false;
  note = null;
  thread = null;
  fields.input.mute = true;
  api.send({ type: "keyboard:close" });
  tell();
}

async function markRead(api, letter) {
  if (letter.read || reading.has(letter.id)) return;
  reading.add(letter.id);
  try {
    const res = await api.net.userRequest("POST", "/api/mail", { action: "read", id: letter.id });
    if (res.status === 200 && !letter.read) {
      letter.read = true;
      mail.unread = Math.max(0, mail.unread - 1);
    }
  } catch {} // Stays open; unread can be retried.
  finally {
    reading.delete(letter.id);
    api.needsPaint();
    tell();
  }
}

async function markAllRead(api) {
  if (busy) return;
  busy = true;
  try {
    const res = await api.net.userRequest("POST", "/api/mail", { action: "read" });
    if (res.status === 200) {
      mail.unread = 0;
      letters.forEach((l) => (l.read = true));
    }
  } catch {}
  busy = false;
  api.needsPaint();
}

// 🧮 Sim
function sim(api) {
  ellipsisTicker?.update(api.clock.time());
  if (composing) fields.sim(api);
  if (status === "noauth" && api.user) {
    status = "loading";
    refresh(api).then(() => {
      if (status === "loaded" && pendingTo) write(api, pendingTo);
    });
  }
  if (tema === "realtime") realtimeTick();
}

function ago(when) {
  const secs = (Date.now() - new Date(when).getTime()) / 1000;
  if (secs < 60) return "now";
  if (secs < 3600) return `${Math.floor(secs / 60)}m`;
  if (secs < 86400) return `${Math.floor(secs / 3600)}h`;
  if (secs < 604800) return `${Math.floor(secs / 86400)}d`;
  return new Date(when).toLocaleDateString("en-US", { month: "short", day: "numeric" });
}

function chip(api, x, y, text, action, opts) {
  const box = paintChip(api, x, y, text, opts);
  hits.push({ ...box, action });
  return box;
}

// 🎨 Paint
function paint(api) {
  const { wipe, ink, screen, text, help, mask, unmask } = api;
  const t = T();
  const c = t.chat;
  const s = S();
  hits = [];
  rows = [];
  mediaHits = [];
  wipe(...t.bg);
  const x = 6;
  const wide = screen.width - x * 2;

  if (status === "loading") {
    ink(c.timestamp).write(s.loading + ellipsisTicker.text(help.repeat, { pad: false }), { center: "xy" });
    return;
  }
  if (status === "error") {
    ink(200, 100, 100).write(s.error, { center: "xy" });
    return;
  }
  if (status === "noauth") {
    let y = 30;
    ink(c.handle).write(s.adTitle, { x, y });
    y += 14;
    const h = text.box(s.adBody, { x, y }, wide).box.height;
    ink(c.messageText).write(s.adBody, { x, y }, undefined, wide, true);
    y += h + 8;
    const up = chip(api, x, y, s.signup, { type: "signup" }, { selected: true });
    chip(api, up.x + up.w + 6, y, s.login, { type: "login" });
    return;
  }

  // 🎚️ Two bars. The top one sits behind the hud label and carries the box's
  // state — unread envelope, and "mark read" while anything is. The bottom one
  // is where writing happens, like chat: tap it to write; while writing it
  // holds send and cancel.
  const BAR = 22;
  const BOT = 22;
  const botY = screen.height - BOT;
  listBottom = botY;
  ink(t.stripeB).box(0, 0, screen.width, BAR);
  ink(c.lines).box(0, BAR, screen.width, 1);
  ink(t.stripeB).box(0, botY, screen.width, BOT);
  ink(c.lines).box(0, botY - 1, screen.width, 1);

  let tr = screen.width - x;
  if (mail.unread > 0) {
    const n = `${mail.unread}`;
    tr -= chipWidth(s.markRead);
    chip(api, tr, Math.floor((BAR - CHIP_H) / 2), s.markRead, { type: "read" }, { tint: c.log, dim: busy });
    tr -= 8 + ENVELOPE.w + badgeWidth(n) - 3;
    const ey = Math.floor((BAR - ENVELOPE.h) / 2);
    paintEnvelope(api, tr, ey, { lit: true });
    paintBadge(api, tr + ENVELOPE.w - 3, ey - 4, n);
  }

  const by = botY + Math.floor((BOT - CHIP_H) / 2);
  if (composing) {
    const sendBox = chip(api, x, by, s.send, { type: "send" }, { selected: true, dim: sending });
    const cancel = chip(api, screen.width - x - chipWidth("×"), by, "×", { type: "write" });
    if (note) {
      // `write` with a bound and no wrap still runs on, so cut it to the room.
      const room = Math.floor((cancel.x - sendBox.x - sendBox.w - 12) / 4);
      const said = note.length > room ? note.slice(0, Math.max(0, room - 1)) + "…" : note;
      ink(255, 130, 130).write(said, { x: sendBox.x + sendBox.w + 6, y: by + 2 }, undefined, undefined, false, CHIP_FONT);
    }
  } else {
    // One line, one target: with a letter selected it answers that letter;
    // with none it starts a new one.
    const picked = letters.find((l) => l.id === expandedId);
    const to = picked && addressOf(picked);
    hits.push({ x: 0, y: botY, w: screen.width, h: BOT, action: { type: to ? "reply" : "write" } });
    if (to) ink(...WRITE).write(`${s.reply} ${to}…`, { x: x + 2, y: by + 2 }, undefined, wide, false, CHIP_FONT);
    else ink([...c.timestamp, 200]).write(`${s.write}…`, { x: x + 2, y: by + 2 }, undefined, undefined, false, CHIP_FONT);
  }
  let y = BAR + 6;

  if (composing) {
    if (thread) {
      ink(c.handle).write(`${thread.who}:`, { x, y }, undefined, undefined, false, CHIP_FONT);
      if (thread.quote) {
        const room = Math.floor((wide - (thread.who.length + 2) * 4) / 4);
        const q = thread.quote.replace(/\s+/g, " ");
        ink([...c.timestamp, 200]).write(q.length > room ? q.slice(0, room - 1) + "…" : q,
          { x: x + (thread.who.length + 2) * 4, y }, undefined, undefined, false, CHIP_FONT);
      }
      y += 10;
    }
    fields.paint(api, { x, y, width: wide, height: Math.max(40, botY - 6 - y) });
    return;
  }

  if (mediaNote) {
    ink(c.timestamp).write(mediaNote, { x, y }, undefined, wide, true, CHIP_FONT);
    y += CHIP_H;
  }

  if (letters.length === 0) {
    ink(c.timestamp).write(s.noMail, { x, y });
    ink([...c.timestamp, 160]).write(s.tryHint, { x, y: y + 12 });
    return;
  }

  // 📜 One list. Your own letters sit indented under a → so a conversation
  // reads top to bottom without switching tabs.
  const compact = screen.width < 320 || screen.height < 220;
  const face = compact ? CHIP_FONT : undefined;
  const cw = compact ? 4 : 6;
  const lh = compact ? 9 : 11;
  const bounds = wide - 12;
  listTop = y;
  const listH = botY - 1 - listTop;

  const measured = letters.map((letter) => {
    const open = letter.id === expandedId;
    let body = letter.text || "";
    if (!open) {
      const most = Math.floor(bounds / cw) * 2 - 1;
      if (body.length > most) body = body.slice(0, most) + "…";
    }
    const textH = text.box(body, { x: 0, y: 0 }, bounds, 1, true, face).box.height;
    const media = mediaView.layout(api, letter, bounds, S());
    const h = lh + textH + (compact ? 5 : 8) + media.reduce((n, m) => n + m.height, 0);
    return { letter, body, textH, media, h };
  });
  const contentH = measured.reduce((n, m) => n + m.h, 0);
  const maxScroll = Math.max(0, contentH - listH);
  if (scrollToLetter) {
    scroll = 0;
    for (const row of measured) {
      if (row.letter.id === scrollToLetter) break;
      scroll += row.h;
    }
    scrollToLetter = null;
  }
  scroll = Math.max(0, Math.min(maxScroll, scroll));

  mask({ x: 0, y: listTop, width: screen.width, height: listH });
  let ly = listTop - scroll;
  for (const { letter, body, textH, media, h } of measured) {
    if (ly + h >= listTop && ly < listTop + listH) {
      const unread = !letter.mine && !letter.read;
      const lx = x + (letter.mine ? 12 : 0);
      ink(unread ? t.stripeB : letter.mine ? [...t.stripeA, 90] : t.stripeA).box(lx, ly, wide - (lx - x), h - 2);
      if (letter.id === expandedId) ink(...WRITE).box(lx, ly, wide - (lx - x), h - 2, "outline");
      const row = { y0: ly, y1: ly + h, letter };
      rows.push(row);
      const yy = ly + 3;
      if (unread) ink(c.log).box(lx + 2, yy + 2, 3, 3);
      const who = (letter.mine ? "→ " : "") + (letter.who || "someone");
      ink(unread ? c.handle : c.timestamp).write(who, { x: lx + 8, y: yy }, undefined, undefined, false, face);
      const right = screen.width - x - 2;
      const when = ago(letter.when);
      ink([...c.timestamp, 160]).write(when, { x: right - when.length * cw - 2, y: yy }, undefined, undefined, false, face);
      ink(letter.mine ? [...c.messageText, 170] : c.messageText).write(body, { x: lx + 8, y: yy + lh }, undefined, bounds - (lx - x), true, face);
      let my = yy + lh + textH + 2;
      for (const item of media) {
        mediaView.paint(api, item, lx + 8, my, bounds, c.painting, S());
        mediaHits.push({ x: lx + 8, y: my, w: bounds, h: item.height, item });
        my += item.height;
      }
    }
    ly += h;
  }
  unmask();
  if (maxScroll > 0) {
    const thumbH = Math.max(6, Math.round((listH * listH) / contentH));
    const thumbY = listTop + Math.round((listH - thumbH) * (scroll / maxScroll));
    ink([...c.lines.slice(0, 3), 140]).box(screen.width - 3, thumbY, 2, thumbH);
  }
}

// 🎪 Act
function act(api) {
  const { event: e, net, needsPaint } = api;
  const hit = (b) => b && e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h;

  const control = e.is("touch") ? hits.findLast(hit) : null; // chips paint over the bar
  if (control) {
    const { type } = control.action;
    if (type === "signup") net.signup();
    else if (type === "login") net.login();
    else if (type === "write") composing ? close(api) : write(api);
    else if (type === "read") markAllRead(api);
    else if (type === "reply") {
      const letter = letters.find((l) => l.id === expandedId);
      if (letter) write(api, addressOf(letter), letter.subject || "", letter.text || "");
    }
    else if (type === "send") {
      fields.sync();
      send(api, { ...fields.values });
    }
    needsPaint();
    return;
  }
  if (status !== "loaded") return;

  if (composing) {
    if (e.is("keyboard:down:escape")) close(api);
    else if (!fields.act(api)) return;
    needsPaint();
    return;
  }

  if (e.is("keyboard:down:enter")) return void (write(api), needsPaint());
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace") || e.is("keyboard:down:`")) {
    return void api.jump("prompt");
  }

  if (e.is("touch")) dragged = false;
  if (e.is("draw") && e.y >= listTop && e.y < listBottom) {
    scroll -= e.delta.y;
    dragged = true;
    return void needsPaint();
  }
  if (e.is("scroll")) {
    scroll += e.y;
    return void needsPaint();
  }

  if (e.is("lift") && !dragged && e.y >= listTop && e.y < listBottom) {
    const media = mediaHits.find(hit);
    if (media) {
      mediaNote = media.item.file ? S().downloading : null;
      mediaView.open(api, media.item).then(() => (mediaNote = null))
        .catch(() => (mediaNote = S().downloadFailed))
        .finally(needsPaint);
      return void needsPaint();
    }
    const row = rows.find((r) => e.y >= r.y0 && e.y < r.y1);
    if (!row) return;
    // A tap selects one letter (and opens it); tapping it again lets go.
    expandedId = expandedId === row.letter.id ? null : row.letter.id;
    if (expandedId) markRead(api, row.letter);
    needsPaint();
  }
}

function leave() {
  mediaView.clear();
  testChannel?.close();
  testChannel = null;
  testApi = null;
}

export { meta, boot, sim, paint, act, leave };
