// Mail, 2026.2.12 → 2026.11.24 (`amail` and `dm` alias here)
// The post of aesthetic.computer, and its direct messages: a letter between
// two handles is a DM. `mail @handle words...` sends from the prompt; `mail`
// lands on one row per person, newest first, between two bars: the top one
// holds the box's state, the bottom one is where writing happens. Tap a
// person for the whole conversation, oldest at the top, and the bottom bar
// writes to them. `mail~@handle` (or `dm @handle`) opens that conversation,
// straight into compose when there is none yet. A conversation refreshes
// while it is open; a notification opens the one it came from. Tema and language follow laklok's saved
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
let convo = null; // { address, who, letters, loading } while reading one person
let scrollToEnd = false;
let hud = null;
let pollAt = 0; // when to next ask the server whether anything arrived
let polling = false;
const POLL_MS = 12000;
let sending = false;
let busy = false; // mark-all-read in flight
let pendingTo = null;
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
      if (letter) write(api, addressOf(letter), letter.subject || "", letter.text || "");
    } else if (data.type === "read") await markAllRead(api);
    else if (data.type === "refresh") await refresh(api);
    else if (data.type === "view") {
      if (composing) close(api);
      if (data.value === "inbox") leaveConvo(api);
    }
    else if (data.type === "open" && data.with) await openConvo(api, data.with);
    else if (data.type === "back") leaveConvo(api);
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
    view: composing ? "compose" : convo ? "conversation" : "inbox",
    with: convo?.address ?? null,
    conversation: convo ? convo.letters.map(({ id, mine, text, read }) => ({ id, mine, text, read })) : null,
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
  const { user, gizmo, store, colon, params } = api;
  hud = api.hud;
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();
  composing = false;
  note = null;
  thread = null;
  convo = null;
  scrollToEnd = false;
  pollAt = Date.now() + POLL_MS;
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
    // A notification opens the conversation its letter belongs to.
    const letter = letters.find((item) => item.id === notificationLetter && !item.mine);
    notificationLetter = null;
    if (letter) {
      await openConvo(api, addressOf(letter), letter.who);
      scrollToLetter = letter.id;
    }
  } else if (pendingTo && status === "loaded") await openPending(api);
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
  if (convo && status === "loaded") await loadConvo({ net });
  tell();
}

// 👥 One row per person: their newest letter and how many of theirs are unread.
function people() {
  const byKey = new Map();
  for (const letter of letters) {
    const address = addressOf(letter);
    if (!address) continue;
    const key = address.toLowerCase();
    let person = byKey.get(key);
    if (!person) byKey.set(key, (person = { key, address, who: letter.who || address, latest: letter, unread: 0 }));
    if (!letter.mine && !letter.read) person.unread++;
  }
  return [...byKey.values()];
}

// 💬 One person's letters, both ways, oldest first. What the box already
// holds shows at once; the server's full history replaces it.
async function openConvo(api, address, who) {
  const key = address.toLowerCase();
  convo = {
    address,
    who: who || address,
    letters: letters.filter((l) => addressOf(l)?.toLowerCase() === key).reverse(),
    loading: true,
  };
  scroll = 0;
  scrollToEnd = true;
  hud?.label(`mail ${convo.who}`);
  api.needsPaint?.();
  tell();
  await loadConvo(api);
  if (convo?.address === address) await markConvoRead(api);
}

async function loadConvo({ net, needsPaint }) {
  const open = convo;
  if (!open) return;
  try {
    const res = await net.userRequest("GET", `/api/mail?with=${encodeURIComponent(open.address)}`);
    if (convo !== open) return;
    if (res.status === 200 && Array.isArray(res.letters)) {
      const grew = res.letters.length > open.letters.length;
      open.letters = res.letters.map((l) => ({ ...l, who: l.mine ? l.to || l.toEmail : l.from || l.fromEmail }));
      if (grew) scrollToEnd = true;
    }
  } catch {} // What the box held stays on screen.
  if (convo === open) open.loading = false;
  needsPaint?.();
  tell();
}

async function markConvoRead(api) {
  const open = convo;
  const unread = open?.letters.filter((l) => !l.mine && !l.read) ?? [];
  if (!unread.length) return;
  try {
    const res = await api.net.userRequest("POST", "/api/mail", { action: "read", with: open.address });
    if (res.status !== 200) return;
    const ids = new Set(unread.map((l) => String(l.id)));
    for (const l of [...unread, ...letters]) if (ids.has(String(l.id))) l.read = true;
    if (mail) mail.unread = Math.max(0, mail.unread - (Number(res.read) || unread.length));
  } catch {} // Stays unread; the next open retries.
  api.needsPaint?.();
  tell();
}

function leaveConvo(api) {
  convo = null;
  scroll = 0;
  hud?.label("mail");
  api?.needsPaint?.();
  tell();
}

// `mail~@handle`: that conversation, or compose when there isn't one yet.
async function openPending(api) {
  const to = pendingTo;
  pendingTo = null;
  await openConvo(api, to);
  if (convo && convo.letters.length === 0) write(api, to, "");
}

// The subject a reply in this conversation carries: the newest one used.
function convoSubject() {
  for (let i = (convo?.letters.length ?? 0) - 1; i >= 0; i--) {
    if (convo.letters[i].subject) return convo.letters[i].subject;
  }
  return "";
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
    // The letter lands in its conversation, newest at the bottom.
    if (convo?.address.toLowerCase() !== to.trim().toLowerCase()) convo = { address: to.trim(), who: to.trim(), letters: [], loading: true };
    hud?.label(`mail ${convo.who}`);
    scrollToEnd = true;
    await refresh(api);
  } else if (res.status === 404) {
    note = `${s.noOne} ${to.trim()}. ${s.fullHandle}`;
  } else {
    note = s.couldntSend;
  }
  api.needsPaint();
  tell();
}

// Put the fields away — what follows is addressing and reading.

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
  // Anything new? The cheap count asks; only a change fetches the box.
  if (status === "loaded" && !composing && !polling && Date.now() >= pollAt) {
    polling = true;
    pollAt = Date.now() + POLL_MS;
    api.net.userRequest("GET", "/api/mail?count=1")
      .then(async (res) => {
        if (res.status === 200 && mail && (res.unread !== mail.unread || (res.total ?? 0) > (mail.total ?? res.total))) {
          await refresh(api);
          api.needsPaint();
        }
        if (res.status === 200 && mail) mail.total = res.total;
      })
      .catch(() => {})
      .finally(() => (polling = false));
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
  // state — unread envelope and "mark read" on the list, "back" in a
  // conversation. The bottom one is where writing happens, like chat: tap it
  // to write; while writing it holds send and cancel.
  const BAR = 22;
  const BOT = 22;
  const botY = screen.height - BOT;
  listBottom = botY;
  ink(t.stripeB).box(0, 0, screen.width, BAR);
  ink(c.lines).box(0, BAR, screen.width, 1);
  ink(t.stripeB).box(0, botY, screen.width, BOT);
  ink(c.lines).box(0, botY - 1, screen.width, 1);

  const barY = Math.floor((BAR - CHIP_H) / 2);
  let tr = screen.width - x;
  if (convo && !composing) {
    tr -= chipWidth(s.back);
    chip(api, tr, barY, s.back, { type: "back" });
  } else if (!convo && mail.unread > 0) {
    const n = `${mail.unread}`;
    tr -= chipWidth(s.markRead);
    chip(api, tr, barY, s.markRead, { type: "read" }, { tint: c.log, dim: busy });
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
  } else if (convo) {
    hits.push({ x: 0, y: botY, w: screen.width, h: BOT, action: { type: "reply" } });
    ink(...WRITE).write(`${s.writeTo} ${convo.who}…`, { x: x + 2, y: by + 2 }, undefined, wide, false, CHIP_FONT);
  } else {
    hits.push({ x: 0, y: botY, w: screen.width, h: BOT, action: { type: "write" } });
    ink([...c.timestamp, 200]).write(`${s.write}…`, { x: x + 2, y: by + 2 }, undefined, undefined, false, CHIP_FONT);
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

  const compact = screen.width < 320 || screen.height < 220;
  const face = compact ? CHIP_FONT : undefined;
  const cw = compact ? 4 : 6;
  const lh = compact ? 9 : 11;
  listTop = y;
  const listH = botY - 1 - listTop;
  const rowsOf = convo ? measureConvo(api, { wide, face, cw, lh, compact }) : measurePeople(api, { wide, face, cw, lh, compact });

  if (rowsOf.length === 0) {
    if (convo) {
      const words = convo.loading ? s.loading + ellipsisTicker.text(help.repeat, { pad: false }) : s.sayHi;
      ink(c.timestamp).write(words, { x, y });
    } else {
      ink(c.timestamp).write(s.noMail, { x, y });
      ink([...c.timestamp, 160]).write(s.tryHint, { x, y: y + 12 });
    }
    return;
  }

  const contentH = rowsOf.reduce((n, r) => n + r.h, 0);
  const maxScroll = Math.max(0, contentH - listH);
  if (scrollToLetter) {
    scroll = 0;
    for (const row of rowsOf) {
      if (row.letter?.id === scrollToLetter) break;
      scroll += row.h;
    }
    scrollToLetter = null;
    scrollToEnd = false;
  } else if (scrollToEnd) {
    scroll = maxScroll;
    scrollToEnd = false;
  }
  scroll = Math.max(0, Math.min(maxScroll, scroll));

  mask({ x: 0, y: listTop, width: screen.width, height: listH });
  let ly = listTop - scroll;
  for (const row of rowsOf) {
    if (ly + row.h >= listTop && ly < listTop + listH) {
      row.paint(ly);
      rows.push({ y0: ly, y1: ly + row.h, person: row.person, letter: row.letter });
    }
    ly += row.h;
  }
  unmask();
  if (maxScroll > 0) {
    const thumbH = Math.max(6, Math.round((listH * listH) / contentH));
    const thumbY = listTop + Math.round((listH - thumbH) * (scroll / maxScroll));
    ink([...c.lines.slice(0, 3), 140]).box(screen.width - 3, thumbY, 2, thumbH);
  }
}

// 👥 The list: one row per person, their newest letter as a two-line preview.
function measurePeople(api, { wide, face, cw, lh, compact }) {
  const { ink, text, screen } = api;
  const t = T();
  const c = t.chat;
  const x = 6;
  const bounds = wide - 12;
  return people().map((person) => {
    const { latest } = person;
    const prefix = latest.mine ? `${S().you}: ` : "";
    let body = prefix + (latest.text || "").replace(/\s+/g, " ");
    const most = Math.floor(bounds / cw) * 2 - 1;
    if (body.length > most) body = body.slice(0, most) + "…";
    const textH = text.box(body, { x: 0, y: 0 }, bounds, 1, true, face).box.height;
    const h = lh + textH + (compact ? 5 : 8);
    return {
      person,
      h,
      paint(ly) {
        const unread = person.unread > 0;
        ink(unread ? t.stripeB : t.stripeA).box(x, ly, wide, h - 2);
        const yy = ly + 3;
        if (unread) ink(c.log).box(x + 2, yy + 2, 3, 3);
        const who = person.who + (person.unread > 1 ? `  ${person.unread}` : "");
        ink(unread ? c.handle : c.timestamp).write(who, { x: x + 8, y: yy }, undefined, undefined, false, face);
        const when = ago(latest.when);
        ink([...c.timestamp, 160]).write(when, { x: screen.width - x - 2 - when.length * cw - 2, y: yy }, undefined, undefined, false, face);
        ink(latest.mine ? [...c.messageText, 170] : c.messageText).write(body, { x: x + 8, y: yy + lh }, undefined, bounds, true, face);
      },
    };
  });
}

// 💬 A conversation: theirs on the left, yours inset on the right, oldest at
// the top. Every letter shows whole, with its media.
function measureConvo(api, { wide, face, cw, lh, compact }) {
  const { ink, text, screen } = api;
  const t = T();
  const c = t.chat;
  const x = 6;
  const INSET = Math.min(36, Math.floor(wide / 6));
  const bounds = wide - INSET - 12;
  return convo.letters.map((letter) => {
    const body = letter.subject && !/^re:/i.test(letter.subject) ? `${letter.subject}\n${letter.text || ""}` : letter.text || "";
    const textH = text.box(body, { x: 0, y: 0 }, bounds, 1, true, face).box.height;
    const media = mediaView.layout(api, letter, bounds, S());
    const h = lh + textH + (compact ? 5 : 8) + media.reduce((n, m) => n + m.height, 0);
    return {
      letter,
      h,
      paint(ly) {
        const lx = letter.mine ? x + INSET : x;
        const w = wide - INSET;
        const unread = !letter.mine && !letter.read;
        ink(letter.mine ? [...t.stripeA, 140] : unread ? t.stripeB : t.stripeA).box(lx, ly, w, h - 2);
        const yy = ly + 3;
        const when = ago(letter.when);
        const whenX = letter.mine ? lx + w - 2 - when.length * cw - 2 : lx + 8;
        ink([...c.timestamp, 160]).write(when, { x: whenX, y: yy }, undefined, undefined, false, face);
        ink(letter.mine ? [...c.messageText, 200] : c.messageText).write(body, { x: lx + 8, y: yy + lh }, undefined, bounds, true, face);
        let my = yy + lh + textH + 2;
        for (const item of media) {
          mediaView.paint(api, item, lx + 8, my, bounds, c.painting, S());
          mediaHits.push({ x: lx + 8, y: my, w: bounds, h: item.height, item });
          my += item.height;
        }
      },
    };
  });
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
    else if (type === "back") leaveConvo(api);
    else if (type === "reply" && convo) write(api, convo.address, convoSubject());
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

  if (e.is("keyboard:down:enter")) {
    convo ? write(api, convo.address, convoSubject()) : write(api);
    return void needsPaint();
  }
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace") || e.is("keyboard:down:`")) {
    return void (convo ? leaveConvo(api) : api.jump("prompt"));
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
    if (row?.person) openConvo(api, row.person.address, row.person.who);
    needsPaint();
  }
}

function leave() {
  mediaView.clear();
  convo = null;
  testChannel?.close();
  testChannel = null;
  testApi = null;
}

export { meta, boot, sim, paint, act, leave };
