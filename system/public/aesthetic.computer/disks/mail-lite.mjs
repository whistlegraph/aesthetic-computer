// Mail Lite, 2026.10.14
// A trial of a smaller `mail`: one list of letters in and out, newest first,
// and a two-field compose — who, and what. Replies carry the thread's subject
// on their own. Tema and language follow laklok's saved choice; there is no
// settings pane, no tabs, no prefs. `mail-lite~@handle` opens addressed.

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
let pendingTo = null;
let expandedId = null;
const reading = new Set();
let hits = [];
let rows = [];
let mediaHits = [];
let mediaNote = null;
let scroll = 0;
let listTop = 0;
let dragged = false;
let tema = "ler";
let lang = "en";
let ellipsisTicker;

const T = () => LAK_THEMES[tema];
const S = () => strings(lang);

function meta() {
  return {
    title: "Mail Lite — aesthetic.computer",
    desc: "Letters between handles, in one list.",
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
  scroll = 0;
  mediaView.clear();

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
    return;
  }
  await refresh(api);
  if (pendingTo && status === "loaded") write(api, pendingTo);
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
    scroll = 0;
    await refresh(api);
  } else if (res.status === 404) {
    note = `${s.noOne} ${to.trim()}. ${s.fullHandle}`;
  } else {
    note = s.couldntSend;
  }
  api.needsPaint();
}

// Open compose; a reply lands on the body with its address filled.
function write(api, to, subject) {
  composing = true;
  note = null;
  // Only a reply is a thread; an address from the URL is just an address.
  thread = to && subject !== undefined
    ? { who: to, subject: subject ? (/^re:/i.test(subject) ? subject : `Re: ${subject}`) : "" }
    : null;
  fields.input.mute = false;
  fields.focus(to ? 1 : 0, api);
  if (to) fields.values.to = to;
}

function close(api) {
  composing = false;
  note = null;
  thread = null;
  fields.input.mute = true;
  api.send({ type: "keyboard:close" });
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
  }
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

  // The top row belongs to the hud label on the left, and on the right the
  // one control: write (or close), with the unread envelope beside it.
  const top = 6;
  const label = composing ? "×" : s.write;
  const wx = screen.width - x - chipWidth(label);
  chip(api, wx, top, label, { type: "write" }, { selected: composing, tint: WRITE });
  if (mail.unread > 0) {
    const n = `${mail.unread}`;
    const ex = wx - 8 - ENVELOPE.w - badgeWidth(n) + 3;
    paintEnvelope(api, ex, top, { lit: true });
    paintBadge(api, ex + ENVELOPE.w - 3, top - 4, n);
  }
  let y = 28;

  if (composing) {
    if (thread) {
      ink(c.timestamp).write(s.replyingTo, { x, y }, undefined, undefined, false, CHIP_FONT);
      const subjectX = x + (s.replyingTo.length + 1) * 4;
      ink(c.handle).write(thread.who, { x: subjectX, y }, undefined, undefined, false, CHIP_FONT);
      y += 10;
    }
    const frame = { x, y, width: wide, height: Math.min(120, Math.max(60, screen.height - y - 30)) };
    fields.paint(api, frame);
    const footer = frame.y + frame.height + 4;
    const sendBox = chip(api, x, footer, s.send, { type: "send" }, { selected: true, dim: sending });
    if (note) {
      ink(255, 130, 130).write(note, { x: sendBox.x + sendBox.w + 6, y: footer + 2 }, undefined, wide - sendBox.w - 6, true, CHIP_FONT);
    }
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
  const listH = screen.height - 4 - listTop;

  const measured = letters.map((letter) => {
    const open = letter.id === expandedId;
    let body = letter.text || "";
    if (!open) {
      const most = Math.floor(bounds / cw) * 2 - 1;
      if (body.length > most) body = body.slice(0, most) + "…";
    }
    const textH = text.box(body, { x: 0, y: 0 }, bounds, 1, true, face).box.height;
    const media = open ? mediaView.layout(api, letter, bounds, S()) : [];
    const h = lh + textH + (compact ? 5 : 8) + media.reduce((n, m) => n + m.height, 0);
    return { letter, body, textH, media, h };
  });
  const contentH = measured.reduce((n, m) => n + m.h, 0);
  const maxScroll = Math.max(0, contentH - listH);
  scroll = Math.max(0, Math.min(maxScroll, scroll));

  mask({ x: 0, y: listTop, width: screen.width, height: listH });
  let ly = listTop - scroll;
  for (const { letter, body, textH, media, h } of measured) {
    if (ly + h >= listTop && ly < listTop + listH) {
      const unread = !letter.mine && !letter.read;
      const lx = x + (letter.mine ? 12 : 0);
      ink(unread ? t.stripeB : letter.mine ? [...t.stripeA, 90] : t.stripeA).box(lx, ly, wide - (lx - x), h - 2);
      const row = { y0: ly, y1: ly + h, letter, reply: null };
      rows.push(row);
      const yy = ly + 3;
      if (unread) ink(c.log).box(lx + 2, yy + 2, 3, 3);
      const who = (letter.mine ? "→ " : "") + (letter.who || "someone");
      ink(unread ? c.handle : c.timestamp).write(who, { x: lx + 8, y: yy }, undefined, undefined, false, face);
      let right = screen.width - x;
      if (!letter.mine) {
        right -= chipWidth(s.reply);
        row.reply = paintChip(api, right, yy - 1, s.reply, { tint: WRITE });
        right -= 4;
      }
      ink([...c.timestamp, 160]).write(ago(letter.when), { x: right - cw * 4, y: yy }, undefined, undefined, false, face);
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

  const control = e.is("touch") ? hits.find(hit) : null;
  if (control) {
    const { type } = control.action;
    if (type === "signup") net.signup();
    else if (type === "login") net.login();
    else if (type === "write") composing ? close(api) : write(api);
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
  if (e.is("draw") && e.y >= listTop) {
    scroll -= e.delta.y;
    dragged = true;
    return void needsPaint();
  }
  if (e.is("scroll")) {
    scroll += e.y;
    return void needsPaint();
  }

  if (e.is("lift") && !dragged && e.y >= listTop) {
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
    if (hit(row.reply)) {
      const { letter } = row;
      const address = letter.from?.startsWith("@") || PERMA.test(letter.from || "") ? letter.from : letter.fromEmail;
      if (address) write(api, address, letter.subject || "");
    } else {
      expandedId = expandedId === row.letter.id ? null : row.letter.id;
      if (expandedId) markRead(api, row.letter);
    }
    needsPaint();
  }
}

function leave() {
  mediaView.clear();
}

export { meta, boot, sim, paint, act, leave };
