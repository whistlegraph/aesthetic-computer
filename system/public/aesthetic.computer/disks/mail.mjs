// Mail, 2026.2.12 → 2026.9.14 (`amail` for a day; that path still aliases here)
// The post of aesthetic.computer. `mail @handle words...` sends from the
// prompt; `mail` lands here; `mail~@handle` lands in compose, addressed.
// Public media codes work like chat; outside attachments stay in the mailbox.
//
// Mail wears the same tema and speaks the same language as `laklok` — one
// saved choice each, one census — so the two rooms read as one house. Its QR
// sits on pale blue paper where laklok's sits on white, so the corners tell
// the rooms apart. Signed out, the room is a short notice of what mail is,
// with the door to signing up.

import {
  LAK_THEMES,
  realtimeTick,
  pickTema,
  restoreTema,
  saveTema,
  reportTema,
  strings,
  pickLang,
  restoreLang,
  saveLang,
  makeQR,
  paintQR,
  paintGear,
  GEAR,
  paintEnvelope,
  ENVELOPE,
  paintBadge,
  badgeWidth,
  paintChip,
  chipWidth,
  CHIP_H,
  CHIP_FONT,
  paintTemaPane,
  temaRow,
  langRow,
} from "./common/laklok-tema.mjs";
import { MailMedia } from "./common/mail-media.mjs";

const mediaView = new MailMedia();
let mediaHits = [];
let mediaNote = null;
let downloading = false;

let view = "inbox"; // inbox · sent · prefs · compose
let status = "loading"; // loading, loaded, error, noauth
let mail = null;
let prefs = null; // blast subscription + history, fetched when prefs opens
let errorMsg = null;
let rows = []; // [{ y0, y1, who }] — paint measures them, act replies to them
let ellipsisTicker;
let busy = false;
let fields; // to · subject · body, sharing one keyboard (see lib/type.mjs)
let composeNote = null; // what went wrong with the last send, if anything
let pendingTo = null; // an address that arrived in the URL, waiting for the box

// 🎛️ The compact controls — tabs and chips — register hit boxes each paint,
// the way the pane's chips do, instead of carrying Button objects around.
let hits = []; // [{ x, y, w, h, action }]

// 📜 The letters scroll under the header. `scroll` is how far the list has
// been pulled up; paint clamps it once it knows the content's height.
let scroll = 0;
let listTop = 0; // where the list begins this frame — drags below it scroll
let dragged = false; // a drag is not a tap on a letter
let showPerma = false; // the permahandle spelling, revealed on tap

// 👗 The tema and 🗣️ language, shared with laklok via the same store keys.
let tema = "ler";
let lang = "en"; // mail is for every handle, so English until told otherwise
let settingsOpen = false;
let gearBox = null; // {x, y, w, h} hit area for the ⚙ toggle
let settingsHits = []; // [{x, y, w, h, action}] chips, rebuilt each paint
let qrCells = null; // aesthetic.computer/mail, top-right
const QR_PAPER = [226, 238, 255]; // pale blue — laklok's is white

function meta() {
  return {
    title: "Mail — aesthetic.computer",
    desc: "Your aesthetic.computer mail — letters between handles.",
  };
}

// The current palette and words, read fresh each frame so the `realtime`
// tema's minute drift and a language switch show up without anyone asking.
const T = () => LAK_THEMES[tema];
const S = () => strings(lang);

// Each tab keeps its own hue so you can tell them apart at a glance.
const TABS = {
  inbox: [120, 190, 255],
  sent: [255, 190, 90],
  prefs: [200, 150, 255],
};
const WRITE = [110, 230, 165]; // the write / send accent

// The compose fields dress in the tema too.
function fieldScheme() {
  const t = T();
  return {
    text: t.chat.messageText,
    background: [...t.stripeA, 220],
    block: t.chat.handle,
    highlight: 0,
    guideline: [...t.chat.lines.slice(0, 3), 128],
  };
}

function wearTema(name) {
  tema = name;
  if (fields?.input) fields.input.scheme = fieldScheme();
}

// The field labels are words, so a language switch rebuilds the fields.
function makeFields(api) {
  const s = S();
  fields = new api.ui.TextFields(
    api,
    [
      { name: "to", label: s.to, placeholder: "@handle or email" },
      { name: "subject", label: s.re, placeholder: s.optional },
      { name: "body", label: s.say, lines: 4, placeholder: "…" },
    ],
    (letter) => send(api, letter),
    { scheme: fieldScheme() },
  );
}

function speak(api, name) {
  lang = name;
  if (view === "compose") leaveCompose(api, "inbox");
  makeFields(api);
}

// 🥾 Boot
async function boot(api) {
  const { user, gizmo, hud, net, store, colon, params } = api;
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  settingsOpen = false;
  view = "inbox";
  composeNote = null;
  hits = [];
  mediaView.clear();
  mediaNote = null;
  downloading = false;

  // 👗 A colon/`~` token pins a tema or language (`mail~skov`, `mail~da`);
  // otherwise the saved ones.
  const tokens = [...(colon || []), ...(params || [])];
  const picked = pickTema(tokens, store);
  wearTema(picked.name);
  restoreTema(store, picked.pinned, (saved) => {
    if (saved) wearTema(saved);
  });
  const spoken = pickLang(tokens, store, "en");
  lang = spoken.name;
  restoreLang(store, spoken.pinned, (saved) => {
    if (saved) speak(api, saved);
  });

  qrCells = makeQR("https://aesthetic.computer/mail");
  makeFields(api);

  // `mail~@handle` — arrived with an address (a tapped handle in laklok);
  // open straight onto a letter to them once the box has loaded.
  pendingTo = tokens.find((t) => /^@\w/.test(t) || /^ac\d\d[a-z]{5}$/.test(t)) || null;

  if (!user) {
    status = "noauth";
    return;
  }

  await refresh(api);
  if (pendingTo && status === "loaded") compose(api, pendingTo);
  // The census waits its turn: two authorized requests in flight at once
  // used to lose one (disk.mjs kept a single pending authorization), and a
  // lost inbox fetch left boot hanging on the noise forever.
  reportTema(net, tema);
}

async function refresh({ net }) {
  try {
    const res = await net.userRequest("GET", "/api/mail");
    if (res.status === 200) {
      mail = res;
      status = "loaded";
    } else {
      status = "error";
      errorMsg = `${res.status}`;
    }
  } catch (err) {
    status = "error";
    errorMsg = "Could not load letters";
  }
}

// Post the letter, then show it in `sent`.
async function send(api, { to, subject, body }) {
  const s = S();
  const text = (body || "").trim();
  if (!to?.trim()) {
    composeNote = s.whoTo;
    return;
  }
  if (!text) {
    composeNote = s.nothingToSay;
    return;
  }

  let res;
  try {
    res = await api.net.userRequest("POST", "/api/mail", {
      to: to.trim(),
      subject,
      text,
    });
  } catch {
    // Request errors may embed submitted content. Keep them out of piece logs.
    composeNote = s.couldntSend;
    return;
  }

  if (res.status === 200) {
    composeNote = null;
    fields.reset(api);
    leaveCompose(api, "sent");
    await refresh(api);
  } else if (res.status === 404) {
    composeNote = `${s.noOne} ${to.trim()}`;
  } else {
    composeNote = s.couldntSend;
  }
}

// Put the fields away and go somewhere.
function leaveCompose(api, to) {
  view = to;
  composeNote = null;
  fields.input.mute = true;
  api.send({ type: "keyboard:close" });
}

// Open compose, optionally already addressed to someone — a reply lands on the
// subject, since the `to` is already answered.
function compose(api, to) {
  view = "compose";
  composeNote = null;
  fields.input.mute = false;
  // `focus` syncs the live buffer back into the field it is leaving first, so
  // the address has to land after the focus has moved off the `to` row.
  fields.focus(to ? 1 : 0, api);
  if (to) fields.values.to = to;
}

// 🧮 Sim
function sim(api) {
  ellipsisTicker?.update(api.clock.time());
  if (view === "compose") fields.sim(api);
  // The signed-in user can settle a beat after boot; when they do, fetch.
  if (status === "noauth" && api.user) {
    status = "loading";
    refresh(api).then(() => {
      if (status === "loaded" && pendingTo) compose(api, pendingTo);
    });
  }
  // 🌅 The ambient tema drifts a hair each minute; paint reads it live.
  if (tema === "realtime" && realtimeTick()) wearTema(tema);
}

const PERMA = /^ac\d\d[a-z]{5}@/;

// How long ago, short enough to sit next to a handle.
function ago(when) {
  const secs = (Date.now() - new Date(when).getTime()) / 1000;
  if (secs < 60) return "now";
  if (secs < 3600) return `${Math.floor(secs / 60)}m`;
  if (secs < 86400) return `${Math.floor(secs / 3600)}h`;
  if (secs < 604800) return `${Math.floor(secs / 86400)}d`;
  return new Date(when).toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
  });
}

// 📱 The top-right corner, right to left: the mail QR on pale paper, the ⚙
// gear, and the envelope with its unread count — the same cluster laklok
// wears, so the eye finds it in the same place in both rooms.
function paintCorner(api) {
  const { screen } = api;
  if (!qrCells) return;
  const qrBoxSize = qrCells.length + 2;
  const qrX = screen.width - qrBoxSize - 3;
  const qrY = 3;
  paintQR(api, qrCells, qrX, qrY, QR_PAPER);

  const gx = qrX - GEAR - 4;
  const gy = qrY + Math.floor((qrBoxSize - GEAR) / 2);
  gearBox = paintGear(api, gx, gy, settingsOpen);

  if (status !== "loaded") return;
  const unread = mail.unread || 0;
  const label = unread > 0 ? `${unread}` : null;
  const cluster = ENVELOPE.w + (label ? badgeWidth(label) - 3 : 0);
  const ex = gearBox.x - 6 - cluster;
  const ey = qrY + Math.floor((qrBoxSize - ENVELOPE.h) / 2);
  // The flap stands open while the inbox is the tab you're on.
  paintEnvelope(api, ex, ey, { lit: unread > 0, open: view === "inbox" || view === "compose" });
  if (label) paintBadge(api, ex + ENVELOPE.w - 3, ey - 5, label);
}

// ⚙️ The indstillinger pane — tema and sprog; mode and filter are the chat's
// business.
function paintSettings(api) {
  settingsHits = [];
  if (!settingsOpen) return;
  const { screen } = api;
  const s = S();
  const qrBoxSize = (qrCells?.length || 0) + 2;
  settingsHits = paintTemaPane(api, {
    theme: T(),
    rows: [temaRow(tema, s), langRow(lang, s)],
    right: screen.width - 3,
    top: 3 + qrBoxSize + 4,
    title: s.settings,
  });
}

// A chip that does something — painted, then remembered for act().
function control(api, x, y, text, action, opts) {
  const box = paintChip(api, x, y, text, opts);
  hits.push({ ...box, action });
  return box;
}

// 📣 Signed out, the room explains itself and offers the door. Everything the
// letters would use — tema, corner, pane — is already here, so a visitor sees
// the room they'd get.
function paintNotice(api, x, wide) {
  const { ink, screen } = api;
  const s = S();
  const c = T().chat;
  let y = 30;

  ink(c.handle).write(s.adTitle, { x, y });
  y += 14;
  // Measure the wrapped body first so the lines below sit under it.
  const bodyH = api.text.box(s.adBody, { x, y }, wide).box.height;
  ink(c.messageText).write(s.adBody, { x, y }, undefined, wide, true);
  y += bodyH + 8;
  ink(c.timestamp).write(s.adPrompt, { x, y }, undefined, wide, true, CHIP_FONT);
  y += 16;

  const signup = control(api, x, y, s.signup, { type: "signup" }, { selected: true });
  control(api, signup.x + signup.w + 6, y, s.login, { type: "login" });
  void screen;
}

// 🎨 Paint
function paint(api) {
  const { wipe, ink, screen, text, help, mask, unmask } = api;
  const t = T();
  const c = t.chat;
  const s = S();
  hits = [];
  mediaHits = [];
  wipe(...t.bg);

  const x = 6;
  const wide = screen.width - x * 2;

  if (status === "loading") {
    ink(c.timestamp).write(
      s.loading + ellipsisTicker.text(help.repeat, { pad: false }),
      { center: "xy", size: 1 },
    );
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (status === "noauth") {
    paintNotice(api, x, wide);
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (status === "error") {
    ink(200, 100, 100).write(`${s.error}: ${errorMsg || "?"}`, { center: "xy" });
    paintCorner(api);
    paintSettings(api);
    return;
  }

  let y = 6; // the hud label is already the title — don't write a second one
  // The corner cluster owns the first row — and the hud label's hitbox is at
  // least 50×20 from (6,6), swallowing any tap under it, so the address line
  // starts below that.
  y += 22;

  // 📱 A phone-sized or dense screen switches the room to the chunky face:
  // 4px characters, 9px lines, so more letters fit a screen.
  const compact = screen.width < 320 || screen.height < 220;
  const face = compact ? CHIP_FONT : undefined;
  const cw = compact ? 4 : 6; // character advance
  const lh = compact ? 9 : 11; // line height

  // One address on show — the @handle. A tap reveals the permahandle
  // spelling under it; it never changes and reads like a serial number, so
  // it wears MatrixChunky8 whatever the face.
  const handleAddress = mail.addresses.find((a) => !PERMA.test(a)) || mail.addresses[0] || "";
  const permaAddress = mail.addresses.find((a) => PERMA.test(a));
  ink(c.handle).write(handleAddress, { x, y }, undefined, undefined, false, face);
  hits.push({ x: x - 2, y: y - 2, w: handleAddress.length * cw + 6, h: lh + 4, action: { type: "perma" } });
  y += lh;
  if (showPerma && permaAddress) {
    ink(c.log).write(permaAddress, { x, y: y + 1 }, undefined, undefined, false, CHIP_FONT);
    y += 9;
  }
  y += 4;

  // 🗂️ Tabs — words on a line, the open one underlined in its hue. Actions
  // sit at the right end of the same line as chips, so the header is one row.
  let tx = x;
  for (const name of ["inbox", "sent", "prefs"]) {
    const label = s[name];
    const w = label.length * cw;
    const open = view === name || (view === "compose" && name === "inbox");
    ink(open ? c.messageText : c.timestamp).write(label, { x: tx, y }, undefined, undefined, false, face);
    if (open) ink(...TABS[name]).box(tx, y + lh - 1, w, 1);
    hits.push({ x: tx - 2, y: y - 3, w: w + 6, h: lh + 4, action: { type: "view", value: name } });
    tx += w + (compact ? 8 : 12);
  }
  // Right end: [mark read] [write] — write lights up while composing.
  let cx = screen.width - x - chipWidth(s.write);
  control(api, cx, y - 1, s.write, { type: "write" }, { selected: view === "compose", tint: WRITE });
  if (view === "inbox" && mail.unread > 0) {
    cx -= chipWidth(s.markRead) + 6;
    control(api, cx, y - 1, s.markRead, { type: "read" }, { tint: c.log, dim: busy });
  }
  y += lh + 4;

  ink(c.lines).box(x, y, wide, 1);
  y += 6;
  if (mediaNote) {
    ink(c.timestamp).write(mediaNote, { x, y }, undefined, wide, true, CHIP_FONT);
    y += text.box(mediaNote, undefined, wide, 1, true, CHIP_FONT).box.height + 4;
  }

  // Compose sits in the room instead of replacing it — the addresses and tabs
  // stay put and the field takes the space the letters were using.
  if (view === "compose") {
    const frame = {
      x,
      y,
      width: wide,
      height: Math.min(110, Math.max(64, screen.height - y - 40)),
    };
    fields.paint(api, frame);

    const footer = frame.y + frame.height + 4;
    const sendBox = control(api, x, footer, s.send, { type: "send" }, { selected: true });
    ink(composeNote ? [255, 130, 130] : c.timestamp).write(
      composeNote || s.composeHint,
      { x: sendBox.x + sendBox.w + 6, y: footer + 2 },
      undefined,
      wide - sendBox.w - 6,
      false,
      CHIP_FONT,
    );
    ink(c.timestamp).write(s.mediaHint, { x, y: footer + CHIP_H + 4 }, undefined, wide, true, CHIP_FONT);
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (view === "prefs") {
    paintPrefs(api, x, y, wide);
    paintCorner(api);
    paintSettings(api);
    return;
  }

  const letters = view === "inbox" ? mail.inbox : mail.sent;

  if (letters.length === 0) {
    ink(c.timestamp).write(view === "inbox" ? s.noMail : s.nothingSent, { x, y });
    if (view === "inbox") {
      ink([...c.timestamp, 160]).write(s.tryHint, { x, y: y + 12 });
    }
    paintCorner(api);
    paintSettings(api);
    return;
  }

  // 📜 The list. Everything from here down is the letters', clipped to the
  // screen and pulled up by `scroll`. Measure first so the whole height is
  // known, then draw only the rows that show. In the compact face a letter
  // gets two preview lines at most — the whole thing opens on tap.
  const bounds = wide - 10;
  const smallTint = compact ? 0 : 2; // the chunky annotations sit 2px low in the big face
  listTop = y;
  const listH = screen.height - 4 - listTop;
  rows = [];

  const measured = letters.map((letter) => {
    let body = letter.text;
    if (compact) {
      const most = Math.floor(bounds / cw) * 2 - 1;
      if (body.length > most) body = body.slice(0, most) + "…";
    }
    const h = text.box(body, { x: x + 10, y: 0 }, bounds, 1, true, face).box.height;
    const mediaItems = mediaView.layout(api, letter, bounds, s);
    return { letter, body, mediaItems, textH: h, rowH: h + lh + (compact ? 5 : 8) + mediaItems.reduce((n, item) => n + item.height, 0) };
  });
  const contentH = measured.reduce((sum, m) => sum + m.rowH, 0);
  const maxScroll = Math.max(0, contentH - listH);
  scroll = Math.max(0, Math.min(maxScroll, scroll));

  mask({ x: 0, y: listTop, width: screen.width, height: listH });
  let ly = listTop - scroll;
  measured.forEach(({ letter, body, rowH, mediaItems, textH }, i) => {
    if (ly + rowH >= listTop && ly < listTop + listH) {
      const unread = view === "inbox" && !letter.read;
      const who = (view === "inbox" ? letter.from : letter.to) || "someone";
      // An answer to an outside letter goes back out as email; act() reads
      // `email` off the row for that.
      const email = view === "inbox" ? letter.fromEmail : letter.toEmail;
      const yy = ly + 3;

      // Stripe the row behind everything, so a long message stays one block —
      // the tema's stripes, unread rows on the brighter one.
      ink(unread ? t.stripeB : i % 2 ? t.stripeA : [...t.stripeA, 110]).box(x, ly, wide, rowH);
      rows.push({ y0: ly, y1: ly + rowH, who, email });

      if (unread) ink(c.log).box(x + 2, yy + 2, 3, 3);
      ink(unread ? c.handle : c.timestamp).write(who, { x: x + 8, y: yy }, undefined, undefined, false, face);
      // A letter from outside the wall carries the sender's address after
      // their name, small, so an email reads apart from a handle at a glance —
      // and a word of warning when Google's gate couldn't vouch for the sender.
      let afterWho = x + 8 + (who.length + 1) * cw;
      if (email && email !== who) {
        ink([...c.timestamp, 170]).write(email, { x: afterWho, y: yy + smallTint }, undefined, undefined, false, CHIP_FONT);
        afterWho += email.length * 4 + 6;
      }
      if (view === "inbox" && letter.fromEmail && letter.auth && !letter.auth.verified) {
        ink(255, 140, 140).write(s.unverified, { x: afterWho, y: yy + smallTint }, undefined, undefined, false, CHIP_FONT);
        afterWho += s.unverified.length * 4 + 6;
      } else if (view === "sent" && letter.toEmail) {
        ink([...c.timestamp, 170]).write(s.outside, { x: afterWho, y: yy + smallTint }, undefined, undefined, false, CHIP_FONT);
        afterWho += s.outside.length * 4 + 6;
      }
      const agoW = (compact ? 4 : 6) * 5 + 4;
      if (letter.subject) {
        // Cut the subject to the room left before the timestamp — `write`
        // with a bound and no wrap still runs on under the clock.
        const room = Math.floor((screen.width - x - agoW - afterWho) / cw);
        let subject = letter.subject;
        if (subject.length > room) subject = room > 1 ? subject.slice(0, room - 1) + "…" : "";
        ink(unread ? c.painting : [...c.painting, 150]).write(subject, { x: afterWho, y: yy }, undefined, undefined, false, face);
      }
      ink([...c.timestamp, 160]).write(ago(letter.when), { x: screen.width - x - agoW + 4, y: yy }, undefined, undefined, false, face);
      ink(unread ? c.messageText : [...c.messageText, 190]).write(body, { x: x + 10, y: yy + lh }, undefined, bounds, true, face);
      let mediaY = yy + lh + textH + 2;
      for (const item of mediaItems) {
        if (mediaY + item.height > listTop && mediaY < screen.height - 4) {
          mediaView.paint(api, item, x + 10, mediaY, bounds, c.painting, s);
          mediaHits.push({ x: x + 10, y: Math.max(listTop, mediaY), w: bounds,
            h: Math.min(screen.height - 4, mediaY + item.height) - Math.max(listTop, mediaY), item });
        }
        mediaY += item.height;
      }
    }
    ly += rowH;
  });
  unmask();

  // A thin thumb at the right edge when there is more than fits.
  if (maxScroll > 0) {
    const thumbH = Math.max(6, Math.round((listH * listH) / contentH));
    const thumbY = listTop + Math.round((listH - thumbH) * (scroll / maxScroll));
    ink([...c.lines.slice(0, 3), 140]).box(screen.width - 3, thumbY, 2, thumbH);
  }

  paintCorner(api);
  paintSettings(api);
}

function paintPrefs(api, x, y, wide) {
  const { ink, screen } = api;
  const c = T().chat;
  const s = S();

  if (!prefs) {
    ink(c.timestamp).write(s.loadingPrefs, { x, y });
    return;
  }

  if (prefs.email) {
    ink(c.timestamp).write(prefs.email, { x, y });
    y += 12;

    if (prefs.subscribed) {
      ink(120, 200, 120).write(s.subscribed, { x, y });
      control(api, x + s.subscribed.length * 6 + 8, y - 1, s.unsubscribe, { type: "unsubscribe" }, { tint: [230, 120, 120], dim: busy });
    } else {
      ink(200, 120, 80).write(s.unsubscribed, { x, y });
      control(api, x + s.unsubscribed.length * 6 + 8, y - 1, s.subscribe, { type: "subscribe" }, { tint: [120, 220, 130], dim: busy });
    }
    y += 16;
  }

  if (busy) {
    ink(c.timestamp).write(s.updating, { x, y });
    y += 14;
  }

  ink(c.kidlisp).write(s.blastHistory, { x, y });
  y += 14;

  if (!prefs.blasts || prefs.blasts.length === 0) {
    ink(c.timestamp).write(s.noBlasts, { x, y });
    return;
  }

  ink([...c.timestamp, 160]).write(
    `${prefs.count} blast${prefs.count !== 1 ? "s" : ""} · ${prefs.totalSent} sent · ${prefs.totalUnsubscribed} unsub`,
    { x, y },
  );
  y += 14;

  for (const blast of prefs.blasts) {
    if (y > screen.height - 16) break;
    const color =
      blast.status === "completed"
        ? [80, 180, 80]
        : blast.status === "in-progress"
          ? [200, 180, 80]
          : [180, 80, 80];
    ink(...color).write(blast.status, { x, y });
    ink(c.timestamp).write(ago(blast.when), { x: x + 80, y });
    ink([...c.timestamp, 160]).write(`${blast.sent}/${blast.totalAttempted}`, { x: x + 130, y });
    y += 10;
    ink([...c.messageText, 160]).write(blast.subject || "(no subject)", { x: x + 8, y }, undefined, wide - 10);
    y += 14;
  }
}

// 🎪 Act
function act(api) {
  const { event: e, net, needsPaint, store } = api;

  const hit = (box) =>
    box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;

  // ⚙️ Gear toggles the pane; while open, the pane owns every pointer event.
  if (e.is("touch") && hit(gearBox)) {
    settingsOpen = !settingsOpen;
    needsPaint();
    return;
  }

  if (settingsOpen && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    if (e.is("touch")) {
      const chip = settingsHits.find((h) => hit(h));
      if (chip) {
        const { type, value } = chip.action;
        if (type === "theme") {
          wearTema(value);
          saveTema(store, value);
          if (value === "realtime") realtimeTick(); // catch up before first paint
          if (status === "loaded") reportTema(net, value);
        } else if (type === "lang") {
          speak(api, value);
          saveLang(store, value);
        }
      } else if (!hit(settingsHits.pane)) {
        settingsOpen = false; // Tap outside closes.
      }
      needsPaint();
    }
    return;
  }

  // The compact controls, wherever they were painted this frame.
  const control = e.is("touch") ? hits.find((h) => hit(h)) : null;
  if (control) {
    const { type, value } = control.action;
    if (type === "signup") net.signup();
    else if (type === "login") net.login();
    else if (type === "perma") showPerma = !showPerma;
    else if (type === "view") {
      if (view === "compose") leaveCompose(api, value);
      else view = value;
      scroll = 0;
      if (value === "prefs" && !prefs) {
        net.userRequest("GET", "/api/mail-status").then((res) => {
          if (res.status === 200) prefs = res;
          needsPaint();
        }).catch(() => {
          needsPaint();
        });
      }
    } else if (type === "write") {
      if (view === "compose") leaveCompose(api, "inbox");
      else compose(api);
    } else if (type === "send") {
      fields.sync();
      send(api, { ...fields.values });
    } else if (type === "read" && !busy) {
      busy = true;
      net.userRequest("POST", "/api/mail", { action: "read" }).then((res) => {
        if (res.status === 200) {
          mail.unread = 0;
          mail.inbox.forEach((letter) => (letter.read = true));
        }
        busy = false;
        needsPaint();
      }).catch(() => {
        busy = false;
        needsPaint();
      });
    } else if ((type === "subscribe" || type === "unsubscribe") && !busy) {
      busy = true;
      net.userRequest("POST", "/api/mail-status", { action: type }).then((res) => {
        if (res.status === 200) prefs.subscribed = type === "subscribe";
        busy = false;
        needsPaint();
      }).catch(() => {
        busy = false;
        needsPaint();
      });
    }
    needsPaint();
    return;
  }

  if (status !== "loaded") return;

  if (view === "compose") {
    if (e.is("keyboard:down:escape")) {
      leaveCompose(api, "inbox");
      needsPaint();
      return;
    }
    if (fields.act(api)) needsPaint();
    return;
  }

  // Off the compose field these keys mean what they mean everywhere else.
  if (e.is("keyboard:down:enter")) {
    compose(api);
    needsPaint();
    return;
  }
  if (
    e.is("keyboard:down:escape") ||
    e.is("keyboard:down:backspace") ||
    e.is("keyboard:down:`")
  ) {
    api.jump("prompt");
    return;
  }

  // 📜 Scrolling the letters — a drag below the header, or the wheel. Same
  // signs as the chat: `scroll` grows as you read further down.
  const listing = view === "inbox" || view === "sent";
  if (listing) {
    if (e.is("touch")) dragged = false;
    if (e.is("draw") && e.y >= listTop) {
      scroll -= e.delta.y;
      dragged = true;
      needsPaint();
      return;
    }
    if (e.is("scroll")) {
      scroll += e.y;
      needsPaint();
      return;
    }
  }

  // Tap a letter to answer it — the field opens already addressed. A drag
  // that ended on a letter was a scroll, not a tap.
  if (e.is("lift") && !dragged && listing) {
    const media = mediaHits.find((box) => hit(box));
    if (media) {
      if (!downloading) {
        downloading = true;
        mediaNote = media.item.file ? S().downloading : null;
        mediaView.open(api, media.item).then(() => { mediaNote = null; })
          .catch(() => { mediaNote = S().downloadFailed; })
          .finally(() => { downloading = false; needsPaint(); });
      }
      needsPaint();
      return;
    }
    const row = rows.find((r) => e.y >= r.y0 && e.y < r.y1);
    const address = row?.who?.startsWith("@") ? row.who : row?.email;
    if (address && e.y >= listTop) {
      compose(api, address);
      needsPaint();
    }
  }
}

function leave() { mediaView.clear(); }

export { meta, boot, sim, paint, act, leave };
