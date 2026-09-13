// Amail, 2026.2.12 → 2026.9.13 (was `mail`; that path still aliases here)
// The post of aesthetic.computer. `amail @handle words...` sends from the
// prompt; `amail` lands here; `amail~@handle` lands in compose, addressed.
// Tier 1: nothing leaves aesthetic.computer. See `system/backend/mail.mjs`.
//
// Amail wears the same tema and speaks the same language as `laklok` — one
// saved choice each, one census — so the two rooms read as one house. Its QR
// sits on pale blue paper where laklok's sits on white, so the corners tell
// the rooms apart. Signed out, the room is a short notice of what amail is,
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

// 👗 The tema and 🗣️ language, shared with laklok via the same store keys.
let tema = "ler";
let lang = "en"; // amail is for every handle, so English until told otherwise
let settingsOpen = false;
let gearBox = null; // {x, y, w, h} hit area for the ⚙ toggle
let settingsHits = []; // [{x, y, w, h, action}] chips, rebuilt each paint
let qrCells = null; // aesthetic.computer/amail, top-right
const QR_PAPER = [226, 238, 255]; // pale blue — laklok's is white

function meta() {
  return {
    title: "Amail — aesthetic.computer",
    desc: "Your aesthetic.computer amail — letters between handles.",
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
      { name: "to", label: s.to, placeholder: "@handle or ac25namuc" },
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
  hud.label("amail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  settingsOpen = false;
  view = "inbox";
  composeNote = null;
  hits = [];

  // 👗 A colon/`~` token pins a tema or language (`amail~skov`, `amail~da`);
  // otherwise the saved ones.
  const tokens = [...(colon || []), ...(params || [])];
  const picked = pickTema(tokens, store);
  wearTema(picked.name);
  restoreTema(store, picked.pinned, (saved) => {
    if (saved) wearTema(saved);
    if (user) reportTema(net, tema); // after the saved tema has had its say
  });
  const spoken = pickLang(tokens, store, "en");
  lang = spoken.name;
  restoreLang(store, spoken.pinned, (saved) => {
    if (saved) speak(api, saved);
  });

  qrCells = makeQR("https://aesthetic.computer/amail");
  makeFields(api);

  // `amail~@handle` — arrived with an address (a tapped handle in laklok);
  // open straight onto a letter to them once the box has loaded.
  pendingTo = tokens.find((t) => /^@\w/.test(t) || /^ac\d\d[a-z]{5}$/.test(t)) || null;

  if (!user) {
    status = "noauth";
    return;
  }

  await refresh(api);
  if (pendingTo && status === "loaded") compose(api, pendingTo);
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
    errorMsg = err.message;
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

  const res = await api.net.userRequest("POST", "/api/mail", {
    to: to.trim(),
    subject,
    text,
  });

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

// 📱 The top-right corner, right to left: the amail QR on pale paper, the ⚙
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
  paintEnvelope(api, ex, ey, { lit: unread > 0 });
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
  const { wipe, ink, screen, text, help } = api;
  const t = T();
  const c = t.chat;
  const s = S();
  hits = [];
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
  y += 16; // the corner cluster owns the first row

  // The two spellings of this mailbox on one tight stack. The permahandle
  // never moves and reads like a serial number, so it wears MatrixChunky8;
  // the @handle is the human alias and stays in the normal face.
  for (const address of mail.addresses) {
    if (PERMA.test(address)) {
      ink(c.log).write(address, { x, y: y + 1 }, undefined, undefined, false, CHIP_FONT);
      y += 9;
    } else {
      ink(c.handle).write(address, { x, y });
      y += 11;
    }
  }
  y += 4;

  // 🗂️ Tabs — words on a line, the open one underlined in its hue. Actions
  // sit at the right end of the same line as chips, so the header is one row.
  let tx = x;
  for (const name of ["inbox", "sent", "prefs"]) {
    const label = s[name];
    const w = label.length * 6;
    const open = view === name || (view === "compose" && name === "inbox");
    ink(open ? c.messageText : c.timestamp).write(label, { x: tx, y });
    if (open) ink(...TABS[name]).box(tx, y + 10, w, 1);
    hits.push({ x: tx - 2, y: y - 3, w: w + 6, h: 15, action: { type: "view", value: name } });
    tx += w + 12;
  }
  // Right end: [mark read] [write] — write lights up while composing.
  let cx = screen.width - x - chipWidth(s.write);
  control(api, cx, y - 1, s.write, { type: "write" }, { selected: view === "compose", tint: WRITE });
  if (view === "inbox" && mail.unread > 0) {
    cx -= chipWidth(s.markRead) + 6;
    control(api, cx, y - 1, s.markRead, { type: "read" }, { tint: c.log, dim: busy });
  }
  y += 15;

  ink(c.lines).box(x, y, wide, 1);
  y += 6;

  // Compose sits in the room instead of replacing it — the addresses and tabs
  // stay put and the field takes the space the letters were using.
  if (view === "compose") {
    const frame = {
      x,
      y,
      width: wide,
      height: Math.min(110, Math.max(64, screen.height - y - 30)),
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

  const bounds = wide - 10;
  rows = [];

  letters.forEach((letter, i) => {
    if (y > screen.height - 14) return;
    const unread = view === "inbox" && !letter.read;
    const who = (view === "inbox" ? letter.from : letter.to) || "someone";
    const body = text.box(letter.text, { x: x + 8, y }, bounds).box.height;

    // Stripe the row behind everything, so a long message stays one block —
    // the tema's stripes, unread rows on the brighter one.
    ink(unread ? t.stripeB : i % 2 ? t.stripeA : [...t.stripeA, 110]).box(
      x,
      y - 3,
      wide,
      body + 17,
    );

    rows.push({ y0: y - 3, y1: y + body + 14, who });

    if (unread) ink(c.log).box(x + 2, y + 2, 3, 3);
    ink(unread ? c.handle : c.timestamp).write(who, { x: x + 8, y });
    if (letter.subject) {
      ink(unread ? c.painting : [...c.painting, 150]).write(
        letter.subject,
        { x: x + 8 + (who.length + 1) * 6, y },
        undefined,
        bounds - (who.length + 2) * 6,
      );
    }
    ink([...c.timestamp, 160]).write(ago(letter.when), { x: screen.width - 34, y });
    y += 11;

    ink(unread ? c.messageText : [...c.messageText, 190]).write(letter.text, { x: x + 10, y }, undefined, bounds);
    y += body + 6;
  });

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
    else if (type === "view") {
      if (view === "compose") leaveCompose(api, value);
      else view = value;
      if (value === "prefs" && !prefs) {
        net.userRequest("GET", "/api/mail-status").then((res) => {
          if (res.status === 200) prefs = res;
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
      });
    } else if ((type === "subscribe" || type === "unsubscribe") && !busy) {
      busy = true;
      net.userRequest("POST", "/api/mail-status", { action: type }).then((res) => {
        if (res.status === 200) prefs.subscribed = type === "subscribe";
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

  // Tap a letter to answer it — the field opens already addressed.
  if (e.is("touch") && (view === "inbox" || view === "sent")) {
    const row = rows.find((r) => e.y >= r.y0 && e.y < r.y1);
    if (row?.who?.startsWith("@")) {
      compose(api, row.who);
      needsPaint();
    }
  }
}

export { meta, boot, sim, paint, act };
