// Amail, 2026.2.12 → 2026.9.13 (was `mail`; that path still aliases here)
// The post of aesthetic.computer. `amail @handle words...` sends from the
// prompt; `amail` lands here; `amail~@handle` lands in compose, addressed.
// Tier 1: nothing leaves aesthetic.computer. See `system/backend/mail.mjs`.
//
// Amail wears the same tema as `laklok` — one saved choice, one census — so
// the two rooms read as one house. Its QR sits on pale blue paper where
// laklok's sits on white, so the corners tell the rooms apart.

import {
  LAK_THEMES,
  realtimeTick,
  pickTema,
  restoreTema,
  saveTema,
  reportTema,
  makeQR,
  paintQR,
  paintGear,
  GEAR,
  paintEnvelope,
  ENVELOPE,
  paintBadge,
  badgeWidth,
  paintTemaPane,
  temaRow,
} from "./common/laklok-tema.mjs";

let view = "inbox"; // inbox · sent · prefs · compose
let status = "loading"; // loading, loaded, error, noauth
let mail = null;
let prefs = null; // blast subscription + history, fetched when prefs opens
let errorMsg = null;
let inboxBtn, sentBtn, prefsBtn, readBtn, writeBtn, subBtn, unsubBtn;
let rows = []; // [{ y0, y1, who }] — paint measures them, act replies to them
let ellipsisTicker;
let busy = false;
let fields; // to · subject · body, sharing one keyboard (see lib/type.mjs)
let sendBtn;
let composeNote = null; // what went wrong with the last send, if anything
let pendingTo = null; // an address that arrived in the URL, waiting for the box

// 👗 The tema, shared with laklok via the same store key.
let tema = "ler";
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

// The current palette, read fresh each frame so the `realtime` tema's minute
// drift shows up without anyone asking.
const T = () => LAK_THEMES[tema];

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

// 🥾 Boot
async function boot(api) {
  const { user, gizmo, hud, net, ui, screen, store, colon, params } = api;
  hud.label("amail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  settingsOpen = false;
  view = "inbox";
  composeNote = null;

  // 👗 A colon/`~` token pins a tema (`amail~skov`); otherwise the saved one.
  const tokens = [...(colon || []), ...(params || [])];
  const picked = pickTema(tokens, store);
  wearTema(picked.name);
  restoreTema(store, picked.pinned, (saved) => {
    if (saved) wearTema(saved);
    if (user) reportTema(net, tema); // after the saved tema has had its say
  });

  qrCells = makeQR("https://aesthetic.computer/amail");

  inboxBtn = new ui.TextButton("inbox", { screen });
  sentBtn = new ui.TextButton("sent", { screen });
  prefsBtn = new ui.TextButton("prefs", { screen });
  readBtn = new ui.TextButton("mark read", { screen });
  writeBtn = new ui.TextButton("write", { screen });
  sendBtn = new ui.TextButton("send", { screen });
  subBtn = new ui.TextButton("subscribe", { screen });
  unsubBtn = new ui.TextButton("unsubscribe", { screen });

  // The compose field takes exactly what the prompt command takes, so there's
  // one grammar to learn: `@handle your message`.
  fields = new ui.TextFields(
    api,
    [
      { name: "to", label: "to", placeholder: "@handle or ac25namuc" },
      { name: "subject", label: "re", placeholder: "(optional)" },
      { name: "body", label: "say", lines: 4, placeholder: "…" },
    ],
    (letter) => send(api, letter),
    { scheme: fieldScheme() },
  );

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
  const text = (body || "").trim();
  if (!to?.trim()) {
    composeNote = "who is it to?";
    return;
  }
  if (!text) {
    composeNote = "nothing to say yet";
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
    composeNote = `no one answers to ${to.trim()}`;
  } else {
    composeNote = "couldn't send that";
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
  if (to) fields.values.to = to;
  fields.focus(to ? 1 : 0, api);
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

const DIM = [[40, 40, 46], [80, 80, 90], [120, 120, 130]];

// Each tab keeps its own hue so you can tell them apart at a glance, and the
// open one is the only one that fills. TextButton takes colors, not ink.
const TABS = {
  inbox: [60, 130, 210],
  sent: [210, 140, 50],
  prefs: [150, 100, 200],
};

function tab(name) {
  const [r, g, b] = TABS[name];
  const ground = T().stripeA;
  return view === name
    ? [[r >> 1, g >> 1, b >> 1], [r, g, b], [255, 255, 255]]
    : [ground, [r >> 2, g >> 2, b >> 2], [r * 0.55, g * 0.55, b * 0.55]];
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

// ⚙️ The indstillinger pane — just the tema row here; mode and filter are
// the chat's business.
function paintSettings(api) {
  settingsHits = [];
  if (!settingsOpen) return;
  const { screen } = api;
  const qrBoxSize = (qrCells?.length || 0) + 2;
  settingsHits = paintTemaPane(api, {
    theme: T(),
    rows: [temaRow(tema)],
    right: screen.width - 3,
    top: 3 + qrBoxSize + 4,
  });
}

// 🎨 Paint
function paint(api) {
  const { wipe, ink, screen, text, help } = api;
  const t = T();
  const c = t.chat;
  wipe(...t.bg);

  if (status === "loading") {
    ink(c.timestamp).write(
      "loading" + ellipsisTicker.text(help.repeat, { pad: false }),
      { center: "xy", size: 1 },
    );
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (status === "noauth") {
    ink(c.messageText).write("log in for amail", { center: "xy" });
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (status === "error") {
    ink(200, 100, 100).write("error: " + (errorMsg || "unknown"), {
      center: "xy",
    });
    paintCorner(api);
    paintSettings(api);
    return;
  }

  const x = 6;
  const wide = screen.width - x * 2;
  let y = 6; // the hud label is already the title — don't write a second one
  y += 16; // the corner cluster owns the first row

  // The two spellings of this mailbox. The permahandle never moves and reads
  // like a serial number, so it wears MatrixChunky8; the @handle is the human
  // alias and stays in the normal face.
  for (const address of mail.addresses) {
    if (PERMA.test(address)) {
      ink(c.log).write(
        address,
        { x, y: y + 1 },
        undefined,
        undefined,
        false,
        "MatrixChunky8",
      );
      y += 9;
    } else {
      ink(c.handle).write(address, { x, y });
      y += 11;
    }
  }
  y += 5;

  inboxBtn.reposition({ x, y, screen });
  inboxBtn.paint(api, tab("inbox"));
  sentBtn.reposition({ x: x + 48, y, screen });
  sentBtn.paint(api, tab("sent"));
  prefsBtn.reposition({ x: x + 88, y, screen });
  prefsBtn.paint(api, tab("prefs"));
  y += 18;

  writeBtn.reposition({ x, y, screen });
  writeBtn.paint(api, [[28, 54, 42], [110, 210, 155], [205, 255, 225]]);
  ink(c.timestamp).write(
    "or from the prompt:  amail @handle your message",
    { x: x + 44, y: y + 6 },
    undefined,
    undefined,
    false,
    "MatrixChunky8",
  );
  y += 18;

  ink(c.lines).box(x, y, wide, 1);
  y += 8;

  // Compose sits in the room instead of replacing it — the addresses and tabs
  // stay put and the field takes the space the letters were using.
  if (view === "compose") {
    const frame = {
      x,
      y,
      width: wide,
      height: Math.min(110, Math.max(64, screen.height - y - 34)),
    };
    fields.paint(api, frame);

    const footer = frame.y + frame.height + 4;
    sendBtn.reposition({ x, y: footer, screen });
    sendBtn.paint(api, [[28, 54, 42], [110, 210, 155], [205, 255, 225]]);
    ink(composeNote ? [255, 130, 130] : c.timestamp).write(
      composeNote || "enter moves down  ·  tap a row to jump",
      { x: x + 40, y: footer + 6 },
      undefined,
      wide - 40,
      false,
      "MatrixChunky8",
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
    ink(c.timestamp).write(
      view === "inbox" ? "no amail yet" : "nothing sent yet",
      { x, y },
    );
    if (view === "inbox") {
      ink([...c.timestamp, 160]).write("try: amail @jeffrey hello", { x, y: y + 12 });
    }
    paintCorner(api);
    paintSettings(api);
    return;
  }

  if (view === "inbox" && mail.unread > 0) {
    readBtn.reposition({ x, y, screen });
    readBtn.paint(api, busy ? DIM : [[30, 60, 60], [100, 200, 200], [190, 255, 255]]);
    y += 18;
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
    ink(unread ? c.handle : c.timestamp).write(who, {
      x: x + 8,
      y,
    });
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

  if (!prefs) {
    ink(c.timestamp).write("loading prefs…", { x, y });
    return;
  }

  if (prefs.email) {
    ink(c.timestamp).write(prefs.email, { x, y });
    y += 12;

    if (prefs.subscribed) {
      ink(120, 200, 120).write("subscribed", { x, y });
      unsubBtn.reposition({ x: x + 80, y: y - 2, screen });
      unsubBtn.paint(api, [[60, 26, 26], [200, 110, 110], [255, 190, 190]]);
    } else {
      ink(200, 120, 80).write("unsubscribed", { x, y });
      subBtn.reposition({ x: x + 96, y: y - 2, screen });
      subBtn.paint(api, [[26, 60, 30], [110, 200, 120], [190, 255, 200]]);
    }
    y += 16;
  }

  if (busy) {
    ink(c.timestamp).write("updating…", { x, y });
    y += 14;
  }

  ink(c.kidlisp).write("blast history", { x, y });
  y += 14;

  if (!prefs.blasts || prefs.blasts.length === 0) {
    ink(c.timestamp).write("no blasts sent yet", { x, y });
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
        }
      } else if (!hit(settingsHits.pane)) {
        settingsOpen = false; // Tap outside closes.
      }
      needsPaint();
    }
    return;
  }

  if (status !== "loaded") return;

  if (view === "compose") {
    // The tabs stay live while composing — they're painted, so they have to
    // work. Otherwise the only way out is a key, and a phone has no escape.
    let left = false;
    const leave = (to) => {
      leaveCompose(api, to);
      left = true;
    };
    inboxBtn?.act(e, () => leave("inbox"));
    sentBtn?.act(e, () => leave("sent"));
    prefsBtn?.act(e, () => leave("prefs"));
    if (left) {
      needsPaint();
      return;
    }

    if (e.is("keyboard:down:escape")) {
      leaveCompose(api, "inbox");
      needsPaint();
      return;
    }

    let sending = false;
    sendBtn?.act(e, () => {
      sending = true;
      fields.sync();
      send(api, { ...fields.values });
    });
    if (sending) {
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

  inboxBtn?.act(e, () => {
    view = "inbox";
    needsPaint();
  });

  sentBtn?.act(e, () => {
    view = "sent";
    needsPaint();
  });

  writeBtn?.act(e, () => {
    compose(api);
    needsPaint();
  });

  prefsBtn?.act(e, async () => {
    view = "prefs";
    needsPaint();
    if (prefs) return;
    const res = await net.userRequest("GET", "/api/mail-status");
    if (res.status === 200) prefs = res;
    needsPaint();
  });

  // Tap a letter to answer it — the field opens already addressed.
  if (e.is("touch") && (view === "inbox" || view === "sent")) {
    const row = rows.find((r) => e.y >= r.y0 && e.y < r.y1);
    if (row?.who?.startsWith("@")) {
      compose(api, row.who);
      needsPaint();
      return;
    }
  }

  if (busy) return;

  // Only a button that's actually on screen may act — otherwise it keeps the
  // box it had in another view and catches clicks meant for something else.
  if (view === "inbox" && mail.unread > 0) {
    readBtn?.act(e, async () => {
      busy = true;
      needsPaint();
      const res = await net.userRequest("POST", "/api/mail", { action: "read" });
      if (res.status === 200) {
        mail.unread = 0;
        mail.inbox.forEach((letter) => (letter.read = true));
      }
      busy = false;
      needsPaint();
    });
  }

  if (view !== "prefs" || !prefs) return;

  const toggle = async (action, subscribed) => {
    busy = true;
    needsPaint();
    const res = await net.userRequest("POST", "/api/mail-status", { action });
    if (res.status === 200) prefs.subscribed = subscribed;
    busy = false;
    needsPaint();
  };

  if (prefs.subscribed) {
    unsubBtn?.act(e, () => toggle("unsubscribe", false));
  } else {
    subBtn?.act(e, () => toggle("subscribe", true));
  }
}

export { meta, boot, sim, paint, act };
