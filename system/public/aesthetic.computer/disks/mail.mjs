// Mail, 2026.2.12 → 2026.9.11 (inbox added)
// AC mail. `mail @handle words...` sends from the prompt; `mail` lands here.
// Tier 1: nothing leaves aesthetic.computer. See `system/backend/mail.mjs`.

let view = "inbox"; // inbox · sent · prefs · compose
let status = "loading"; // loading, loaded, error, noauth
let mail = null;
let prefs = null; // blast subscription + history, fetched when prefs opens
let errorMsg = null;
let inboxBtn, sentBtn, prefsBtn, readBtn, writeBtn, subBtn, unsubBtn;
let rows = []; // [{ y0, y1, who }] — paint measures them, act replies to them
let ellipsisTicker;
let busy = false;
let input; // the compose field — one line, same grammar as the prompt command
let composeNote = null; // what went wrong with the last send, if anything
let pendingTo = null; // an address waiting for the field to be ready for it

function meta() {
  return {
    title: "Mail — aesthetic.computer",
    desc: "Your aesthetic.computer inbox.",
  };
}

// 🥾 Boot
async function boot(api) {
  const { user, gizmo, hud, net, ui, screen } = api;
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  inboxBtn = new ui.TextButton("inbox", { screen });
  sentBtn = new ui.TextButton("sent", { screen });
  prefsBtn = new ui.TextButton("prefs", { screen });
  readBtn = new ui.TextButton("mark read", { screen });
  writeBtn = new ui.TextButton("write", { screen });
  subBtn = new ui.TextButton("subscribe", { screen });
  unsubBtn = new ui.TextButton("unsubscribe", { screen });

  // The compose field takes exactly what the prompt command takes, so there's
  // one grammar to learn: `@handle your message`.
  input = new ui.TextInput(api, "@handle your message", (text) => send(api, text));

  if (!user) {
    status = "noauth";
    return;
  }

  await refresh(api);
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

// Post whatever's in the compose field, then fall back to the inbox.
async function send(api, raw) {
  const body = raw.trim();
  input.text = "";
  input.showBlink = false;
  input.mute = true;
  api.send({ type: "keyboard:text:replace", content: { text: "" } });
  api.send({ type: "keyboard:close" });

  const gap = body.indexOf(" ");
  if (gap < 1) {
    composeNote = "who? try: @handle your message";
    return;
  }

  const to = body.slice(0, gap);
  const text = body.slice(gap + 1).trim();
  if (!text) {
    composeNote = "nothing to say yet";
    return;
  }

  const res = await api.net.userRequest("POST", "/api/mail", { to, text });
  if (res.status === 200) {
    composeNote = null;
    view = "sent";
    await refresh(api);
  } else if (res.status === 404) {
    composeNote = `no one answers to ${to}`;
  } else {
    composeNote = "couldn't send that";
  }
}

// Open compose, optionally already addressed to someone. Opening the keyboard
// resyncs the field from an empty textarea, so the address has to wait for it
// (see sim) instead of being written here.
function compose(api, to) {
  view = "compose";
  composeNote = null;
  input.mute = false;
  pendingTo = to ? `${to} ` : null;
  api.send({ type: "keyboard:open" });
}

// 🧮 Sim
function sim(api) {
  ellipsisTicker?.update(api.clock.time());
  if (view !== "compose") return;

  if (pendingTo && input.canType) {
    input.text = pendingTo;
    api.send({ type: "keyboard:text:replace", content: { text: pendingTo } });
    pendingTo = null;
  }

  input.sim(api);
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
  return view === name
    ? [[r >> 1, g >> 1, b >> 1], [r, g, b], [255, 255, 255]]
    : [[20, 19, 24], [r >> 2, g >> 2, b >> 2], [r * 0.55, g * 0.55, b * 0.55]];
}

const PERMA = /^ac\d\d[a-z]{5}@/;

// 📬 The corner envelope — shut and grey when the box is empty, lit with its
// flap open when something is waiting.
function envelope(api, x, y, unread) {
  const { ink } = api;
  const w = 15;
  const h = 10;
  ink(unread ? [0, 120, 140] : [46, 44, 56]).box(x, y, w, h);
  ink(unread ? [120, 255, 255] : [96, 92, 114]).box(x, y, w, h, "outline");
  ink(unread ? [190, 255, 255] : [70, 68, 84]);
  api.line(x, y, x + (w >> 1), y + (h >> 1));
  api.line(x + w - 1, y, x + (w >> 1), y + (h >> 1));
}

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

// 🎨 Paint
function paint(api) {
  const { wipe, ink, screen, text, help } = api;
  wipe(24, 20, 28);

  if (status === "loading") {
    ink(180).write(
      "loading" + ellipsisTicker.text(help.repeat, { pad: false }),
      { center: "xy", size: 1 },
    );
    return;
  }

  if (status === "noauth") {
    ink(180).write("log in for mail", { center: "xy" });
    return;
  }

  if (status === "error") {
    ink(200, 100, 100).write("error: " + (errorMsg || "unknown"), {
      center: "xy",
    });
    return;
  }

  if (view === "compose") {
    input.paint(api);
    if (composeNote) {
      ink(255, 130, 130).write(composeNote, { x: 6, y: screen.height - 12 });
    }
    return;
  }

  const x = 6;
  const wide = screen.width - x * 2;
  let y = 6; // the hud label is already the title — don't write a second one

  const envX = screen.width - 21;
  envelope(api, envX, y - 2, mail.unread);
  if (mail.unread > 0) {
    const count = `${mail.unread}`;
    ink(0, 255, 255).write(count, { x: envX - 4 - count.length * 6, y });
  }
  y += 16;

  // The two spellings of this mailbox. The permahandle never moves and reads
  // like a serial number, so it wears MatrixChunky8; the @handle is the human
  // alias and stays in the normal face.
  for (const address of mail.addresses) {
    if (PERMA.test(address)) {
      ink(110, 200, 165).write(
        address,
        { x, y: y + 1 },
        undefined,
        undefined,
        false,
        "MatrixChunky8",
      );
      y += 9;
    } else {
      ink(150, 170, 225).write(address, { x, y });
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
  ink(96, 104, 118).write(
    "or from the prompt:  mail @handle your message",
    { x: x + 44, y: y + 6 },
    undefined,
    undefined,
    false,
    "MatrixChunky8",
  );
  y += 18;

  ink(60).box(x, y, wide, 1);
  y += 8;

  if (view === "prefs") {
    paintPrefs(api, x, y, wide);
    return;
  }

  const letters = view === "inbox" ? mail.inbox : mail.sent;

  if (letters.length === 0) {
    ink(100).write(
      view === "inbox" ? "no mail yet" : "nothing sent yet",
      { x, y },
    );
    if (view === "inbox") {
      ink(70).write("try: mail @jeffrey hello", { x, y: y + 12 });
    }
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

    // Stripe the row behind everything, so a long message stays one block.
    ink(unread ? [30, 42, 56] : i % 2 ? [26, 23, 32] : [33, 29, 40]).box(
      x,
      y - 3,
      wide,
      body + 17,
    );

    rows.push({ y0: y - 3, y1: y + body + 14, who });

    if (unread) ink(0, 255, 255).box(x + 2, y + 2, 3, 3);
    ink(unread ? [170, 220, 255] : [130, 140, 170]).write(who, {
      x: x + 8,
      y,
    });
    ink(70).write(ago(letter.when), { x: screen.width - 34, y });
    y += 11;

    ink(unread ? 245 : 190).write(letter.text, { x: x + 10, y }, undefined, bounds);
    y += body + 6;
  });
}

function paintPrefs(api, x, y, wide) {
  const { ink, screen } = api;

  if (!prefs) {
    ink(140).write("loading prefs…", { x, y });
    return;
  }

  if (prefs.email) {
    ink(120).write(prefs.email, { x, y });
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
    ink(160).write("updating…", { x, y });
    y += 14;
  }

  ink(160, 140, 200).write("blast history", { x, y });
  y += 14;

  if (!prefs.blasts || prefs.blasts.length === 0) {
    ink(100).write("no blasts sent yet", { x, y });
    return;
  }

  ink(80).write(
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
    ink(140).write(ago(blast.when), { x: x + 80, y });
    ink(100).write(`${blast.sent}/${blast.totalAttempted}`, { x: x + 130, y });
    y += 10;
    ink(80).write(blast.subject || "(no subject)", { x: x + 8, y }, undefined, wide - 10);
    y += 14;
  }
}

// 🎪 Act
function act(api) {
  const { event: e, net, needsPaint } = api;
  if (status !== "loaded") return;

  if (view === "compose") {
    if (e.is("keyboard:down:escape")) {
      view = "inbox";
      composeNote = null;
      pendingTo = null;
      api.send({ type: "keyboard:close" });
      needsPaint();
      return;
    }
    input.act(api);
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
