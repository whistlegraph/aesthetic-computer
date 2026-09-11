// Mail, 2026.2.12 → 2026.9.11 (inbox added)
// AC mail. `mail @handle words...` sends from the prompt; `mail` lands here.
// Tier 1: nothing leaves aesthetic.computer. See `system/backend/mail.mjs`.

let view = "inbox"; // inbox · sent · prefs
let status = "loading"; // loading, loaded, error, noauth
let mail = null;
let prefs = null; // blast subscription + history, fetched when prefs opens
let errorMsg = null;
let inboxBtn, sentBtn, prefsBtn, readBtn, subBtn, unsubBtn;
let ellipsisTicker;
let busy = false;

function meta() {
  return {
    title: "Mail — aesthetic.computer",
    desc: "Your aesthetic.computer inbox.",
  };
}

// 🥾 Boot
async function boot({ user, gizmo, hud, net, ui, screen }) {
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  inboxBtn = new ui.TextButton("inbox", { screen });
  sentBtn = new ui.TextButton("sent", { screen });
  prefsBtn = new ui.TextButton("prefs", { screen });
  readBtn = new ui.TextButton("mark read", { screen });
  subBtn = new ui.TextButton("subscribe", { screen });
  unsubBtn = new ui.TextButton("unsubscribe", { screen });

  if (!user) {
    status = "noauth";
    return;
  }

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

// 🧮 Sim
function sim({ clock }) {
  ellipsisTicker?.update(clock.time());
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

  const x = 6;
  const wide = screen.width - x * 2;
  let y = 8;

  ink(200, 180, 255).write("mail", { x, y, size: 2 });
  if (mail.unread > 0) {
    ink(0, 255, 255).write(`${mail.unread} new`, { x: x + 46, y: y + 4 });
  }
  y += 20;

  // The two spellings of this mailbox — permahandle first, it never moves.
  for (const address of mail.addresses) {
    ink(address.startsWith("ac") ? 110 : 90).write(address, { x, y });
    y += 10;
  }
  y += 4;

  inboxBtn.reposition({ x, y, screen });
  ink(view === "inbox" ? [70, 60, 110] : [44, 40, 54]);
  inboxBtn.paint(api);
  sentBtn.reposition({ x: x + 48, y, screen });
  ink(view === "sent" ? [70, 60, 110] : [44, 40, 54]);
  sentBtn.paint(api);
  prefsBtn.reposition({ x: x + 88, y, screen });
  ink(view === "prefs" ? [70, 60, 110] : [44, 40, 54]);
  prefsBtn.paint(api);
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
    ink(busy ? [60, 60, 60] : [40, 70, 70]);
    readBtn.paint(api);
    y += 18;
  }

  for (const letter of letters) {
    if (y > screen.height - 14) break;
    const unread = view === "inbox" && !letter.read;
    const who = (view === "inbox" ? letter.from : letter.to) || "someone";

    if (unread) ink(0, 255, 255).box(x, y + 2, 3, 3);
    ink(unread ? [170, 220, 255] : [130, 140, 170]).write(who, {
      x: x + 6,
      y,
    });
    ink(70).write(ago(letter.when), { x: screen.width - 34, y });
    y += 11;

    const bounds = wide - 10;
    ink(unread ? 245 : 190).write(letter.text, { x: x + 8, y }, undefined, bounds);
    y += text.box(letter.text, { x: x + 8, y }, bounds).box.height + 6;
  }
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
      ink(120, 80, 80);
      unsubBtn.paint(api);
    } else {
      ink(200, 120, 80).write("unsubscribed", { x, y });
      subBtn.reposition({ x: x + 96, y: y - 2, screen });
      ink(80, 160, 80);
      subBtn.paint(api);
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
function act({ event: e, net, needsPaint }) {
  if (status !== "loaded") return;

  inboxBtn?.act(e, () => {
    view = "inbox";
    needsPaint();
  });

  sentBtn?.act(e, () => {
    view = "sent";
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

  if (busy) return;

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
