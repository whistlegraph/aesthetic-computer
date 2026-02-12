// Mail, 2026.2.12
// Email preferences and blast history for aesthetic.computer.

let status = "loading"; // loading, loaded, error, noauth
let data = null;
let errorMsg = null;
let subBtn, unsubBtn;
let ellipsisTicker;
let toggling = false;

// 📰 Meta
function meta() {
  return {
    title: "Mail — aesthetic.computer",
    desc: "Email preferences and blast history.",
  };
}

// 🥾 Boot
async function boot({ user, gizmo, hud, net, ui, screen }) {
  hud.label("mail");
  ellipsisTicker = new gizmo.EllipsisTicker();

  if (!user) {
    status = "noauth";
    return;
  }

  try {
    const res = await net.userRequest("GET", "/api/mail-status");
    if (res.status === 200) {
      data = await res.json();
      status = "loaded";
    } else {
      status = "error";
      errorMsg = `${res.status}`;
    }
  } catch (err) {
    status = "error";
    errorMsg = err.message;
  }

  subBtn = new ui.TextButton("subscribe", { screen });
  unsubBtn = new ui.TextButton("unsubscribe", { screen });
}

// 🧮 Sim
function sim({ clock }) {
  ellipsisTicker?.update(clock.time());
}

// 🎨 Paint
function paint(api) {
  const { wipe, ink, screen, help } = api;
  wipe(24, 20, 28);

  if (status === "loading") {
    ink(180)
      .write(
        "loading" + ellipsisTicker.text(help.repeat, { pad: false }),
        { center: "xy", size: 1 },
      );
    return;
  }

  if (status === "noauth") {
    ink(180).write("log in to manage email preferences", { center: "xy" });
    return;
  }

  if (status === "error") {
    ink(200, 100, 100).write("error: " + (errorMsg || "unknown"), {
      center: "xy",
    });
    return;
  }

  const leftX = 6;
  let y = 8;
  const lineH = 10;

  // Header
  ink(200, 180, 255).write("mail", { x: leftX, y, size: 2 });
  y += 18;

  // Subscription status
  if (data.email) {
    ink(120).write(data.email, { x: leftX, y });
    y += lineH + 2;

    if (data.subscribed) {
      ink(120, 200, 120).write("subscribed", { x: leftX, y });
      // Show unsubscribe button
      unsubBtn.reposition({ x: leftX + 80, y: y - 2, screen });
      ink(120, 80, 80);
      unsubBtn.paint(api);
    } else {
      ink(200, 120, 80).write("unsubscribed", { x: leftX, y });
      // Show subscribe button
      subBtn.reposition({ x: leftX + 96, y: y - 2, screen });
      ink(80, 160, 80);
      subBtn.paint(api);
    }
    y += lineH + 4;
  }

  if (toggling) {
    ink(160).write("updating...", { x: leftX, y });
    y += lineH + 4;
  }

  // Divider
  ink(60).box(leftX, y, screen.width - leftX * 2, 1);
  y += 8;

  // Blast history
  ink(160, 140, 200).write("blast history", { x: leftX, y });
  y += lineH + 4;

  if (!data.blasts || data.blasts.length === 0) {
    ink(100).write("no blasts sent yet", { x: leftX, y });
    return;
  }

  ink(80).write(
    `${data.count} blast${data.count !== 1 ? "s" : ""} · ${data.totalSent} emails sent · ${data.totalUnsubscribed} unsub`,
    { x: leftX, y },
  );
  y += lineH + 4;

  for (const blast of data.blasts) {
    if (y > screen.height - 16) break;

    const date = new Date(blast.when).toLocaleDateString("en-US", {
      month: "short",
      day: "numeric",
      year: "numeric",
    });

    const statusColor =
      blast.status === "completed"
        ? [80, 180, 80]
        : blast.status === "in-progress"
          ? [200, 180, 80]
          : [180, 80, 80];

    ink(...statusColor).write(blast.status, { x: leftX, y });
    ink(140).write(date, { x: leftX + 80, y });
    ink(100).write(
      `${blast.sent}/${blast.totalAttempted} sent`,
      { x: leftX + 150, y },
    );
    y += lineH;

    // Subject line
    const subj =
      blast.subject?.length > 50
        ? blast.subject.slice(0, 47) + "..."
        : blast.subject || "(no subject)";
    ink(80).write(subj, { x: leftX + 8, y });
    y += lineH + 4;
  }
}

// 🎪 Act
function act({ event: e, net, needsPaint }) {
  if (status !== "loaded" || toggling) return;

  if (data.subscribed) {
    unsubBtn?.act(e, async () => {
      toggling = true;
      needsPaint();
      try {
        const res = await net.userRequest("POST", "/api/mail-status", {
          action: "unsubscribe",
        });
        if (res.status === 200) {
          data.subscribed = false;
        }
      } catch (err) {
        console.error("Unsubscribe failed:", err);
      }
      toggling = false;
      needsPaint();
    });
  } else {
    subBtn?.act(e, async () => {
      toggling = true;
      needsPaint();
      try {
        const res = await net.userRequest("POST", "/api/mail-status", {
          action: "subscribe",
        });
        if (res.status === 200) {
          data.subscribed = true;
        }
      } catch (err) {
        console.error("Subscribe failed:", err);
      }
      toggling = false;
      needsPaint();
    });
  }
}

export { meta, boot, sim, paint, act };
