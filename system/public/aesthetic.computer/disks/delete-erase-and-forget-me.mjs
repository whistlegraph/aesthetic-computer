// Delete-erase-and-forget-me, 2023.12.15.13.08.49.336
// Delete your aesthetic computer account.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Delete chat messages / set as deleted if a user deletes their account.
  + Done
  - [x] Wire button up w/ a POST request to `api/delete-erase-and-forget-me`.
  - [x] Support account deletion through multiple taps of a button.
#endregion */

let sfx,
  btn,
  times = 3;
let ellipsisTicker;
let hasAccount;
let problem;
let lockedUntil; // The day the server will delete the account.
let mailed; // Whether the server emailed a link to keep it.
let summary; // What deletion removes and keeps, from ?preview.
let exportBtn; // Downloads a copy of the account before deleting it.
let exporting = false;

const plural = (n, word) => `${n.toLocaleString()} ${word}${n === 1 ? "" : "s"}`;

// Turns the server's preview into a few short lines.
function describe(p) {
  const c = p.counts || {};
  const gone = [
    c.paintings && plural(c.paintings, "painting"),
    c.pieces && plural(c.pieces, "piece"),
    c.tapes && plural(c.tapes, "tape"),
    c.moods && plural(c.moods, "mood"),
    c.news && plural(c.news, "news post"),
    c.chat && plural(c.chat, "chat message"),
    c.kidlispDeleted && `${c.kidlispDeleted.toLocaleString()} KidLisp`,
  ].filter(Boolean);
  const lines = [`Deletes ${gone.length ? gone.join(", ") : "your account"}.`];
  if (c.kidlispKept) {
    lines.push(`Keeps ${c.kidlispKept.toLocaleString()} KidLisp without your name, because it is minted or used by others.`);
  }
  if (p.braincells) lines.push(`Loses ${p.braincells.toLocaleString()} braincells.`);
  lines.push(
    p.handleGoesToSotce
      ? `Your handle stays with your Sotce Net account.`
      : `Nobody can take your handle for ${p.handleHoldDays} days.`,
  );
  lines.push(
    `Locks now and deletes after ${p.graceDays} days.` +
      (p.email ? ` A link to keep it goes to ${p.email}.` : ""),
  );
  return lines.join("\n");
}

async function boot({ api, wipe, handle, ui, net, user, screen }) {
  const han = handle() || user?.email;

  if (han !== undefined) {
    hasAccount = true;
    const name = "startup";
    sfx = await net.preload(name);
    btn = new ui.TextButton(`Delete ${han}`);
    exportBtn = new ui.TextButton("Download my data", { center: "x", bottom: 12, screen });
    const preview = await net.userRequest("GET", "/api/delete-erase-and-forget-me?preview");
    if (preview.status === 200) summary = describe(preview);
  }
}

function paint({ ink, wipe, screen, write, help }) {
  if (problem) {
    wipe("maroon");
    ink("pink");
    write(
      `Oops, something went wrong!`,
      { center: "xy" },
      "black",
      screen.width / 1.25,
    );
  } else if (!hasAccount) {
    wipe("maroon");
    ink("white");
    write(`No account found.`, { center: "xy" }, "black", screen.width / 1.25);
  } else if (times > 0) {
    wipe("maroon");
    if (summary) {
      ink("pink");
      write(summary, { x: 8, y: 26 }, "black", screen.width - 16);
    }
    btn.reposition({ center: "xy", screen });
    exportBtn?.reposition({ center: "x", bottom: 12, screen }, exporting ? "Downloading..." : "Download my data");
    exportBtn?.paint({ ink });
    btn.paint({ ink });
    ink("white");
    let text;

    if (times === 1) {
      text = `Push 1 more time to delete your account!`;
    } else {
      text = `Push ${times} more times to delete your account.`;
    }
    write(
      text,
      { center: "x", y: screen.height / 2 + 20 },
      "red",
      screen.width / 2,
    );
  } else if (lockedUntil) {
    wipe("red");
    write(
      `Your account is locked and will be deleted on ${lockedUntil}.` +
        (mailed ? `\n\nWe emailed you a link to keep it.` : ""),
      { center: "xy" },
      "red",
      screen.width / 1.25,
    );
  } else {
    wipe("red");
    write(
      `Your account is being deleted!\n\n Please wait${ellipsisTicker.text(
        help.repeat,
      )}`,
      { center: "xy" },
      "red",
      screen.width / 1.25,
    );
  }
}

function act({ event: e, sound, gizmo, net, notice, download }) {
  if (times > 0) {
    exportBtn?.act(e, async () => {
      if (exporting) return;
      exporting = true;
      const { status, ...copy } = await net.userRequest("GET", "/api/delete-erase-and-forget-me?export");
      exporting = false;
      if (status === 200) {
        download("aesthetic-computer-export.json", JSON.stringify(copy, null, 2));
      } else {
        notice("EXPORT FAILED", ["white", "red"]);
      }
    });
  }
  btn?.act(e, async () => {
    sound.play(sfx);
    times -= 1;
    if (times === 0) {
      ellipsisTicker = new gizmo.EllipsisTicker();
      const res = await net.userRequest(
        "POST",
        "/api/delete-erase-and-forget-me",
      );
      console.log("Account deletion response:", res);
      if (res.status === 200) {
        const when = res.purgeAfter ? new Date(res.purgeAfter) : null;
        lockedUntil = when && !isNaN(when)
          ? when.toLocaleDateString(undefined, { year: "numeric", month: "long", day: "numeric" })
          : "in 14 days";
        mailed = res.mailed === true;
        notice("ACCOUNT LOCKED", ["white", "red"]);
        setTimeout(() => {
          net.logout();
        }, 6000);
      } else {
        problem = true;
      }
    }
  });
}

function sim() {
  ellipsisTicker?.sim();
}

function meta() {
  return {
    title: "Delete-erase-and-forget-me",
    desc: "Delete your aesthetic computer account.",
  };
}

export { boot, paint, act, sim, meta };