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

async function boot({ api, wipe, handle, ui, net: { preload }, user }) {
  const han = handle() || user?.email;

  if (han !== undefined) {
    hasAccount = true;
    const name = "startup";
    sfx = await preload(name);
    btn = new ui.TextButton(`Delete ${han}`);
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
    btn.reposition({ center: "xy", screen });
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

function act({ event: e, sound, gizmo, net, notice }) {
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
        notice("ACCOUNT DELETED", ["white", "red"]);
        setTimeout(() => {
          net.logout();
        }, 1000);
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