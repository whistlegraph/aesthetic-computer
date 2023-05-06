// A text based "access-everything" console.

// Currently the aesthetic.computer home piece!

/* #region 🏁 todo
 - [] Prevent non-printable characters from causing an empty space.
 - [] Generate or pretty print docs (made from the APIs) inside this disk.
      (This would allow people to have a reference while writing disks.)
 + Done
 - [x] Generically "lock" prompt after input before a result returns.
      Show a spinner if too much time has passed?
 + Later
 - [] An iOS app would need a small ESC or arrow overlay button in Swift
      to make this work properly.
#endregion */

import { parse } from "../lib/parse.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { Desktop, MetaBrowser } from "../lib/platform.mjs";
import { TextInput } from "../lib/type.mjs";
const { abs } = Math;

const scheme = {
  dark: {
    fg: [255, 100],
    bg: [70, 50, 100],
    block: [200, 30, 100],
    line: [0, 0, 255, 64],
  },
  light: {
    fg: [0, 200],
    bg: [170, 150, 200],
    block: [30, 200, 200],
    line: [0, 0, 0, 128],
  },
};

let input, motd;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashMessage;
let flashColor = [];
let flashPresent = false;
let uploadProgress = 0; // If not zero, then draw a progress bar.

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const {
    api,
    handle,
    authorize,
    pieceCount,
    glaze,
    load,
    download,
    dark,
    darkMode,
    num,
    store,
    connect,
    bgm,
    needsPaint,
    system,
    screen,
    painting,
    net,
    jump,
    user,
    upload,
  } = $;

  glaze({ on: true }); // TODO: Every glaze triggers `frame` in `disk`, this could be optimized. 2022.04.24.04.25

  makeMotd($); // Generate welcome message.

  input = new TextInput(
    $,
    motd,
    scheme[dark ? "dark" : "light"],
    // 🍎 Process commands...
    async (text) => {
      // Roughly parse out the text (could also do a full `parse` here.)
      const tokens = text.split(" ");
      const slug = tokens[0]; // Note: Includes colon params.
      const params = tokens.slice(1);
      if (text.startsWith("handle")) {
        // Set username handle.
        // TODO: This could eventually be abstracted for more API calls.
        // Something like... await post({handle: "new"});

        // Make sure there is a parameter.
        const handle = text.split(" ")[1];

        // And a handle has been specified.
        if (handle?.length > 0 && validateHandle(handle)) {
          const token = await authorize(); // Get user token.
          if (token) {
            const headers = {
              Authorization: `Bearer ${token}`,
              "Content-Type": "application/json",
            };
            // 😀 Try to set the handle.
            // TODO: Lock the input prompt / put a spinner here...
            try {
              const response = await fetch("/handle", {
                method: "POST",
                headers: headers,
                body: JSON.stringify({ handle }),
              });
              const data = await response.json();
              if (response.status !== 200) {
                flashColor = [255, 0, 0];
                console.error("🧖 Error:", response, data);
              } else {
                flashColor = [0, 128, 0];
                store["handle:updated"] = data.handle;
                console.log("🧖 Handle changed:", data.handle);
                makeFlash($, true, "hi @" + data.handle);
              }
            } catch (error) {
              flashColor = [255, 0, 0]; // Server error.
              makeFlash($);
              console.error("🧖 Error:", error);
            }
          } else {
            flashColor = [255, 0, 0]; // Authorization error.
            makeFlash($);
            console.error("🧖 Not logged in.");
          }
          needsPaint();
        } else {
          flashColor = [255, 0, 0];
          console.warn("🧖 No @handle specified / bad handle design.");
          makeFlash($, true, "HANDLE INVALID");
        }
      } else if ((text === "ul" || text === "upload") && store["painting"]) {
        if (!navigator.onLine) {
          flashColor = [255, 0, 0];
          makeFlash($, true, "OFFLINE");
        } else {
          const filename = `painting-${num.timestamp()}.png`;
          // The first dashed string will get replaced with a slash / media directory filter on the server.
          uploadProgress = -1; // Trigger progress bar rendering.
          upload(filename, store["painting"], (p) => (uploadProgress = p))
            .then((data) => {
              console.log("🪄 Painting uploaded:", filename, data);
              flashColor = [0, 255, 0];
              makeFlash($);
              const slug = user
                ? `${handle || user.email}/painting/${data.slug}`
                : data.slug;
              jump(`download:painting ${slug}`);
            })
            .catch((err) => {
              console.error("🪄 Painting upload failed:", err);
              flashColor = [255, 0, 0];
              makeFlash($);
            });
        }
      } else if (slug === "resize" || slug === "res") {
        // Resize the active painting if one exists, or make one at this
        // size if it doesn't.
        const w = params[0],
          h = params[0] || w;
        if (isNaN(w)) {
          flashColor = [255, 0, 0];
        } else {
          nopaint_adjust(screen, system, painting, store, { w, h });
          flashColor = [0, 255, 0];
        }
        makeFlash($);
      } else if (text.startsWith("dl") || text.startsWith("download")) {
        if (store["painting"]) {
          download(`painting-${num.timestamp()}.png`, store["painting"], {
            scale: abs(parseInt(text.split(" ")[1])) || 6,
            // Read an integer parameter for scale.
            cropToScreen: !(store["painting:resolution-lock"] === true),
            // Only cut the download off at screen-size if user never
            // set a resolution.
          });
          // Show a green flash if we succesfully download the file.
          flashColor = [0, 255, 0];
        } else {
          flashColor = [255, 0, 0]; // Show a red flash otherwise.
        }
        makeFlash($);
      } else if (slug === "login" || slug === "hi") {
        net.login();
        flashColor = [255, 255, 0, 100]; // Yellow
        makeFlash($);
      } else if (text === "logout" || text === "bye") {
        net.logout();
        flashColor = [255, 255, 0, 100]; // Yellow
        makeFlash($);
      } else if (text === "no") {
        system.nopaint.no({ system, store, needsPaint });
        if (system.nopaint.undo.paintings.length > 1) {
          flashColor = [0, 0, 255, 100]; // Blue for succesful undo.
        } else {
          flashColor = [255, 0, 0, 100]; // Red for failed undo.
        }
        makeFlash($);
      } else if (text === "nopan") {
        system.nopaint.resetTransform(api);
        system.nopaint.storeTransform(store, system); // Store the translation after completion.
        input.text = "";
      } else if (text === "painting:reset" || text === "no!") {
        const deleted = system.nopaint.noBang({
          system,
          store,
          screen,
          needsPaint,
        });

        if (deleted) {
          flashColor = [0, 0, 255]; // Blue for succesful deletion.
        } else {
          flashColor = [255, 0, 0]; // Red if delete failed.
        }

        makeFlash($);
        needsPaint();
      } else if (text === "3dline:reset") {
        const deleted = await store.delete("3dline:drawing", "local:db");

        if (deleted) {
          flashColor = [0, 0, 255]; // Blue for succesful deletion.
        } else {
          flashColor = [255, 0, 0]; // Red if delete failed.
        }

        makeFlash($);
        needsPaint();
      } else if (text === "dark" || text === "dark:reset") {
        if (text === "dark:reset") {
          store.delete("dark-mode");
          darkMode("default");
          flashColor = [127, 127, 127]; // Gray for system setting.
        } else {
          let current = await store.retrieve("dark-mode");
          current = current === true ? false : true;
          darkMode(current);
          if (current) {
            flashColor = [0, 0, 0]; // Black for dark mode enabled.
          } else {
            flashColor = [255, 255, 255]; // White for dark mode disabled.
          }
        }
        makeFlash($);
      } else if (text.startsWith("2022")) {
        load(parse("wand~" + text)); // Execute the current command.
      } else if (text === "connect") {
        let identity;
        input.text = "";
        try {
          identity = await connect(); // Web3 connect.
          store["identity"] = identity; // Store the identity.
          store.persist("identity"); // ... and persist it!
          makeFlash($, false);
          flashColor = [0, 255, 0];
        } catch (e) {
          makeFlash($, false);
          flashColor = [255, 0, 0];
        }
      } else if (text === "bgm stop") {
        bgm.stop();
        makeFlash($, false);
        flashColor = [255, 0, 0];
      } else if (text === "help") {
        // Go to the Discord for now if anyone types help.
        jump("https://discord.gg/aesthetic-computer");
      } else if (text === "shillball" || text === "sb") {
        // Shortcuts for Yeche's Shillball game.
        jump("https://galerie-yechelange.baby/ball");
      } else if (text === "prod") {
        jump("https://prompt.ac"); // Visit the live site.
      } else if (text === "local" || text.startsWith("local")) {
        const param = text.replace("local", "").trim().replaceAll(" ", "~");
        const slug = param.length > 0 ? `/${param}` : "";
        console.log(slug);
        jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
      } else if (ordfish[text]) {
        debugger;
        jump("https://ordinals.com/content/" + ordfish[text]); // Jump to an official ordinal inscription.
      } else {
        // 🟠 Local and remote pieces...
        load(parse(text)); // Execute the current command.
      }
    }
  );

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (pieceCount > 0) {
    if (Desktop) input.canType = true;
    input.text = "";
  }
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($) {
  input?.sim($);

  if ($.store["handle:received"] && input.canType === false) {
    makeMotd($);
    input.text = motd;
    input.canType = false;
    delete $.store["handle:received"];
    $.needsPaint();
  }

  if (flashPresent) flash.step();
}

// 🎨 Paint (Runs once per display refresh rate)
function paint($) {
  let { screen, wipe, ink, history, api, system, store, dark } = $;

  const pal = scheme[dark ? "dark" : "light"];
  if (input) input.pal = pal; // Update text input palette.

  if (store["painting"]) {
    system.nopaint.present(api); // Render the painting.
    ink(...pal.bg, 127).box(screen); // Backdrop
  } else {
    wipe(...pal.bg);
  }

  const glyphsLoaded = input?.paint($); // Paint the text input.

  // Paint last command if needed.
  // TODO: This could be a much shorter call...

  let historyTexts;

  if (history.length === 0) {
    historyTexts = ["aesthetic.computer"];
  } else {
    historyTexts = history.map((h) => h.replaceAll("~", " "));
  }

  historyTexts.reverse().forEach((t, i) => {
    const ii = i + 1;
    const yMargin = i === 0 ? 0 : 2;
    ink(140, 90, 235, 80 / ii).printLine(
      t,
      input?.typeface.glyphs,
      6,
      screen.height - 6 * 3 * ii - 6 - yMargin,
      6,
      2,
      0
    );
  });

  if (uploadProgress > 0 || uploadProgress === -1) {
    ink(0).box(1, 1, screen.width - 2, 2);
    if (uploadProgress > 0) {
      ink(scheme.dark.block).box(1, 1, (screen.width - 2) * uploadProgress, 2);
    }
  }

  // Trigger a red or green screen flash with a timer.
  if (flashShow) {
    ink(flashColor).box(0, 0, screen.width, screen.height);
    if (flashMessage) ink(255).write(flashMessage, { x: 5, y: 4, size: 2 });
  }

  // return glyphsLoaded;
  return false;
}

// ✒ Act (Runs once per user interaction, after boot.)
async function act($) {
  const { event: e, needsPaint, store, screen, system, painting } = $;

  if (e.is("reframed")) {
    nopaint_adjust(screen, system, painting, store);
    needsPaint();
  }

  input?.act($);

  if (e.is("load-error")) {
    makeFlash($);
    flashColor = [255, 0, 0];

    if (MetaBrowser) input.canType = false;
    needsPaint();
  }
}

function meta() {
  return {
    title: "prompt · aesthetic.computer",
    desc: "Type a command to get started.",
  };
}

export { boot, sim, paint, act, meta };

// 📚 Library (Useful classes & functions used throughout the piece)

function makeMotd({ handle, user }) {
  motd =
    `"chaos in a system"                             ` +
    `                                                ` +
    `Try typing:                                     ` +
    `                                                ` +
    ` 'ff'                                           ` +
    `  to see Freaky Flowers                         ` +
    `                                                ` +
    ` 'shape'                                        ` +
    `  to paint freehand shapes                      ` +
    `                                                ` +
    ` 'bleep'                                        ` +
    `  to play microtones                            ` +
    `                                                ` +
    `Or...                                           ` +
    `                                                ` +
    ` 'help'                                         ` +
    `  to learn more!                                ` +
    `                                                ` +
    `mail@aesthetic.computer                         `;

  if (user)
    motd =
      `Welcome, ${handle || user.name}!`.padEnd(48) + " ".padEnd(48) + motd;
}

function makeFlash($, clear = true, message) {
  flash = new $.gizmo.Hourglass($.seconds(message ? 0.35 : 0.1), {
    flipped: () => {
      flashShow = false;
      flashPresent = false;
      flashMessage = undefined;
      flash = undefined;
      $.needsPaint();
    },
    autoFlip: true,
  });

  flashPresent = true;
  flashShow = true;
  flashMessage = message;
  if (clear) input.text = "";
}

const ordfish = {
  afajydklwun:
    "5b182278c42d5a74ab4dd9f43826a0a2fce0158000001ed3bdf44873f1378d93i0",
  bhouvfjcwjv:
    "ffea6caf389f625cb58aadcce4e6d59c0ba79e9328fb550df7d7773085b7c2f9i0",
  bsoehhcnrdk:
    "ea356f87bda9f6a2a2f63e2a7aa8182bbd43a0e7ffb42a0db6669a255f90dfb8i0",
  bsoehhdbpto:
    "ebec9d350dbe923cf808f07ac2a98f66d1a5c8fbeada617dc5da0c5b8fe21298i0",
  ewqzayivxuy:
    "9b60a92316a0e1472d1e434c775636e45e9173084c99615bb38eb9e2fd83c1c5i0",
  hqdekuuuyhb:
    "47ce3cac5662cde8d2c228a7d186b2be92ad7a4e00069b9735af84409f96d4d3i0",
  atbfflnurtx:
    "fd29ec8b749dee828bc170c62533bd541d2084039ff6619c5efa1d135d1700d0i0",
  ewqzayirtoi:
    "2e6690811e32b9712d7ae2231f90807a6963938dc6377a544be8212fb80f491ci0",
  atbfflnswvx:
    "d52041661ef0ee12d8dcc784f1274a7a4995b1753d8b37f11a603aa23ee56a24i0",
  ewqzayiqitk:
    "0c369e39812f83d8f1274246f45f5de41cfc1fd008135935aed7bb77dbdb8919i0",
  afajydkiabh:
    "bb31335aa76fab43f19c36bf76923168324d0c17b8cbbf7e083fe8b383120812i0",
  atbfflnrapr:
    "e0f3c237c46dcd5224feae2442f48929e13e0a0589436eca6e65caeaa1941c48i0",
  czvjqrsrfzg:
    "20a439eb33cdc4800c080f71d5848591621687d2c7873605fc62aa59336b1c44i0",
  exgjjlbpmuf:
    "fd0fa116569856df3a22719d8ace732662210ab86440afb20ae3b38cc8ace262i0",
  bsoehhcydbw:
    "e4073a8cb6a5c50b34f1639adcb4ebb2d7c15be29e6bdb3f5b23167a892d1549i0",
  atbfflnofpf:
    "8ecfd56cf0cb086eafafb7c27fa3b66c42d445408bf0d982d94c5163b8c734bdi0",
  cyizglvvggw:
    "7a17276207151786d73149ef59fd197c2df6c460ed13c23d912ed1268813eaf9i0",
  cyizglvsoki:
    "11f894b119f9aa495bc7406d6522ad83a521e1ea86edb89e3909ee365bd609f5i0",
  hqdekuuqjit:
    "762c294f52fb25e0696385ca71061f0d7f6fcfc77e99a0fbdd8c3c714e9deac7i0",
  gxlbncjokam:
    "055777368ab2af60774ce11310b861c241628e775c3d9b5550b1171365a6744fi0",
  exgjjlbleox:
    "452dd0688a9752fd39224e0ea49c2a3b2b04de8b125cff509593ceb962214a9ai0",
  gxlbncjkabl:
    "b7de80e8363791d46f5094ee903c53eed6cc25ba7b21cc68462b31430dadbcfdi0",
  afajydkdnav:
    "3c772118cce5265822adeb7bb768d9d7272a327a9ed1c85e991373bb6c9f3bffi0",
  atbfflnmnrr:
    "15d09ed178c442299e22737dd25cc93036f67b500f64baced2c51b644dbfd16bi0",
  afajydjybwq:
    "eaf0e999a61728d02af1fbed32d72df0020ee2759e1262e760346510bc3d8a5bi0",
  afajydjubgq:
    "d4a3d5ec2f867dedd6c33118830ff61eb2816df18ef18023cc0bc3f8c8d782c1i0",
  atbfflnkhzr:
    "0fa12802efd733c4059fd66ee2d8ee7d9fbc8e460b80991a2fedfdd40115b7d4i0",
  cyizglvotma:
    "d58eb96234dcc2409e01f73a3a3f3176f23a097036c6d9648a407efc04314687i0",
  bhouvfjavyt:
    "34afc143bad54f469ee64d2fb6dda167f590d90d65284783a3002d09be7aedc0i0",
  afajydjpkxa:
    "c5b97ff9749d3bb59cb0864f6e903433fc9a2101e9299934b8ccfeb57b05d875i0",
  gxlbncizwhg:
    "80dcb20cc0f72ece2788a196ff1d72b287a9bb3174b31d8a63d27b0fed98b946i0",
  atbfflnbxxm:
    "b24edd0ade77f84584427c85b94cf46c391600d4224c7b8309ba0b9ce66a9895i0",
  bikdtxjurmm:
    "1575cf2d9b689f4ad4004c7b02a2d899904b5ea42d5416503d9ddfac1d635a6ei0",
  ewqzayimcig:
    "2efb094b7c44220b660eabd3269812c85181bd52a21a37f7778507ffc1bb543ai0",
  bhouvfiukft:
    "129dc4ba01b40cd91d4703ef451bbd22a42bb90f07cbbc42e37e6d91b9d9ea9ci0",
  atbfflmwhua:
    "2721164e0160d4feb1e92030ee747d01159a650737df52778d109f539154f2fei0",
  czvjqrsmtio:
    "56ee4746d25caf6d59a3467fd6f036287db68a51ae7c75f66aacba23ec82a574i0",
  ddqccmegpuz:
    "5973583f8955695ec17b5b634540f500feb585230a8d98aa7a06ec9009c489c7i0",
  atbfflmszgu:
    "c87d1ae363e7812deb8fadf0c54dfabdd6cedb9e09f06a08485b1adeb979b6f8i0",
  blnwvwcksbj:
    "f9dda6d9cc426de067f5374ef9254022a32e3cbd31296d0a6a63a816786be672i0",
  cggizjswstp:
    "a63313ddad09cbe4f8cd35f876c93db6a0dc92f9c8f8541697b74a255bccf8e0i0",
  empraaozdzg:
    "cd86563c0330863252b9bb498034f85371c9d99e5ae0a13338a5a6c3f95a546ci0",
  afajydjjxqz:
    "9e5f39181d7b2cbb18c2000ae5196bcfe6109d3498beca65164b8ab35f034f1di0",
  jahkarqnepw:
    "718deb8a4a2f18a230b5da19cba28780cf768aedb0c97afe6b0cd022ea1f04dei0",
  blnwvwchezz:
    "b8a12a3c23b17234fec5ae5a475041d6113351ab3f48f4e7cb0ebde0db3f19a1i0",
  dphnonhypod:
    "5f25fa2b3604e55352db003b106858807479397536cbe8a40a557ce2b0d3d194i0",
  cggizjsqzhk:
    "e66aae9db6ccf2049ddca95bbec86dfb16d967b0f0935f95f306927ce7e3e962i0",
  empraapnasc:
    "cf33fed2a637b9cf6640f35aa7329bf0792669a74e44a618d3235db16e0978cbi0",
  bcfqtpqefzb:
    "3290fc083f787cb77d9049e246e1f307b0c029e596c6e7993992ada4ba6f1b43i0",
  jahkarpbgus:
    "b32733504fd221006763ee969f113f1b0f4482c4a56b56d0557d3aac3b475d46i0",
  jmnmofemhyx:
    "ee2490d85792d156fe2f7eb40daf1a861295bef0cfff1094e06f555859560007i0",
  aqdaszimniv:
    "634e525f7bafa9d48bd8223deb2e598b0f950df5b7a2966c169f13d8eeacf846i0",
  ddqccmdsbqf:
    "a997f335430dc852e42dddeca28ddb4b915cc09d07ed16794bee6d25aeee9db1i0",
  aqdasziiajw:
    "f704a6b279a766269c19662a314dd68077ac10a46a3edead88b568788cb2a8bdi0",
};
