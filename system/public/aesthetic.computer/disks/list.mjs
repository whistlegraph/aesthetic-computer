// List, 2024.1.30.13.18.29.955
// A directory of all system pieces and prompt commands.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let docs;
let merged;
let prompts = [];
let scroll = 0;

async function boot({ ui, typeface, store, net }) {
  // Retrieve scroll if it exists.
  scroll = (await store.retrieve("list:scroll")) || 0;

  // 📔 Get the docs off the api.
  net.requestDocs().then((d) => {
    docs = d;
    merged = { ...docs.pieces, ...docs.prompts };

    keys(merged).forEach((key) => {
      if (merged[key].hidden) delete merged[key]; // Remove hidden commands.
    });

    // Build buttons here.
    keys(merged)
      .sort()
      .forEach((key, i) => {
        const [gw, gh] = typeface.glyphs[0].resolution;
        const w = gw * key.length;
        const h = gh + 1;
        prompts.push({
          word: key,
          button: new ui.Button(6, scroll + 22 + 12 * i, w, h),
        });
      });
  });
}

const { keys } = Object;

function paint({ wipe, ink, ui, hud, screen }) {
  wipe("black");
  if (!docs) return;
  keys(merged)
    .sort()
    .forEach((key, i) => {
      prompts[i].button.paint((b) => {
        ink(b.down ? "yellow" : "white").write(key, {
          x: 6,
          y: scroll + 22 + 12 * i,
        });
        ink("gray").write(merged[key].desc, {
          x: 6 + key.length * 6 + 6,
          y: scroll + 22 + 12 * i,
        });
        // ink("blue", 128).box(b.box);
      });
    });
  ink(0, 128).box(0, 0, screen.width, 18);
  if (anyDown) {
    ink(/*"gray"*/ [200, 30, 100]).box(
      6 + hud.currentLabel.text.length * 6,
      6,
      6,
      10,
    );
  }
}

let anyDown = false;

function act({ event: e, hud, piece, geo, jump, send }) {
  if (!anyDown && e.is("draw:1")) {
    scroll += e.delta.y;
    checkScroll();
    prompts.forEach((p, i) => {
      p.button.box = new geo.Box(
        6,
        scroll + 22 + 12 * i,
        p.button.box.w,
        p.button.box.h,
      );
    });
  }

  if (e.is("scroll")) {
    scroll -= e.y;
    checkScroll();
    needsPaint();
  }

  prompts.forEach((prompt, i) => {
    prompt.button.act(e, {
      push: () => {
        jump("prompt~" + prompt.word)(() => {
          send({ type: "keyboard:open" });
        });
      },
      rollover: (b) => {
        if (anyDown) {
          hud.label(prompt.word, "white");
          prompts.forEach((p) => {
            p.button.down = false;
          });
          b.down = true;
        }
      },
      down: () => {
        console.log("Highlighting:", prompt.word);
        hud.label(prompt.word, "white");
        anyDown = true;
        send({ type: "keyboard:enabled" });
        send({ type: "keyboard:unlock" });
      },
      cancel: () => {
        anyDown = false;
        hud.label(piece);
        console.log("CANCEL");
        send({ type: "keyboard:disabled" });
        send({ type: "keyboard:lock" });
      },
    });
  });
}

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

function leave({ store }) {
  store["list:scroll"] = scroll;
  store.persist("list:scroll");
}

function meta() {
  return {
    title: "List",
    desc: "A directory of all system pieces and prompt commands.",
  };
}

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, leave, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

function checkScroll() {
  if (scroll < -prompts.length * 12) scroll = -prompts.length * 12;
  if (scroll > 0) scroll = 0;
}