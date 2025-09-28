// List, 2024.1.30.13.18.29.955
// A directory of all system pieces and prompt commands.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let list;
let prompts = [];
let scroll = 0;

async function boot({ ui, typeface, store, net, params }) {
  // Retrieve scroll if it exists.

  function populate(data) {
    list = data;

    keys(list).forEach((key) => {
      if (list[key].hidden) delete list[key]; // Remove hidden commands.
    });

    // const listKeys = keys(list);
    // console.log("Total:", listKeys.length);

    // Build buttons here.
    keys(list)
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
  }

  if (!params[0]) {
    // 📔 Get the docs off the api.
    net.requestDocs().then(async (docs) => {
      scroll = (await store.retrieve("list:scroll")) || 0;
      populate({ ...docs.pieces, ...docs.prompts });
    });
  } else {
    // 🫅 Get a handle's pieces as the list.
    fetch(`/media/${params[0]}/pieces`)
      .then((response) => response.json())
      .then((json) => {
        const data = json.files.reduce((acc, url) => {
          const parsedUrl = new URL(url);
          const pathSegments = parsedUrl.pathname.split("/").filter(Boolean);
          const key = `${pathSegments[1]}/${pathSegments[3].replace(".mjs", "")}`;
          acc[key] = {};
          return acc;
        }, {});
        populate(data);
      })
      .catch((err) => {
        console.error("Error:", err);
      });
  }
}

const { keys } = Object;

function paint({ wipe, ink, ui, hud, screen, paintCount }) {
  wipe("black");
  if (!list) {
    if (paintCount > 8n) {
      ink("green").write("Fetching...", { center: "xy" });
    }
    return;
  }
  keys(list)
    .sort()
    .forEach((key, i) => {
      prompts[i].button.paint((b) => {
        ink(b.down ? "yellow" : "white").write(key, {
          x: 6,
          y: scroll + 22 + 12 * i,
        });
        ink("gray").write(list[key].desc, {
          x: 6 + key.length * 6 + 6,
          y: scroll + 22 + 12 * i,
        });
      });
    });
  ink(0, 128).box(0, 0, screen.width, 18);
  if (anyDown) {
    ink(/*"gray"*/ [200, 30, 100]).box(
      6 + (hud.currentLabel().plainText || hud.currentLabel().text || "").length * 6,
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
        jump("prompt~" + prompt.word);
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
      },
      cancel: () => {
        anyDown = false;
        hud.label(piece);
      },
    });
  });
}

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

export { boot, paint, act, leave, meta };

// 📚 Library

function checkScroll() {
  if (scroll < -prompts.length * 12) scroll = -prompts.length * 12;
  if (scroll > 0) scroll = 0;
}
