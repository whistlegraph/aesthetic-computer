// Share, 2023.12.01.23.27.31.234
// Share a link with a QR code of an AC piece.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [x] Make sure work with any url.
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
import * as starfield from "./starfield.mjs";

let cells,
  enter,
  slug,
  url,
  alt = false,
  alt2 = false;

function boot({ api, hud, params, net, ui, blink }) {
  console.log("Params:", params);
  if (params[0]?.startsWith("https://") || params[0]?.startsWith("http://")) {
    url = params[0];
    hud.label(`share ${params[0]}`);
  } else {
    url = net.lan ? net.lan : `https://${net.host}`;
    slug = params.join("~");
    if (slug) url += `/${slug}`;
    if (slug.length === 0) {
      slug = "prompt";
      hud.label(`share prompt`);
    }
  }

  console.log("👐 Sharing:", url);

  cells = qr(url).modules;
  starfield.boot(api, { stars: 512 });
  starfield.wipe(false);
  enter = new ui.TextButton("Enter");
  blink(120, () => (alt = !alt));
  blink(200, () => (alt2 = !alt2));
}

const { floor, min, max } = Math;

function paint({ api, wipe, ink, screen, help: { choose } }) {
  wipe(alt ? [32, 0, 64] : [20, 8, 54]); // Clear the screen.
  starfield.paint(api, { alpha: 0.8, color: [255, 0, 200] }); // 🌟 Backdrop.

  let margin = screen.width / screen.height > 0.8 ? 32 : 6; // 🔳 Paint QR Code
  const width = screen.width - margin * 2;
  const height = screen.height - margin * 2;
  let scale = max(floor(min(width, height) / cells.length), 1); // At least 1.

  const size = cells.length * scale;
  const ox = (screen.width - size) / 2;
  const oy = (screen.height - size) / 2 + 8;

  for (let y = 0; y < cells.length; y += 1) {
    for (let x = 0; x < cells.length; x += 1) {
      const black = cells[y][x];
      if (!alt2) {
        if (black) {
          ink(choose(0, 16, 24, "purple")).box(
            ox + x * scale,
            oy + y * scale,
            scale,
          );
        } else {
          ink(choose(255, 240, 230, "yellow")).box(
            ox + x * scale,
            oy + y * scale,
            scale,
          );
        }
      } else {
        ink(!black).box(ox + x * scale, oy + y * scale, scale);
      }
    }
  }

  ink("white").box(ox, oy, size, size, "outline:2");
  ink(!alt2 ? undefined : "red").box(
    ox - 2,
    oy - 2,
    size + 4,
    size + 4,
    "outline",
  );
  ink("purple").box(ox - 4, oy - 4, size + 8, size + 8, "outline:4");

  ink("magenta", 180).write(url.replace("https://", ""), {
    x: 6,
    y: 18,
  });

  if (!enter.btn.disabled) {
    enter.reposition({ right: 6, bottom: 6, screen });
    enter.paint(api);
  }
}

function act({ event: e, jump }) {
  enter.btn.act(e, { push: () => jump(slug || url) });
}

function sim($) {
  starfield.sim($);
}

function meta() {
  return {
    title: "Share",
    desc: "Share a link with a QR code of an AC piece.",
  };
}

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
