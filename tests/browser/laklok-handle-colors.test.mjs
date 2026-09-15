// laklok-handle-colors.test, 2026.09.15
// End-to-end proof that per-character handle colors reach the laklok
// (laer-klokken) room: every author in the room gets asked about once, the
// answer matches the handle-colors API, and a colored handle is painted in
// its own colors — read off a screenshot of the running page, not a flag.
//
//   npm run test:laklok:colors           # headless, production
//   npm run test:laklok:colors:local     # against a local `npm run site`
//   AC_HEADED=1 npm run test:laklok:colors
//
// The piece publishes its state on the "ac-chat-test" BroadcastChannel when
// booted with `?test=1` (disks/chat.mjs, installChatTestHook). A target
// without that hook fails the first scenario, which is the point.
//
// Puppeteer needs a Chrome; with none downloaded, point it at the installed
// one: PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome".

import { PNG } from "pngjs";
import { ACSession, CONFIG, scenario, report } from "./ac-harness.mjs";

const base = new URL(CONFIG.baseURL);
const isLocal = base.hostname === "localhost" || base.hostname === "127.0.0.1";
// A local `npm run site` is self-signed; Node's fetch must accept it too.
if (isLocal) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const ac = await ACSession.open();

// The harness flips acDEBUG on so prompt pieces surface their hooks — but
// in debug the chat lib dials a local chat-clock (localhost:8085). This
// test wants the real room, so debug goes off and the hook rides ?test=1.
await ac.page.evaluateOnNewDocument(() => {
  window.acDEBUG = false;
  let snapshot = null;
  const channel = new BroadcastChannel("ac-chat-test");
  channel.onmessage = ({ data }) => {
    if (data?.ready) snapshot = data;
  };
  window.__acChatTest = () => snapshot;
  window.__acChatSeed = (handle, colors) =>
    channel.postMessage({ type: "seed-colors", handle, colors });
});

const state = () => ac.page.evaluate(() => window.__acChatTest?.() || null);

async function waitFor(pred, { timeout = 30000, every = 250, label = "condition" } = {}) {
  const t0 = Date.now();
  let last = null;
  while (Date.now() - t0 < timeout) {
    last = await state();
    if (last && pred(last)) return last;
    await ac.wait(every);
  }
  throw new Error(`timed out waiting for ${label}; last state: ${JSON.stringify(last)?.slice(0, 600)}`);
}

// Distinct rgb triples the page actually shows inside a box given in piece
// pixels. The runtime scales its canvas to the viewport, so the box is mapped
// through the canvas' on-page rect and cut out of a real screenshot.
async function paintedColors(box, screen) {
  const rect = await ac.page.evaluate(() => {
    const r = document.querySelector("#aesthetic-computer canvas").getBoundingClientRect();
    return { x: r.x, y: r.y, w: r.width, h: r.height };
  });
  const sx = rect.w / screen.w;
  const sy = rect.h / screen.h;
  const clip = {
    x: rect.x + box.x * sx,
    y: rect.y + box.y * sy,
    width: Math.max(1, box.w * sx),
    height: Math.max(1, box.h * sy),
  };
  const bytes = await ac.page.screenshot({ clip, encoding: "binary" });
  const png = PNG.sync.read(Buffer.from(bytes));
  const seen = new Set();
  for (let i = 0; i < png.data.length; i += 4) {
    seen.add(`${png.data[i]},${png.data[i + 1]},${png.data[i + 2]}`);
  }
  return { seen: [...seen], clip };
}

const rgb = (c) => `${c.r},${c.g},${c.b}`;
const clean = (h) => (h.startsWith("@") ? h.slice(1) : h);

async function apiColors(handle) {
  // The functions path answers on both lith and `netlify dev`; /api/ only on lith.
  const res = await fetch(
    `${CONFIG.baseURL}/.netlify/functions/handle-colors?handle=${encodeURIComponent(clean(handle))}`,
  );
  if (!res.ok) throw new Error(`handle-colors ${res.status} for @${handle}`);
  const { colors } = await res.json();
  return Array.isArray(colors) && colors.length > 0 ? colors : null;
}

let room = null;

try {
  await scenario("laklok boots and the room fills", async (expect) => {
    await ac.boot("laklok?test=1");
    room = await waitFor((s) => s.messageCount > 0, {
      label: "chat-clock history (is the piece hooked, and the room reachable?)",
    });
    await ac.shot("laklok-colors/01-room");
    expect(room.messageCount > 0, `room has messages (${room.messageCount})`);
    expect(Object.keys(room.authors).length > 0, `room has authors (${Object.keys(room.authors).length})`);
  });

  await scenario("every author is asked once and the cache matches the API", async (expect) => {
    // `netlify dev` answers each function call slowly (fresh DB connect), so
    // a room with a couple dozen authors can take a while to settle.
    room = await waitFor(
      (s) => Object.values(s.authors).every((a) => a.cached),
      { timeout: 90000, label: "colors resolved for every author" },
    );
    const authors = Object.keys(room.authors);
    const expected = {};
    for (const h of authors) expected[h] = await apiColors(h);

    let mismatches = 0;
    for (const h of authors) {
      const want = expected[h] ? expected[h].length : null;
      const got = room.authors[h].colors;
      if (want !== got) {
        mismatches++;
        console.log(`  ✗ @${h}: api says ${want} colors, piece cached ${got}`);
      }
    }
    expect(mismatches === 0, `cached colors match the handle-colors API for ${authors.length} authors`);

    const colored = authors.filter((h) => expected[h]);
    console.log(`  ℹ️  authors with colors set: ${colored.length ? colored.map((h) => "@" + h).join(" ") : "none"}`);

    // No re-asking: the fetch counter must sit still while the room idles.
    const before = room.fetches;
    await ac.wait(3000);
    const after = (await state()).fetches;
    expect(
      after === before,
      `no refetch storm while idle (${before} → ${after} asks for ${authors.length} authors + ${room.presence.length} online)`,
    );
    expect(
      before <= authors.length + room.presence.length + 2,
      `one ask per handle at most (${before})`,
    );
  });

  await scenario("a real colored author paints in their colors", async (expect) => {
    const st = await state();
    const visibleColored = [];
    for (const v of st.visible) {
      const colors = await apiColors(v.from);
      if (colors) visibleColored.push({ ...v, colors });
    }
    if (visibleColored.length === 0) {
      console.log("  ℹ️  no visible author has custom colors right now — seeded scenario covers the paint path");
      expect(true, "skipped (no colored author on screen)");
      return;
    }
    const v = visibleColored[0];
    expect(v.colored, `@${clean(v.from)} was painted through the custom-color path`);
    const distinct = [...new Set(v.colors.map(rgb))];
    const { seen, clip } = await paintedColors(v, st.screen);
    const hits = distinct.filter((c) => seen.includes(c)).length;
    await ac.shot("laklok-colors/02-real-colors");
    expect(
      hits >= Math.min(3, distinct.length),
      `page shows @${clean(v.from)}'s colors (${hits}/${distinct.length} at ${JSON.stringify(clip)})`,
    );
  });

  await scenario("seeded colors paint per character (deterministic)", async (expect) => {
    let st = await state();
    // The lowest visible message sits clear of the header; take its author.
    const target = [...st.visible].sort((a, b) => b.y - a.y)[0];
    expect(!!target, "a visible message to seed");
    if (!target) return;

    const handle = clean(target.from);
    // Loud, unmistakable colors — one per character of "@handle".
    const palette = [
      [250, 10, 20], [10, 250, 20], [20, 10, 250], [250, 250, 10],
      [250, 10, 250], [10, 250, 250], [250, 130, 10], [130, 10, 250],
    ];
    const colors = [...("@" + handle)].map((_, i) => {
      const [r, g, b] = palette[i % palette.length];
      return { r, g, b };
    });
    await ac.page.evaluate((h, c) => window.__acChatSeed(h, c), handle, colors);

    st = await waitFor(
      (s) => s.visible.some((v) => clean(v.from) === handle && v.colored),
      { timeout: 10000, label: `@${handle} to repaint in seeded colors` },
    );
    const box = st.visible.find((v) => clean(v.from) === handle);
    await ac.wait(400); // one more paint so the page holds the new lines
    await ac.shot("laklok-colors/03-seeded");

    const distinct = [...new Set(colors.map(rgb))];
    const { seen, clip } = await paintedColors(box, st.screen);
    const hits = distinct.filter((c) => seen.includes(c));
    expect(
      hits.length >= Math.min(3, distinct.length),
      `page shows the seeded per-character colors (${hits.length}/${distinct.length} at ${JSON.stringify(clip)}; box ${JSON.stringify(box)})`,
    );
  });
} finally {
  await ac.close();
}

process.exit(report());
