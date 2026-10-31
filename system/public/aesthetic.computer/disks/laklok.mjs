// laklok, 2025.5.08.16.31.51.182
// Read along or say hi in the community conversation.
// 🌐 Backed by the branded domain laklok.com.

/* 📝 Notes
  The raster piece and the vector client (`/html/` on laklok.com, source at
  `system/public/html/index.html`) are sisters — the settings pane (mode,
  tema, filter) exists on both and any change here should land there too.
  The update path lives in `toolchain/laklok-sisters/PARITY.md`:
  `parity.mjs` diffs the mirrored constants (run it after touching either
  side) and `sisters.mjs` renders them side by side for the visual half.

  The temas themselves, the saved choice, the interface language, the census
  and the corner chrome (QR, gear, envelope, pane) live in
  `common/laklok-tema.mjs`, shared with `mail` so the two rooms always wear
  the same dress.
 */

import { createCommentCounts } from "../lib/chat-comments.mjs";
import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.
import { laklokAction } from "../lib/laklok-activity.mjs";
import {
  LAK_THEMES,
  realtimeTick,
  pickTema,
  restoreTema,
  saveTema,
  reportTema,
  strings,
  pickLang,
  restoreLang,
  saveLang,
  makeQR,
  paintQR,
  paintGear,
  GEAR,
  paintEnvelope,
  ENVELOPE,
  paintBadge,
  badgeWidth,
  paintTemaPane,
  temaRow,
  langRow,
  LAK_CHAT_TOP,
  lakChatOptions,
} from "./common/laklok-tema.mjs";

let client;
let commentCounts;

// 📐 Top chrome height — shared between chat.paint (topMargin) and the circus
// marquee so the banner fills the whole header down to the margin line.
const LAK_TOP_MARGIN = LAK_CHAT_TOP;

// 📱 laklok.com QR rendered in the top-right corner (see paintCorner).
let lakQRCells = null;

// ⚙️ Settings pane state — mode (raster here / vector on laklok.com/html),
// tema, the media-links filter, and the interface language. The same pane
// exists on the vector side.
let lakTheme = "ler";
let lakLang = "da"; // the room's mother tongue, until the visitor says otherwise
let lakLinksOnly = false;
let settingsOpen = false;
let gearBox = null; // {x, y, w, h} hit area for the ⚙ toggle
let lakTV = false; // 📺 `~tv` colon token — broadcast chrome (no input, no gear)
let settingsHits = []; // [{x, y, w, h, action}] chips, rebuilt each paint

// 📬 The door to `mail`, beside the gear: an envelope, lit when something is
// waiting, with a red count of what's unread. Only a signed-in visitor has a
// box, so only they get the door.
let mailCount = null; // { unread, total } once asked
let mailBox = null; // hit area, or null when the door isn't drawn
let mailAsking = false; // one count request in flight at a time
let mailAskedAt = 0; // when the last count was asked, ms
let mailAsks = 0; // how many times, for the test hook
// People sit in this room all day, so the count is re-asked on a cadence
// and again when the window comes back into focus. `let` so a test can
// shorten it (see installLakTestHook).
let mailPollMs = 60_000;

// 🧪 Test hook — a BroadcastChannel the browser e2e listens on
// (tests/browser/mail-journey.test.mjs), like chat.mjs' own hook but for the
// envelope: one snapshot per resolved count. Only when booted with `?test=1`.
let lakTestChannel = null;
function installLakTestHook(query) {
  lakTestChannel?.close();
  lakTestChannel = null;
  if (!query?.test || typeof BroadcastChannel === "undefined") return;
  lakTestChannel = new BroadcastChannel("ac-mail-test");
  lakTestChannel.onmessage = ({ data }) => {
    if (data?.type === "mail-poll" && Number.isFinite(data.every)) {
      mailPollMs = Math.max(1000, data.every);
    }
  };
}
function tellLakTest() {
  lakTestChannel?.postMessage({
    ready: true,
    piece: "laklok",
    mail: mailCount ? { unread: mailCount.unread || 0, total: mailCount.total || 0 } : null,
    asks: mailAsks,
  });
}

// 📬 Ask whether there's mail waiting — two counts, not the inbox. The badge
// repaints when the count moved.
function askMail($) {
  mailAsking = true;
  mailAskedAt = Date.now();
  mailAsks += 1;
  const settle = (count) => {
    mailAsking = false;
    const moved = count.unread !== mailCount?.unread || count.total !== mailCount?.total;
    mailCount = count;
    if (moved) $.needsPaint?.();
    tellLakTest();
  };
  $.net
    .userRequest("GET", "/api/mail?count=1")
    .then((res) => settle(res?.status === 200 ? res : { unread: 0, total: 0 }))
    .catch(() => settle({ unread: 0, total: 0 }));
}

// 🔗 What counts as a media link — hosts that ARE media plus direct files.
// Mirrored verbatim in the vector client; edit both or the sisters drift.
const LAK_MEDIA_LINK =
  /(youtube\.com\/(watch|shorts|embed)|youtu\.be\/|vimeo\.com\/|tiktok\.com\/|soundcloud\.com\/|bandcamp\.com|\.(png|jpe?g|gif|webp|mp4|mov|webm|mp3|wav|ogg|m4a)\b)/i;
function hasMediaLink(text) {
  if (!text) return false;
  const urls = text.match(/https?:\/\/[^\s]+|www\.[^\s]+/gi);
  return !!urls && urls.some((u) => LAK_MEDIA_LINK.test(u));
}

// The chat system, filtered down to media links when the filter is on. A
// fresh shallow view per frame — message objects keep their identity so the
// renderer's per-message caches stay warm.
function chatView() {
  const sys = client.system;
  if (!lakLinksOnly) return sys;
  return { ...sys, messages: sys.messages.filter((m) => hasMediaLink(m.text)) };
}

function boot({ api, wipe, debug, send, hud, store, colon, params, jump, net, query }) {
  client = new Chat(debug, send);
  commentCounts?.dispose();
  commentCounts = createCommentCounts("clock", () => api.needsPaint?.());
  client.system.commentCounts = commentCounts.counts;
  client.system.commentInstance = "clock";
  client.connect("clock"); // Connect to 'clock' chat. (DB stays `chat-clock`.)
  chat.boot(api, client.system, { onAction: name => laklokAction(api, name) }); // Use default font

  // 🚫 chat.boot stamps a prompt.ac/chat QR to the LEFT of the HUD label; clear
  // it so laklok shows only its own laklok.com QR in the top-right (paintCorner).
  hud.qr(null);

  // ⚙️ Colon overrides first (`laklok:nat:links` — handy for the sisters
  // suite and shareable themed URLs; `laklok:vector` jumps straight to the
  // vector client), then saved preferences fill whatever colon left alone.
  settingsOpen = false;
  lakLinksOnly = false;
  lakTV = false;
  mailCount = null;
  mailBox = null;
  mailAsking = false;
  mailAskedAt = 0;
  mailAsks = 0;
  installLakTestHook(query);
  let colonLinks = false;
  // URL `~` separators land in params, `:` in colon — `laer-klokken~tv`
  // and `laklok:tv` should both reach the same switches, so scan both.
  const tokens = [...(colon || []), ...(params || [])];
  for (const token of tokens) {
    if (token === "links") { lakLinksOnly = true; colonLinks = true; }
    if (token === "alle") { lakLinksOnly = false; colonLinks = true; }
    if (token === "vector") jump("out:https://laklok.com/html/");
    // 📺 `laer-klokken~tv` / `laklok~tv` — the broadcast face: no login or
    // message bar, no settings gear; the room is the whole picture.
    if (token === "tv") lakTV = true;
  }
  const tema = pickTema(tokens, store);
  lakTheme = tema.name;
  restoreTema(store, tema.pinned, (saved) => {
    if (saved) {
      lakTheme = saved;
      chat.refresh(client.system);
    }
    reportTema(net, lakTheme); // after the saved tema has had its say
  });
  const lang = pickLang(tokens, store, "da");
  lakLang = lang.name;
  restoreLang(store, lang.pinned, (saved) => {
    if (saved) lakLang = saved;
  });
  if (!colonLinks && typeof store["laklok:links"] === "boolean") {
    lakLinksOnly = store["laklok:links"];
  }
  store.retrieve("laklok:links").then((v) => {
    if (!colonLinks && typeof v === "boolean") {
      lakLinksOnly = v;
      chat.refresh(client.system);
    }
  });

  // 📱 Generate the laklok.com QR once; painted top-right each frame.
  lakQRCells = makeQR("https://laklok.com");
  // 🏷️ Ensure label shows piece name (not "chat"), pinned white so it doesn't
  // ride the red/orange/lime connection-status color.
  hud.label("laklok", "white");
  // 🌐 Complete the laklok.com wordmark on the same baseline.
  hud.suffix(".com");
}

// 🎪 "Laer Klokken" circus banner — a striped marquee across the header, the
// name in MatrixChunky8 with every character a different circus color and its
// own little bounce, a pixel pumpkin on the left and a flapping bat on the
// right. The sign lives in the lane between the HUD label and the corner
// chrome (envelope, gear, QR) and sheds its critters, then itself, before it
// would ever run under either.
const LAK_SIGN_FONT = "MatrixChunky8";
const LAK_SIGN_H = 8; // MatrixChunky8 glyph height
const LAK_SIGN_TOP = 7; // beside the HUD label's row, above the presence line
const LAK_SIGN_GAP = 1; // extra letter spacing when there's room
const LAK_CIRCUS_COLS = [
  [255, 80, 80],   // red
  [255, 210, 70],  // gold
  [120, 210, 255], // sky
  [140, 240, 150], // green
  [255, 150, 220], // pink
  [180, 150, 255], // violet
];
const LAK_HUD_FALLBACK_WIDTH = 78;
const LAK_HEADER_GUTTER = 4;

// 🎃 A jack-o'-lantern, 9×8.
const PUMPKIN = {
  rows: [
    "....s....",
    "...ss....",
    ".ooorooo.",
    "ooyoroyoo",
    "ooooroooo",
    "oyoooooyo",
    "ooyyyyyoo",
    ".ooorooo.",
  ],
  palette: {
    s: [90, 170, 70],
    o: [255, 140, 30],
    r: [225, 105, 20],
    y: [255, 235, 90],
  },
};

// 🦇 A bat, 11×5, two wing frames.
const BAT_FRAMES = [
  [
    "w.........w",
    "ww.......ww",
    ".ww.ybY.ww.",
    "..wwbbbww..",
    "....bbb....",
  ],
  [
    "...........",
    "....ybY....",
    ".wwwbbbwww.",
    "ww..bbb..ww",
    "w.........w",
  ],
];
const BAT_PALETTE = { w: [40, 28, 60], b: [30, 20, 45], y: [255, 70, 70], Y: [255, 70, 70] };
const BAT_W = 11;
const BAT_H = 5;
const BAT_DRIFT = 3; // px the bat flutters left and right of its perch
const PUMPKIN_W = 9;
const SIGN_PAD = 3; // between a critter and the word

function paintSprite($, rows, palette, x, y) {
  const { ink } = $;
  for (let r = 0; r < rows.length; r++) {
    const row = rows[r];
    for (let c = 0; c < row.length; c++) {
      const col = palette[row[c]];
      if (col) ink(col[0], col[1], col[2]).box(x + c, y + r, 1, 1);
    }
  }
}

// 📐 The header's free lane: from the HUD label's right edge to the corner
// chrome's left edge, measured each frame so a long handle label, an unread
// badge or a narrow phone all push the sign rather than overlap it.
function signLane($) {
  const { screen, hud } = $;
  const hudBox = hud?.currentLabel?.()?.btn?.box;
  const hudRight = hudBox
    ? (hudBox.x || 0) + (hudBox.w || 0)
    : 6 + LAK_HUD_FALLBACK_WIDTH;
  const corner = cornerLayout($);
  const right = (corner ? corner.left : screen.width) - LAK_HEADER_GUTTER;
  return { left: hudRight + LAK_HEADER_GUTTER, right };
}

// Fit the word (and critters) into the lane, dropping pieces in order:
// letter spacing, then the bat, then the pumpkin, then the word itself.
function signLayout($, label, lane) {
  const { text, screen } = $;
  const widths = [...label].map((ch) => text.width(ch, LAK_SIGN_FONT));
  const glyphs = widths.reduce((a, b) => a + b, 0);
  const available = lane.right - lane.left;
  const tries = [
    { gap: LAK_SIGN_GAP, pumpkin: true, bat: true },
    { gap: 0, pumpkin: true, bat: true },
    { gap: LAK_SIGN_GAP, pumpkin: true, bat: false },
    { gap: 0, pumpkin: true, bat: false },
    { gap: 0, pumpkin: false, bat: false },
  ];
  for (const t of tries) {
    const word = glyphs + t.gap * (label.length - 1);
    const width = word
      + (t.pumpkin ? PUMPKIN_W + SIGN_PAD : 0)
      + (t.bat ? BAT_W + BAT_DRIFT * 2 + SIGN_PAD : 0);
    if (width > available) continue;
    // Prefer the screen's centre when that still sits inside the lane.
    let left = Math.round((screen.width - width) / 2);
    if (left < lane.left || left + width > lane.right) {
      left = lane.left + Math.round((available - width) / 2);
    }
    return { ...t, left, width, widths };
  }
  return null;
}

function paintLaerKlokkenSign($, headerHeight = LAK_TOP_MARGIN) {
  const { ink, screen } = $;
  const now = typeof performance !== "undefined" ? performance.now() : 0;
  const t = now * 0.004;
  const label = "Laer Klokken";
  const themed = LAK_THEMES[lakTheme];

  // Striped circus backdrop, slowly scrolling like a barber pole, tuned per
  // theme so the banner reads as one piece with the room.
  const stripeW = 14;
  const scroll = Math.floor(t * 6);
  for (let bx = 0; bx < screen.width; bx += stripeW) {
    const odd = Math.floor((bx + scroll) / stripeW) % 2;
    const c = odd ? themed.stripeB : themed.stripeA;
    ink(c[0], c[1], c[2]).box(bx, 0, Math.min(stripeW, screen.width - bx), headerHeight);
  }

  const layout = signLayout($, label, signLane($));
  if (!layout) return; // too tight: the HUD label stays the sole title
  let cx = layout.left;
  const top = LAK_SIGN_TOP;

  // 🎃 Pumpkin, sitting still on the left.
  if (layout.pumpkin) {
    paintSprite($, PUMPKIN.rows, PUMPKIN.palette, cx, top);
    cx += PUMPKIN_W + SIGN_PAD;
  }

  // Each character: its own circus color + individual vertical bounce.
  for (let i = 0; i < label.length; i++) {
    const ch = label[i];
    if (ch !== " ") {
      const bounce = Math.round(Math.sin(t * 2.6 + i * 0.7));
      const c = LAK_CIRCUS_COLS[i % LAK_CIRCUS_COLS.length];
      ink(20, 10, 6).write(ch, { x: cx + 1, y: top + bounce + 1 }, undefined, undefined, false, LAK_SIGN_FONT);
      ink(c[0], c[1], c[2]).write(ch, { x: cx, y: top + bounce }, undefined, undefined, false, LAK_SIGN_FONT);
    }
    cx += layout.widths[i] + layout.gap;
  }

  // 🦇 Bat, flapping and fluttering on the right.
  if (layout.bat) {
    const frame = BAT_FRAMES[Math.floor(t * 1.5) % 2];
    const bx = cx + SIGN_PAD + BAT_DRIFT + Math.round(Math.sin(t * 0.9) * BAT_DRIFT);
    const by = top + Math.round((LAK_SIGN_H - BAT_H) / 2) + Math.round(Math.sin(t * 1.3) * 2);
    paintSprite($, frame, BAT_PALETTE, bx, by);
  }
}

// 📐 Where the corner chrome lands, right to left: QR, gear, envelope (+ its
// badge). `left` is the leftmost pixel anything in the corner occupies, so the
// sign can keep clear of it. Shared by paintCorner and signLane.
function cornerLayout($) {
  if (!lakQRCells) return null;
  const { screen } = $;
  const qrBoxSize = lakQRCells.length + 2;
  const rightInset = 3; // Match the QR's 3px top inset (4px below at this height).
  const qrX = screen.width - qrBoxSize - rightInset;
  const qrY = Math.floor((LAK_TOP_MARGIN - qrBoxSize) / 2);
  const out = { qrX, qrY, qrBoxSize, left: qrX };
  if (lakTV) return out; // 📺 no controls on television
  out.gx = qrX - GEAR - 4;
  out.gy = qrY + Math.floor((qrBoxSize - GEAR) / 2);
  out.left = out.gx;
  if (!mailCount) return out;
  const unread = mailCount.unread || 0;
  out.label = unread > 0 ? `${unread}` : null;
  // The badge overhangs the envelope's top-right corner, so the cluster is
  // the envelope plus that overhang.
  out.cluster = ENVELOPE.w + (out.label ? badgeWidth(out.label) - 3 : 0);
  out.ex = out.gx - 6 - out.cluster;
  out.ey = qrY + Math.floor((qrBoxSize - ENVELOPE.h) / 2);
  out.mailBox = { x: out.ex - 4, y: out.ey - 6, w: out.cluster + 8, h: ENVELOPE.h + 12 };
  out.left = out.mailBox.x;
  return out;
}

// 📱 The top-right corner, right to left: the laklok.com QR on white paper,
// the ⚙ settings toggle, and — for a signed-in visitor — the envelope that
// opens `mail`, wearing a red count when letters are waiting.
function paintCorner($) {
  const c = cornerLayout($);
  if (!c) return;
  paintQR($, lakQRCells, c.qrX, c.qrY);

  if (lakTV) { gearBox = null; mailBox = null; return; }
  gearBox = paintGear($, c.gx, c.gy, settingsOpen);

  if (!c.mailBox) { mailBox = null; return; }
  mailBox = c.mailBox;
  const { pen } = $;
  const hot = !!pen && pen.x >= mailBox.x && pen.x < mailBox.x + mailBox.w &&
    pen.y >= mailBox.y && pen.y < mailBox.y + mailBox.h;
  paintEnvelope($, c.ex, c.ey, { lit: (mailCount.unread || 0) > 0, hot });
  if (c.label) paintBadge($, c.ex + ENVELOPE.w - 3, c.ey - 5, c.label, hot);
}

// ⚙️ The settings pane — mode / tema / filter / sprog, drawn under the header
// by the QR. Chips register their hit boxes into `settingsHits` for act().
function paintSettings($) {
  settingsHits = [];
  if (!settingsOpen) return;
  const { screen } = $;
  const s = strings(lakLang);

  const rows = [
    {
      label: s.mode,
      chips: [
        { text: "raster", selected: true, action: { type: "mode", value: "raster" } },
        { text: "vector", selected: false, action: { type: "mode", value: "vector" } },
      ],
    },
    temaRow(lakTheme, s),
    {
      label: s.filter,
      chips: [
        { text: s.all, selected: !lakLinksOnly, action: { type: "links", value: false } },
        { text: s.links, selected: lakLinksOnly, action: { type: "links", value: true } },
      ],
    },
    langRow(lakLang, s),
  ];

  settingsHits = paintTemaPane($, {
    theme: LAK_THEMES[lakTheme],
    rows,
    right: screen.width - 3,
    top: LAK_TOP_MARGIN + 2,
    title: s.settings,
  });
}

function paint($) {
  // The widgets themselves are shared with `lairk` — see lakChatOptions.
  chat.paint($, lakChatOptions(lakTheme, {
    otherChat: chatView(),
    inputPlaceholder: "Say hi...",
    hideInput: lakTV, // 📺 broadcast face drops the login/message footer
    // QR box + gear + breathing room, and the envelope when it's drawn.
    presenceRightInset: 50 + (mailBox ? mailBox.w + 2 : 0),
    // 🎪 Circus marquee as the header backdrop — fills the whole chrome panel,
    // painted under the online counter so the counter stays readable on top.
    paintHeader: (api, tm) => paintLaerKlokkenSign(api, tm),
  }));

  // 🎪 The circus marquee is painted as the chat header backdrop (via the
  // paintHeader option above), so it fills the header under the counter.
  // 📱 QR + ⚙ gear + 📬 envelope, top-right corner (over everything).
  paintCorner($);
  paintSettings($);
}

function act($) {
  const { event: e, jump, store, needsPaint, net } = $;

  const hit = (box) =>
    box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;

  // ⚙️ Gear toggles the pane; while open, the pane owns every pointer event
  // so a tap on a chip never scrolls the chat underneath.
  if (e.is("touch") && hit(gearBox)) {
    settingsOpen = !settingsOpen;
    if (settingsOpen) laklokAction($, "settings_opened");
    needsPaint?.();
    return;
  }

  // 📬 The envelope is the door to mail.
  if (e.is("touch") && !settingsOpen && hit(mailBox)) {
    laklokAction($, "mail_open_requested");
    jump("mail");
    return;
  }

  // Back from another tab or window: the count is due now, not next minute.
  // (bios forwards window focus; a plain visibilitychange never reaches act.)
  if (e.is("focus")) mailAskedAt = 0;

  if (settingsOpen && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    if (e.is("touch")) {
      const chip = settingsHits.find((h) => hit(h));
      if (chip) {
        const { type, value } = chip.action;
        if (type === "mode" && value === "vector") {
          laklokAction($, "mode_switch_requested");
          jump("out:https://laklok.com/html/");
        } else if (type === "theme") {
          if (lakTheme !== value) laklokAction($, "theme_changed");
          lakTheme = value;
          saveTema(store, value);
          if (value === "realtime") realtimeTick(); // catch up before first paint
          chat.refresh(client.system); // recolor cached message lines
          reportTema(net, value);
        } else if (type === "links") {
          if (lakLinksOnly !== value) laklokAction($, "filter_changed");
          lakLinksOnly = value;
          store["laklok:links"] = value;
          store.persist("laklok:links");
          chat.refresh(client.system); // relayout the filtered feed
        } else if (type === "lang") {
          if (lakLang !== value) laklokAction($, "language_changed");
          lakLang = value;
          saveLang(store, value);
        }
      } else if (!hit(settingsHits.pane)) {
        settingsOpen = false; // Tap outside closes.
      }
      needsPaint?.();
    }
    return;
  }

  chat.act($, chatView(), { allowDelete: true, onAction: name => laklokAction($, name) });
}

function sim($) {
  commentCounts?.refresh(client.system.messages);
  // 🌅 The ambient tema drifts a hair each minute; recolor when it does.
  if (lakTheme === "realtime" && realtimeTick()) chat.refresh(client.system);

  // 📬 Ask whether there's mail waiting — once the signed-in user has settled
  // (a beat after boot, like the prompt curtain), then every mailPollMs.
  if ($.user && !lakTV && !mailAsking && Date.now() - mailAskedAt >= mailPollMs) {
    askMail($);
  }

  chat.sim($);
}

function leave() {
  commentCounts?.dispose();
  client.kill();
  lakTestChannel?.close();
  lakTestChannel = null;
}

export { boot, paint, act, sim, leave };
