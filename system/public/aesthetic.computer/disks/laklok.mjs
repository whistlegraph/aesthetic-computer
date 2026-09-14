// laklok, 2025.5.08.16.31.51.182
// Learn the 'clock'! (formerly `laer-klokken`; that path still aliases here.)
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

import { Chat } from "../lib/chat.mjs"; // TODO: Eventually expand to `net.Socket`
import * as chat from "./chat.mjs"; // Import chat everywhere.
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
} from "./common/laklok-tema.mjs";

let client;

// 📐 Top chrome height — shared between chat.paint (topMargin) and the circus
// marquee so the banner fills the whole header down to the margin line.
const LAK_TOP_MARGIN = 34;

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
let mailCount = null; // { unread, total } once asked; "asking" in flight
let mailBox = null; // hit area, or null when the door isn't drawn

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

function boot({ api, wipe, debug, send, hud, store, colon, params, jump, net }) {
  client = new Chat(debug, send);
  client.connect("clock"); // Connect to 'clock' chat. (DB stays `chat-clock`.)
  chat.boot(api, client.system); // Use default font

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

// 🎪 "Laer Klokken" circus banner — a striped, gold-trimmed marquee in the
// top-right header strip, in GNU Unifont, with every character a different
// circus color and its own little bounce. Masked to the top header area.
const LAK_SIGN_FONT = "unifont";
const LAK_CIRCUS_COLS = [
  [255, 80, 80],   // red
  [255, 210, 70],  // gold
  [120, 210, 255], // sky
  [140, 240, 150], // green
  [255, 150, 220], // pink
  [180, 150, 255], // violet
];

// The canvas uses logical pixels, so phone browsers are typically around 256px
// wide even when their CSS viewport is much larger. Below this breakpoint the
// full 8px circus spacing collides with both the HUD label and the QR code.
const LAK_COMPACT_HEADER_MAX = 399;
const LAK_HUD_FALLBACK_WIDTH = 78;
const LAK_HEADER_GUTTER = 4;

function getSignLayout($, labelLength, charWidth) {
  const { screen, hud } = $;
  const qrSize = lakQRCells?.length || 0;

  if (screen.width > LAK_COMPACT_HEADER_MAX) {
    const gap = 8;
    const width = labelLength * charWidth + gap * Math.max(0, labelLength - 1);
    return width <= screen.width - LAK_HEADER_GUTTER
      ? { left: Math.round((screen.width - width) / 2), gap }
      : null;
  }

  const hudBox = hud?.currentLabel?.()?.btn?.box;
  const hudRight = hudBox
    ? (hudBox.x || 0) + (hudBox.w || 0)
    : 6 + LAK_HUD_FALLBACK_WIDTH;
  const left = hudRight + LAK_HEADER_GUTTER;
  const right = screen.width - qrSize - 4 - LAK_HEADER_GUTTER;
  const available = right - left;
  const glyphWidth = labelLength * charWidth;
  const gaps = Math.max(0, labelLength - 1);
  const gap = gaps > 0 ? Math.min(2, Math.floor((available - glyphWidth) / gaps)) : 0;

  return gap >= 0 ? { left, gap } : null;
}

function paintLaerKlokkenSign($, headerHeight = LAK_TOP_MARGIN) {
  const { ink, screen } = $;
  const now = typeof performance !== "undefined" ? performance.now() : 0;
  const t = now * 0.004;
  const label = "Laer Klokken";
  const themed = LAK_THEMES[lakTheme];

  // Unifont latin glyphs are a fixed 8px advance — no need to measure per frame.
  const CHAR_W = 8;
  const headerH = headerHeight; // marquee fills the header down to the margin
  const sx = 0;
  const sy = 0; // flush with the top of the screen (no gap)
  const sw = screen.width; // full-width banner across the header
  const sh = headerH;
  // Wide screens keep the spacious centered marquee. Phones compact it into
  // the measured gap between the HUD label and QR; if that gap is too small,
  // the branded HUD label remains as the sole title rather than overlapping.
  const signLayout = getSignLayout($, label.length, CHAR_W);

  // Striped circus backdrop, slowly scrolling like a barber pole, tuned per
  // theme so the banner reads as one piece with the room.
  const stripeW = 14;
  const scroll = Math.floor(t * 6);
  for (let bx = 0; bx < sw; bx += stripeW) {
    const odd = Math.floor((bx + scroll) / stripeW) % 2;
    const c = odd ? themed.stripeB : themed.stripeA;
    ink(c[0], c[1], c[2]).box(sx + bx, sy, Math.min(stripeW, sw - bx), sh);
  }
  // Each character: its own circus color + individual vertical bounce.
  if (!signLayout) return;
  let cx = signLayout.left;
  const baseY = sy + Math.round((sh - 16) / 2); // 16 ≈ unifont glyph height, vertically centered
  for (let i = 0; i < label.length; i++) {
    const ch = label[i];
    if (ch !== " ") {
      const bounce = Math.round(Math.sin(t * 2.6 + i * 0.7) * 2);
      const c = LAK_CIRCUS_COLS[i % LAK_CIRCUS_COLS.length];
      ink(20, 10, 6).write(ch, { x: cx + 1, y: baseY + bounce + 1 }, undefined, undefined, false, LAK_SIGN_FONT);
      ink(c[0], c[1], c[2]).write(ch, { x: cx, y: baseY + bounce }, undefined, undefined, false, LAK_SIGN_FONT);
    }
    cx += CHAR_W + signLayout.gap;
  }
}

// 📱 The top-right corner, right to left: the laklok.com QR on white paper,
// the ⚙ settings toggle, and — for a signed-in visitor — the envelope that
// opens `mail`, wearing a red count when letters are waiting.
function paintCorner($) {
  if (!lakQRCells) return;
  const { screen } = $;
  const qrBoxSize = lakQRCells.length + 2;
  const rightInset = 3; // Match the QR's 3px top inset (4px below at this height).
  const qrX = screen.width - qrBoxSize - rightInset;
  const qrY = Math.floor((LAK_TOP_MARGIN - qrBoxSize) / 2);
  paintQR($, lakQRCells, qrX, qrY);

  if (lakTV) { gearBox = null; mailBox = null; return; } // 📺 no controls on television
  const gx = qrX - GEAR - 4;
  const gy = qrY + Math.floor((qrBoxSize - GEAR) / 2);
  gearBox = paintGear($, gx, gy, settingsOpen);

  if (!mailCount || mailCount === "asking") { mailBox = null; return; }
  const unread = mailCount.unread || 0;
  const label = unread > 0 ? `${unread}` : null;
  // The badge overhangs the envelope's top-right corner, so the cluster is
  // the envelope plus that overhang.
  const cluster = ENVELOPE.w + (label ? badgeWidth(label) - 3 : 0);
  const ex = gearBox.x - 6 - cluster;
  const ey = qrY + Math.floor((qrBoxSize - ENVELOPE.h) / 2);
  mailBox = { x: ex - 4, y: ey - 6, w: cluster + 8, h: ENVELOPE.h + 12 };
  const { pen } = $;
  const hot = !!pen && pen.x >= mailBox.x && pen.x < mailBox.x + mailBox.w &&
    pen.y >= mailBox.y && pen.y < mailBox.y + mailBox.h;
  paintEnvelope($, ex, ey, { lit: unread > 0, hot });
  if (label) paintBadge($, ex + ENVELOPE.w - 3, ey - 5, label, hot);
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
  const themed = LAK_THEMES[lakTheme];
  chat.paint($, {
    otherChat: chatView(),
    hideChrome: true,
    hideInput: lakTV, // 📺 broadcast face drops the login/message footer
    topMargin: LAK_TOP_MARGIN, // Shorter top chrome panel than the default 42.
    attachAfterInput: true,
    inputPlaceholder: "Chat...",
    presenceOnlineOnly: true,
    // QR box + gear + breathing room, and the envelope when it's drawn.
    presenceRightInset: 50 + (mailBox ? mailBox.w + 2 : 0),
    // 🎪 Circus marquee as the header backdrop — fills the whole chrome panel,
    // painted under the online counter so the counter stays readable on top.
    paintHeader: (api, tm) => paintLaerKlokkenSign(api, tm),
    presenceTop: 24, // "N online" counter sits low in the header, over the marquee.
    theme: themed.chat,
  });

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
    needsPaint?.();
    return;
  }

  // 📬 The envelope is the door to mail.
  if (e.is("touch") && !settingsOpen && hit(mailBox)) {
    jump("mail");
    return;
  }

  if (settingsOpen && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    if (e.is("touch")) {
      const chip = settingsHits.find((h) => hit(h));
      if (chip) {
        const { type, value } = chip.action;
        if (type === "mode" && value === "vector") {
          jump("out:https://laklok.com/html/");
        } else if (type === "theme") {
          lakTheme = value;
          saveTema(store, value);
          if (value === "realtime") realtimeTick(); // catch up before first paint
          chat.refresh(client.system); // recolor cached message lines
          reportTema(net, value);
        } else if (type === "links") {
          lakLinksOnly = value;
          store["laklok:links"] = value;
          store.persist("laklok:links");
          chat.refresh(client.system); // relayout the filtered feed
        } else if (type === "lang") {
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

  chat.act($, chatView(), { allowDelete: true });
}

function sim($) {
  // 🌅 The ambient tema drifts a hair each minute; recolor when it does.
  if (lakTheme === "realtime" && realtimeTick()) chat.refresh(client.system);

  // 📬 Ask once whether there's mail waiting — two counts, not the inbox.
  // Asked from sim, not boot, because the signed-in user can settle a beat
  // after the piece does (the prompt curtain does the same).
  if (mailCount === null && $.user && !lakTV) {
    mailCount = "asking";
    $.net
      .userRequest("GET", "/api/mail?count=1")
      .then((res) => {
        mailCount = res?.status === 200 ? res : { unread: 0, total: 0 };
      })
      .catch(() => (mailCount = { unread: 0, total: 0 }));
  }

  chat.sim($);
}

function leave() {
  client.kill();
}

export { boot, paint, act, sim, leave };
