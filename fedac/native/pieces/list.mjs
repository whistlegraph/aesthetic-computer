// list.mjs — Dense command reference with categories and scrolling
// Arrow keys scroll, escape returns to prompt.

let frame = 0;
let scrollY = 0;
let totalH = 0;

// ── Categories ──────────────────────────────────────────────
const MAIN = [
  { name: "notepat",  desc: "synthesizer instrument" },
];

const TOOLS = [
  { name: "os",       desc: "system updates (OTA)" },
  { name: "wifi",     desc: "network picker" },
  { name: "code",     desc: "AI assistant (Claude)" },
  { name: "terminal", desc: "shell (PTY)" },
  { name: "machine",  desc: "hardware info" },
  { name: "geo",      desc: "IP geolocation" },
  { name: "chat",     desc: "live chat room" },
  { name: "print",    desc: "printer / thermal" },
  { name: "list",     desc: "this screen" },
];

const COMMANDS = [
  { name: "off",     desc: "power off" },
  { name: "reboot",  desc: "restart" },
  { name: "hi",      desc: "current user" },
  { name: "bye",     desc: "log out" },
  { name: "version", desc: "OS hash" },
  { name: "ssh",     desc: "SSH server" },
  { name: "clear",   desc: "clear history" },
  { name: "help",    desc: "quick help" },
];

const CODES = [
  { name: "$roz", desc: "generative art" },
];

// Populated in boot — pieces not in MAIN or TOOLS
let UNSTABLE = [];
let LISP_PIECES = [];

function boot({ system }) {
  var known = {};
  MAIN.forEach(function(p) { known[p.name] = true; });
  TOOLS.forEach(function(p) { known[p.name] = true; });
  var skip = { prompt: 1, lisp: 1, cc: 1, "404": 1, error: 1 };
  var names = (system && system.listPieces ? system.listPieces() : []);
  UNSTABLE = [];
  LISP_PIECES = [];
  for (var i = 0; i < names.length; i++) {
    var n = names[i];
    // .lisp pieces go in their own section
    if (n.endsWith && n.endsWith(".lisp")) {
      LISP_PIECES.push({ name: n.replace(".lisp", ""), desc: "common lisp" });
    } else if (!known[n] && !skip[n]) {
      UNSTABLE.push({ name: n });
    }
  }
  UNSTABLE.sort(function(a, b) { return a.name < b.name ? -1 : 1; });
  LISP_PIECES.sort(function(a, b) { return a.name < b.name ? -1 : 1; });
}

function act({ event: e, system }) {
  if (e.is("keyboard:down")) {
    var key = e.key;
    if (key === "escape" || key === "backspace") {
      if (system && system.jump) system.jump("prompt");
      return;
    }
    if (key === "arrowdown" || key === "j") scrollY += 20;
    if (key === "arrowup" || key === "k") scrollY = Math.max(0, scrollY - 20);
    if (key === "pagedown" || key === " ") scrollY += 100;
    if (key === "pageup") scrollY = Math.max(0, scrollY - 100);
    if (key === "home") scrollY = 0;
    if (key === "end") scrollY = Math.max(0, totalH - 150);
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  frame++;
  var T = __theme.update();
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  var W = screen.width;
  var H = screen.height;
  var font = "6x10";
  var cW = 6; // char width
  var lH = 11; // line height (dense)
  var pad = 5;
  var col2 = 85; // description column start
  var y = pad - scrollY;

  // ── Header ──
  ink(T.accent[0], T.accent[1], T.accent[2]);
  write("ac/native", { x: pad, y: y, size: 1, font: "matrix" });
  ink(T.fgMute, T.fgMute, T.fgMute);
  write("arrows/jk scroll  esc back", { x: W - 160, y: y + 2, size: 1, font });
  y += 14;

  // ── Draw a section ──
  function section(title, items, titleColor, nameColor, twoCol) {
    // Title bar
    ink(titleColor[0], titleColor[1], titleColor[2]);
    write(title, { x: pad, y: y, size: 1, font });
    ink(T.border[0], T.border[1], T.border[2]);
    var tw = title.length * cW + pad + 4;
    if (y + lH > 0 && y < H) line(tw, y + 4, W - pad, y + 4);
    y += lH + 2;

    if (twoCol) {
      // Two-column: name + description on same line
      for (var i = 0; i < items.length; i++) {
        if (y > H + 10) { y += lH; continue; }
        if (y > -lH) {
          ink(nameColor[0], nameColor[1], nameColor[2]);
          write(items[i].name, { x: pad + 2, y: y, size: 1, font });
          if (items[i].desc) {
            ink(T.fgMute, T.fgMute, T.fgMute);
            write(items[i].desc, { x: col2, y: y, size: 1, font });
          }
        }
        y += lH;
      }
    } else {
      // Compact grid: names only, 3 columns
      var colW = Math.floor((W - pad * 2) / 3);
      var col = 0;
      var rowY = y;
      for (var i = 0; i < items.length; i++) {
        if (rowY > -lH && rowY < H + 10) {
          ink(nameColor[0], nameColor[1], nameColor[2]);
          write(items[i].name, { x: pad + 2 + col * colW, y: rowY, size: 1, font });
        }
        col++;
        if (col >= 3) { col = 0; rowY += lH; }
      }
      if (col > 0) rowY += lH;
      y = rowY;
    }
    y += 4;
  }

  // ── Sections ──
  section("instrument", MAIN, T.ok, [T.fg, T.fg, T.fg], true);
  section("tools", TOOLS, T.link, [T.fg - 10, T.fg, T.fg + 15], true);
  section("commands", COMMANDS, [T.fgDim, T.fgDim + 20, T.fgDim], [T.fgDim + 30, T.fgDim + 30, T.fgDim + 40], true);
  section("$codes", CODES, T.warn, T.warn, true);

  if (LISP_PIECES.length > 0) {
    section("lisp (" + LISP_PIECES.length + ")", LISP_PIECES,
      [180, 120, 220],
      [200, 150, 240],
      true);
  }

  if (UNSTABLE.length > 0) {
    section("unstable (" + UNSTABLE.length + ")", UNSTABLE,
      [T.fgMute, T.fgMute - 10, T.fgMute],
      [T.fgMute + 20, T.fgMute + 10, T.fgMute + 20],
      false);
  }

  // Footer
  if (y > -lH && y < H + 10) {
    ink(T.fgMute - 10, T.fgMute - 15, T.fgMute);
    write("anything else -> kidlisp", { x: pad, y: y, size: 1, font });
  }
  y += lH;

  totalH = y + scrollY;

  // Scrollbar
  if (totalH > H) {
    var barH = Math.max(10, Math.floor(H * H / totalH));
    var barY = Math.floor(scrollY / totalH * H);
    ink(T.border[0], T.border[1], T.border[2]);
    box(W - 2, barY, 2, barH, true);
  }
}

export { boot, act, paint };
