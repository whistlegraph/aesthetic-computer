// list.mjs — Shows all available pieces and commands on the native system
// Press escape or backspace to return to prompt.

let frame = 0;
let scrollY = 0;

const PIECES = [
  { name: "notepat",       alias: "np",  desc: "synthesizer instrument with touch grid" },
  { name: "os",            alias: null,  desc: "system update panel (OTA flash)" },
  { name: "wifi",          alias: "net", desc: "network picker and saved credentials" },
  { name: "claude",        alias: "cl",  desc: "AI assistant (Claude Code)" },
  { name: "terminal",      alias: null,  desc: "PTY terminal emulator (sh, claude)" },
  { name: "geo",           alias: "location", desc: "IP-based geolocation" },
  { name: "chat",          alias: null,  desc: "real-time chat on aesthetic.computer" },
  { name: "laer-klokken",  alias: "lk",  desc: "clock room chat (warm theme)" },
  { name: "machine",       alias: null,  desc: "hardware & software info" },
  { name: "roz",           alias: null,  desc: "generative art viewer" },
  { name: "prompt",        alias: null,  desc: "command prompt (home)" },
  { name: "list",          alias: null,  desc: "this screen" },
];

const COMMANDS = [
  { name: "off",      alias: "shutdown", desc: "power off the machine" },
  { name: "reboot",   alias: null,      desc: "restart the system" },
  { name: "hi",       alias: "login",   desc: "show current logged-in user" },
  { name: "bye",      alias: "logout",  desc: "log out current user" },
  { name: "version",  alias: "ver",     desc: "show current OS version hash" },
  { name: "ssh",      alias: null,      desc: "start SSH server on port 22" },
  { name: "clear",    alias: "cls",     desc: "clear command history" },
  { name: "help",     alias: null,      desc: "show quick help" },
];

const CODES = [
  { name: "$roz", desc: "generative art pattern (KidLisp)" },
];

function act({ event: e, system }) {
  if (e.is("keyboard:down")) {
    const key = e.key;
    if (key === "escape" || key === "backspace") {
      system?.jump?.("prompt");
      return;
    }
    if (key === "arrowdown") scrollY += 14;
    if (key === "arrowup") scrollY = Math.max(0, scrollY - 14);
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  frame++;
  wipe(30, 15, 45);

  const W = screen.width;
  const H = screen.height;
  const font = "6x10";
  const charW = 6;
  const lineH = 13;
  const pad = 6;
  let y = pad - scrollY;

  // Title
  ink(200, 140, 220);
  write("ac/native commands", { x: pad, y, size: 1, font });
  y += lineH + 4;

  // Section: Pieces
  ink(120, 200, 140);
  write("pieces", { x: pad, y, size: 1, font });
  y += lineH;
  ink(60, 80, 70);
  line(pad, y - 2, W - pad, y - 2);

  for (const p of PIECES) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(220, 180, 255);
      write(p.name, { x: pad, y, size: 1, font });
      if (p.alias) {
        ink(120, 100, 140);
        write("/" + p.alias, { x: pad + p.name.length * charW + 2, y, size: 1, font });
      }
      ink(100, 100, 110);
      write(p.desc, { x: pad, y: y + 11, size: 1, font });
    }
    y += lineH * 2;
  }

  y += 6;

  // Section: Commands
  ink(140, 180, 220);
  write("commands", { x: pad, y, size: 1, font });
  y += lineH;
  ink(60, 70, 80);
  line(pad, y - 2, W - pad, y - 2);

  for (const c of COMMANDS) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(180, 200, 240);
      write(c.name, { x: pad, y, size: 1, font });
      if (c.alias) {
        ink(100, 110, 140);
        write("/" + c.alias, { x: pad + c.name.length * charW + 2, y, size: 1, font });
      }
      ink(100, 100, 110);
      write(c.desc, { x: pad, y: y + 11, size: 1, font });
    }
    y += lineH * 2;
  }

  y += 6;

  // Section: Code aliases
  ink(200, 180, 120);
  write("$codes", { x: pad, y, size: 1, font });
  y += lineH;
  ink(80, 70, 50);
  line(pad, y - 2, W - pad, y - 2);

  for (const c of CODES) {
    if (y > H + 20) break;
    if (y > -lineH) {
      ink(240, 200, 140);
      write(c.name, { x: pad, y, size: 1, font });
      ink(100, 100, 110);
      write(c.desc, { x: pad, y: y + 11, size: 1, font });
    }
    y += lineH * 2;
  }

  y += 8;

  // Footer hint
  if (y > -lineH && y < H + 20) {
    ink(80, 60, 100);
    write("anything else -> kidlisp", { x: pad, y, size: 1, font });
    y += lineH;
    ink(60, 50, 80);
    write("esc to return", { x: pad, y, size: 1, font });
  }
}

export { act, paint };
