// power.mjs — USB-C power role management
// Shows all Type-C ports with current power/data roles.
// Press Enter/Space to toggle the selected port between source and sink.
// Press escape to return.

let frame = 0;
let selectedIdx = 0;
let swapResult = null; // null, "ok", or "fail"
let swapFrame = 0;

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }

  const ports = system?.typec || [];
  if (ports.length === 0) return;

  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    selectedIdx = Math.max(0, selectedIdx - 1);
  }
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    selectedIdx = Math.min(ports.length - 1, selectedIdx + 1);
  }

  // Toggle power role
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return") || e.is("keyboard:down:space")) {
    const port = ports[selectedIdx];
    if (port && port.canSwap) {
      const newRole = port.powerRole === "source" ? "sink" : "source";
      const ok = system?.setPowerRole?.(port.port, newRole);
      swapResult = ok ? "ok" : "fail";
      swapFrame = frame;
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, system }) {
  frame++;
  const w = screen.width, h = screen.height;
  const pad = 6;
  const font = "6x10";
  const charW = 6;
  const lineH = 12;

  wipe(12, 10, 18);

  // Title
  ink(160, 170, 200);
  write("power", { x: pad, y: 4, size: 1, font });

  // Divider
  ink(40, 45, 55);
  line(pad, 15, w - pad, 15);

  let y = 20;

  // Battery summary
  const bat = system?.battery;
  if (bat && bat.percent >= 0) {
    const charging = bat.charging;
    ink(charging ? 80 : 180, charging ? 220 : 180, charging ? 120 : 80);
    const batLabel = bat.percent + "% " + bat.status;
    write(batLabel, { x: pad, y, size: 1, font });
    if (bat.minutesLeft > 0) {
      ink(100, 110, 130);
      const hrs = Math.floor(bat.minutesLeft / 60);
      const mins = bat.minutesLeft % 60;
      write("  " + hrs + "h" + mins + "m left", { x: pad + batLabel.length * charW, y, size: 1, font });
    }
    y += lineH + 4;
  }

  // USB-C ports header
  ink(100, 140, 180);
  write("usb-c type-c ports", { x: pad, y, size: 1, font });
  y += lineH + 2;

  const ports = system?.typec || [];

  if (ports.length === 0) {
    ink(80, 80, 100);
    write("no type-c ports detected", { x: pad, y, size: 1, font });
    y += lineH;
    ink(60, 65, 75);
    write("(needs CONFIG_TYPEC in kernel)", { x: pad, y, size: 1, font });
    y += lineH * 2;
  }

  for (let i = 0; i < ports.length; i++) {
    const p = ports[i];
    const sel = i === selectedIdx;
    const rowY = y;

    // Selection indicator
    if (sel) {
      ink(30, 35, 50);
      box(pad - 2, rowY - 1, w - pad * 2 + 4, lineH * 3 + 4, true);
    }

    // Port name
    ink(sel ? 200 : 120, sel ? 210 : 130, sel ? 230 : 150);
    write((sel ? "> " : "  ") + p.port, { x: pad, y: rowY, size: 1, font });

    // Power role
    const isSource = p.powerRole === "source";
    ink(isSource ? 255 : 80, isSource ? 180 : 200, isSource ? 60 : 255);
    write("power: " + p.powerRole, { x: pad + 14 * charW, y: rowY, size: 1, font });

    // Data role
    ink(140, 150, 170);
    write("data: " + p.dataRole, { x: pad + 4, y: rowY + lineH, size: 1, font });

    // Swap capability
    if (p.canSwap) {
      ink(80, 200, 120);
      write("swappable", { x: pad + 18 * charW, y: rowY + lineH, size: 1, font });
    } else {
      ink(100, 70, 70);
      write("fixed", { x: pad + 18 * charW, y: rowY + lineH, size: 1, font });
    }

    // Action hint for selected
    if (sel && p.canSwap) {
      const target = isSource ? "sink" : "source";
      const pulse = Math.floor(140 + 60 * Math.sin(frame * 0.08));
      ink(pulse, pulse, 200);
      write("enter: swap to " + target, { x: pad + 4, y: rowY + lineH * 2, size: 1, font });
    } else if (sel && !p.canSwap) {
      ink(80, 70, 70);
      write("hardware does not support swap", { x: pad + 4, y: rowY + lineH * 2, size: 1, font });
    }

    y += lineH * 3 + 6;
  }

  // Swap result flash
  if (swapResult && frame - swapFrame < 120) {
    const alpha = Math.max(0, 1 - (frame - swapFrame) / 120);
    const a = Math.floor(255 * alpha);
    if (swapResult === "ok") {
      ink(80, 255, 120, a);
      write("power role swapped!", { x: pad, y, size: 1, font });
    } else {
      ink(255, 80, 80, a);
      write("swap failed (check permissions/hardware)", { x: pad, y, size: 1, font });
    }
    y += lineH + 4;
  }

  // Footer
  ink(60, 65, 80);
  write("esc: back", { x: pad, y: h - 12, size: 1, font });
  if (ports.length > 1) {
    write("arrows: select port", { x: pad + 12 * charW, y: h - 12, size: 1, font });
  }
}

function sim() {}

export { act, paint, sim };
