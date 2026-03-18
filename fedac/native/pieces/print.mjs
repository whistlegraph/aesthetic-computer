// print.mjs — Printer detection and test page printing
// Detects USB printers (including thermal printers), shows info,
// and can print ESC/POS test pages.
// Press 'p' to print test page, 'r' to rescan, escape to return.

let frame = 0;
let printers = [];
let scanning = false;
let scanFrame = 0;
let printStatus = ""; // "", "printing", "done", "error"
let printMsg = "";
let selectedIdx = 0;

function scanPrinters(system) {
  if (!system?.listPrinters) {
    printers = [];
    return;
  }
  scanning = true;
  scanFrame = frame;
  printers = system.listPrinters() || [];
  scanning = false;
}

// Build ESC/POS test page as byte array
function buildTestPage(printerName) {
  const bytes = [];
  const push = (b) => bytes.push(b);
  const pushBytes = (arr) => arr.forEach((b) => bytes.push(b));
  const pushStr = (s) => {
    for (let i = 0; i < s.length; i++) bytes.push(s.charCodeAt(i));
  };

  // ESC @ — Initialize printer
  pushBytes([0x1b, 0x40]);

  // ESC a 1 — Center alignment
  pushBytes([0x1b, 0x61, 0x01]);

  // ESC E 1 — Bold on
  pushBytes([0x1b, 0x45, 0x01]);

  // GS ! 0x11 — Double width + double height
  pushBytes([0x1d, 0x21, 0x11]);
  pushStr("AESTHETIC\n");
  pushStr("COMPUTER\n");

  // GS ! 0x00 — Normal size
  pushBytes([0x1d, 0x21, 0x00]);

  // ESC E 0 — Bold off
  pushBytes([0x1b, 0x45, 0x00]);

  pushStr("\n");

  // Divider line
  pushStr("--------------------------------\n");

  // ESC a 0 — Left alignment
  pushBytes([0x1b, 0x61, 0x00]);

  pushStr("Printer Test Page\n");
  pushStr("\n");

  if (printerName) {
    pushStr("Device: " + printerName + "\n");
  }

  const now = new Date();
  const ts =
    now.getFullYear() +
    "-" +
    String(now.getMonth() + 1).padStart(2, "0") +
    "-" +
    String(now.getDate()).padStart(2, "0") +
    " " +
    String(now.getHours()).padStart(2, "0") +
    ":" +
    String(now.getMinutes()).padStart(2, "0");
  pushStr("Time: " + ts + "\n");
  pushStr("\n");

  // Character set test
  pushStr("ASCII Test:\n");
  pushStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ\n");
  pushStr("abcdefghijklmnopqrstuvwxyz\n");
  pushStr("0123456789 !@#$%^&*()\n");
  pushStr("\n");

  // Barcode test — Code 39
  pushStr("Barcode:\n");
  // GS k 4 — Code39
  pushBytes([0x1d, 0x6b, 0x04]);
  pushStr("AC2026");
  push(0x00); // NUL terminator for barcode data
  pushStr("\n\n");

  // ESC a 1 — Center
  pushBytes([0x1b, 0x61, 0x01]);

  // Density test — varying shades using block chars
  pushStr("Density Test:\n");
  for (let i = 0; i < 32; i++) {
    push(i < 8 ? 0xb0 : i < 16 ? 0xb1 : i < 24 ? 0xb2 : 0xdb);
  }
  pushStr("\n\n");

  // Font size samples
  // GS ! 0x01 — Double width
  pushBytes([0x1d, 0x21, 0x01]);
  pushStr("Wide Text\n");

  // GS ! 0x10 — Double height
  pushBytes([0x1d, 0x21, 0x10]);
  pushStr("Tall Text\n");

  // GS ! 0x00 — Normal
  pushBytes([0x1d, 0x21, 0x00]);

  pushStr("\n");
  pushStr("--------------------------------\n");

  // ESC E 1 — Bold
  pushBytes([0x1b, 0x45, 0x01]);
  pushStr("aesthetic.computer\n");
  // ESC E 0 — Bold off
  pushBytes([0x1b, 0x45, 0x00]);

  pushStr("\n\n\n");

  // GS V 1 — Partial cut
  pushBytes([0x1d, 0x56, 0x01]);

  return bytes;
}

function boot({ system }) {
  scanPrinters(system);
}

function act({ event: e, system, needsPaint }) {
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }

  if (e.is("keyboard:down:r")) {
    scanPrinters(system);
    printStatus = "";
    printMsg = "";
    needsPaint?.();
    return;
  }

  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    if (selectedIdx > 0) selectedIdx--;
    needsPaint?.();
    return;
  }

  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    if (selectedIdx < printers.length - 1) selectedIdx++;
    needsPaint?.();
    return;
  }

  if (e.is("keyboard:down:p")) {
    if (printers.length === 0) {
      printStatus = "error";
      printMsg = "no printer connected";
      needsPaint?.();
      return;
    }

    const printer = printers[selectedIdx];
    if (!printer || !system?.printRaw) {
      printStatus = "error";
      printMsg = "print API unavailable";
      needsPaint?.();
      return;
    }

    printStatus = "printing";
    printMsg = "sending to " + (printer.name || printer.device);
    needsPaint?.();

    const pageData = buildTestPage(printer.name || printer.id);
    const ok = system.printRaw(printer.device, pageData);

    if (ok) {
      printStatus = "done";
      printMsg = "test page sent (" + pageData.length + " bytes)";
    } else {
      printStatus = "error";
      printMsg = "write failed — check connection";
    }
    needsPaint?.();
  }
}

function paint({ wipe, ink, box, line, write, screen, system }) {
  frame++;
  const T = __theme.update();
  const W = screen.width,
    H = screen.height;
  const pad = 6;
  const font = "6x10";
  const charW = 6;
  const lineH = 11;

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  let y = 4;
  ink(T.accent[0], T.accent[1], T.accent[2]);
  write("print", { x: pad, y, size: 1, font });
  y += lineH + 4;

  // Divider
  ink(T.border[0], T.border[1], T.border[2]);
  line(pad, y, W - pad, y);
  y += 6;

  // Printer count
  ink(T.fg, T.fg, T.fg);
  if (printers.length === 0) {
    write("no printers detected", { x: pad, y, size: 1, font });
    y += lineH + 2;
    ink(T.fgDim, T.fgDim, T.fgDim);
    write("connect a USB printer and press 'r'", {
      x: pad,
      y,
      size: 1,
      font,
    });
    y += lineH;
  } else {
    write(
      printers.length + " printer" + (printers.length > 1 ? "s" : "") + ":",
      { x: pad, y, size: 1, font },
    );
    y += lineH + 4;

    // List printers
    const maxC = Math.floor((W - pad * 2) / charW);
    for (let i = 0; i < printers.length; i++) {
      const p = printers[i];
      const selected = i === selectedIdx;

      // Selection highlight
      if (selected) {
        ink(T.accent[0], T.accent[1], T.accent[2], 30);
        box(pad - 2, y - 1, W - pad * 2 + 4, lineH * 3 + 4, true);
      }

      // Printer name
      const marker = selected ? "> " : "  ";
      ink(
        selected ? T.accent[0] : T.fg,
        selected ? T.accent[1] : T.fg,
        selected ? T.accent[2] : T.fg,
      );
      let label = marker + (p.name || p.id);
      write(label.slice(0, maxC), { x: pad, y, size: 1, font });
      y += lineH;

      // Vendor
      if (p.vendor) {
        ink(T.fgDim, T.fgDim, T.fgDim);
        write("  vendor: " + p.vendor, { x: pad, y, size: 1, font });
        y += lineH;
      }

      // Device path
      ink(T.fgMute, T.fgMute, T.fgMute);
      write("  " + p.device, { x: pad, y, size: 1, font });
      y += lineH + 4;
    }
  }

  // Print status
  y += 4;
  if (printStatus === "printing") {
    ink(T.warn[0], T.warn[1], T.warn[2]);
    write(printMsg, { x: pad, y, size: 1, font });
    y += lineH;
  } else if (printStatus === "done") {
    ink(T.ok[0], T.ok[1], T.ok[2]);
    write(printMsg, { x: pad, y, size: 1, font });
    y += lineH;
  } else if (printStatus === "error") {
    ink(T.err[0], T.err[1], T.err[2]);
    write(printMsg, { x: pad, y, size: 1, font });
    y += lineH;
  }

  // Footer
  ink(T.fgMute, T.fgMute, T.fgMute);
  const hint = "esc: back  r: rescan  p: test page";
  write(hint, { x: pad, y: H - 12, size: 1, font });

  // Arrow hint if multiple printers
  if (printers.length > 1) {
    write("up/down: select printer", { x: pad, y: H - 24, size: 1, font });
  }
}

export { boot, act, paint };
