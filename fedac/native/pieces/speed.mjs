// speed.mjs — disk / USB write speed test
//
// Lists every block device the kernel knows about with its size, model,
// and removable flag, then runs a sequential write benchmark against
// three mount points (USB /mnt, internal /tmp/hd if mounted, tmpfs
// /tmp). Reports MB/s for each, plus free-space usage so you can see at
// a glance where the bottleneck is.
//
// Controls:
//   space / enter — re-run the benchmark
//   esc           — back to prompt

let frame = 0;
let devices = [];        // [{name, sizeBytes, removable, model, vendor}]
let targets = [];        // [{label, path, info, result}]
let running = false;     // true while a run is in progress
let status = "press SPACE to test";

function fmtBytes(n) {
  if (!Number.isFinite(n) || n <= 0) return "—";
  const units = ["B", "K", "M", "G", "T"];
  let i = 0;
  while (n >= 1024 && i < units.length - 1) { n /= 1024; i++; }
  return n.toFixed(n < 10 ? 1 : 0) + units[i];
}

function fmtMBps(mbps) {
  if (!Number.isFinite(mbps)) return "—";
  return mbps.toFixed(mbps < 10 ? 2 : 1) + " MB/s";
}

function scan(system) {
  devices = system?.blockDevices?.() || [];
  const candidates = [
    { label: "usb  /mnt     (boot media)", path: "/mnt" },
    { label: "hdd  /tmp/hd  (nvme/sata)",  path: "/tmp/hd" },
    { label: "ram  /tmp     (tmpfs)",      path: "/tmp" },
  ];
  targets = candidates.map((c) => {
    const info = system?.diskInfo?.(c.path);
    return { ...c, info, result: null };
  });
}

function boot({ system, wipe }) {
  wipe(14, 16, 24);
  scan(system);
}

// Build a printable test buffer of roughly `bytes` bytes. We repeat a
// 1-KiB chunk so the compiler / QuickJS string interning doesn't fold
// it into a single allocation.
function makeBuffer(bytes) {
  const kChunk = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890-+" .repeat(16); // 1024
  const n = Math.max(1, Math.ceil(bytes / 1024));
  return kChunk.repeat(n);
}

function runOne(system, target, sizeBytes) {
  if (!target.info) return { error: "unmounted", mbps: 0, bytes: 0, ms: 0 };
  // Skip if the target's free space is too small
  if (target.info.available < sizeBytes + 1024 * 1024) {
    return { error: "low space", mbps: 0, bytes: 0, ms: 0 };
  }
  const path = `${target.path}/.speedtest-${sizeBytes}.bin`;
  const buf = makeBuffer(sizeBytes);
  const t0 = Date.now();
  const ok = system?.writeFile?.(path, buf);
  const dt = Math.max(1, Date.now() - t0);
  // Clean up so we don't leave trash behind
  system?.deleteFile?.(path);
  if (!ok) return { error: "write failed", mbps: 0, bytes: 0, ms: dt };
  const mbps = (sizeBytes / (1024 * 1024)) / (dt / 1000);
  return { mbps, bytes: sizeBytes, ms: dt };
}

async function runAll(system) {
  if (running) return;
  running = true;
  status = "running...";
  scan(system);
  // Test multiple sizes and take the best (warmup effect)
  const sizes = [1 * 1024 * 1024, 4 * 1024 * 1024, 16 * 1024 * 1024];
  for (const t of targets) {
    if (!t.info) { t.result = { error: "unmounted" }; continue; }
    let best = { mbps: 0, bytes: 0, ms: 0 };
    for (const s of sizes) {
      const r = runOne(system, t, s);
      if (r.error) { best = r; break; }
      if (r.mbps > best.mbps) best = r;
    }
    t.result = best;
    status = "running " + t.label.split(" ")[0] + "...";
  }
  status = "done — SPACE to re-run";
  running = false;
}

function act({ event: e, system }) {
  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (key === "escape") { system?.jump?.("prompt"); return; }
    if (key === "enter" || key === " " || key === "space" || key === "r") {
      runAll(system);
      return;
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, system }) {
  const w = screen.width;
  const h = screen.height;
  wipe(14, 16, 24);

  // === Title ===
  ink(200, 220, 255);
  write("speed", { x: 8, y: 8, size: 2, font: "matrix" });
  ink(120, 140, 170);
  write("disk + usb write test — space to run, esc prompt",
        { x: 80, y: 14, size: 1, font: "font_1" });

  // === Block devices list (top third) ===
  let y = 36;
  ink(160, 210, 255);
  write("block devices", { x: 8, y, size: 1, font: "font_1" });
  y += 12;
  if (devices.length === 0) {
    ink(140, 140, 160);
    write("(no devices)", { x: 16, y, size: 1, font: "font_1" });
    y += 10;
  } else {
    for (const d of devices) {
      // Row background
      const rem = d.removable;
      ink(rem ? 50 : 30, rem ? 55 : 38, rem ? 35 : 46, 200);
      box(8, y - 1, w - 16, 10, true);
      // Name + removable badge
      ink(rem ? 255 : 180, rem ? 210 : 210, rem ? 100 : 240);
      const nameCol = `${d.name}${rem ? "  (removable)" : ""}`;
      write(nameCol, { x: 12, y, size: 1, font: "font_1" });
      // Size
      const sz = fmtBytes(d.sizeBytes);
      ink(200, 220, 240);
      write(sz, { x: 160, y, size: 1, font: "font_1" });
      // Model
      const model = ((d.vendor || "") + " " + (d.model || "")).trim() || "unknown";
      ink(140, 160, 190);
      const modelX = 220;
      const modelMax = Math.floor((w - modelX - 8) / 6);
      const modelTrim = model.length > modelMax ? model.slice(0, modelMax - 1) + "." : model;
      write(modelTrim, { x: modelX, y, size: 1, font: "font_1" });
      y += 11;
    }
  }

  // === Benchmarks (middle) ===
  y += 6;
  ink(160, 255, 210);
  write("write speed", { x: 8, y, size: 1, font: "font_1" });
  y += 12;

  for (const t of targets) {
    ink(30, 40, 50, 200);
    box(8, y - 1, w - 16, 22, true);
    // Label
    ink(220, 230, 250);
    write(t.label, { x: 12, y, size: 1, font: "font_1" });
    // Info row
    if (t.info) {
      const used = t.info.total - t.info.available;
      const usedStr = `${fmtBytes(used)} / ${fmtBytes(t.info.total)}`;
      ink(140, 160, 190);
      write(usedStr, { x: 12, y: y + 11, size: 1, font: "font_1" });
      // Bar
      const barX = 180;
      const barW = w - barX - 140;
      const pct = Math.max(0, Math.min(1, used / Math.max(1, t.info.total)));
      ink(40, 50, 65);
      box(barX, y + 12, barW, 6, true);
      ink(120, 200, 240);
      box(barX, y + 12, Math.max(1, Math.floor(barW * pct)), 6, true);
    } else {
      ink(255, 120, 100);
      write("not mounted", { x: 12, y: y + 11, size: 1, font: "font_1" });
    }
    // Result
    const r = t.result;
    if (r) {
      if (r.error) {
        ink(255, 140, 120);
        write(r.error, { x: w - 128, y: y + 5, size: 1, font: "font_1" });
      } else {
        // Color by speed: green > 50, amber > 10, red <
        const mbps = r.mbps;
        const [cr, cg, cb] = mbps > 50 ? [140, 240, 160]
                          : mbps > 10 ? [240, 220, 140]
                          : [240, 160, 140];
        ink(cr, cg, cb);
        write(fmtMBps(mbps), { x: w - 128, y: y + 5, size: 1, font: "font_1" });
      }
    } else {
      ink(120, 140, 170);
      write("—", { x: w - 128, y: y + 5, size: 1, font: "font_1" });
    }
    y += 24;
  }

  // === Status bar ===
  ink(dark() ? 40 : 220, dark() ? 48 : 225, dark() ? 60 : 235);
  box(0, h - 14, w, 14, true);
  ink(running ? 255 : 180, running ? 220 : 200, running ? 100 : 180);
  write(status, { x: 8, y: h - 12, size: 1, font: "font_1" });
}

function dark() { return true; } // always dark for this piece

function sim() { frame++; }

export { boot, act, paint, sim };
