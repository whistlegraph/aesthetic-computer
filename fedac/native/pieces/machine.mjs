// machine.mjs — Hardware & software info + BPF trace reader
// Left column: hardware (model, CPU, RAM, battery, devices)
// Right column: software (OS version, wifi, uptime, governor)
// Bottom: BPF trace circular buffer (perf CSV + trace files)
// Uses the system prompt theme with dark/light mode support.
// Press escape to return. Press 't' to toggle trace view.

let frame = 0;
let showTrace = false;
let traceLines = [];      // Circular buffer of recent trace/perf lines
let traceReadFrame = 0;   // Last frame we polled trace files
let perfChunkSeq = -1;    // Last perf chunk sequence we read
let theme = "default";
let isDark = true;

// Theme palettes (matching prompt.mjs scheme structure)
const themes = {
  default: {
    dark:  { bg: [70, 50, 100], fg: [255, 255, 255], dim: [160, 170, 200], accent: [200, 30, 100], section: [100, 140, 180], section2: [140, 120, 180], bar: [60, 140, 80], barBg: [30, 20, 50], divider: [80, 60, 120], footer: [120, 100, 160] },
    light: { bg: [252, 247, 197], fg: [40, 30, 90], dim: [80, 70, 120], accent: [56, 122, 223], section: [40, 80, 140], section2: [100, 60, 140], bar: [56, 122, 80], barBg: [220, 215, 170], divider: [200, 190, 140], footer: [120, 110, 140] },
  },
  serious: {
    dark:  { bg: [0, 0, 0], fg: [255, 255, 255], dim: [160, 160, 160], accent: [128, 128, 128], section: [200, 200, 200], section2: [180, 180, 180], bar: [180, 180, 180], barBg: [40, 40, 40], divider: [60, 60, 60], footer: [100, 100, 100] },
    light: { bg: [255, 255, 255], fg: [0, 0, 0], dim: [80, 80, 80], accent: [128, 128, 128], section: [40, 40, 40], section2: [60, 60, 60], bar: [60, 60, 60], barBg: [220, 220, 220], divider: [200, 200, 200], footer: [140, 140, 140] },
  },
  neo: {
    dark:  { bg: [0, 0, 0], fg: [0, 255, 0], dim: [0, 160, 0], accent: [0, 100, 255], section: [0, 200, 0], section2: [0, 180, 100], bar: [0, 255, 0], barBg: [0, 30, 0], divider: [0, 60, 0], footer: [0, 120, 0] },
    light: { bg: [0, 255, 0], fg: [0, 0, 0], dim: [0, 80, 0], accent: [0, 0, 180], section: [0, 60, 0], section2: [0, 80, 40], bar: [0, 80, 0], barBg: [0, 210, 0], divider: [0, 180, 0], footer: [0, 100, 0] },
  },
};

function c(role) {
  const t = themes[theme] || themes.default;
  const mode = isDark ? t.dark : t.light;
  return mode[role] || mode.fg;
}

// Read the latest perf CSV chunk and/or trace files into the circular buffer.
function pollTraces(system) {
  if (!system?.readFile) return;

  // Poll every 60 frames (~1 second)
  if (frame - traceReadFrame < 60) return;
  traceReadFrame = frame;

  // Try to find the latest perf chunk
  for (let seq = perfChunkSeq + 1; seq < perfChunkSeq + 3; seq++) {
    const s = seq < 0 ? 0 : seq;
    const path = `/mnt/perf/${String(s).padStart(4, "0")}.csv`;
    const data = system.readFile(path, 10); // last 10 lines
    if (data) {
      perfChunkSeq = s;
      const lines = data.split("\n").filter(l => l && !l.startsWith("frame,"));
      for (const l of lines) pushTrace("perf", l);
    }
  }

  // Read BPF trace files if they exist
  const traceFiles = ["syscall", "frame", "alsa", "input", "alloc"];
  for (const name of traceFiles) {
    const data = system.readFile(`/mnt/trace/${name}.csv`, 5);
    if (data) {
      const lines = data.split("\n").filter(l => l && !l.startsWith("timestamp,"));
      for (const l of lines) pushTrace(name, l);
    }
  }
}

const TRACE_MAX = 32; // Circular buffer capacity
function pushTrace(source, line) {
  const entry = `[${source}] ${line.slice(0, 80)}`;
  // Dedup: don't push if identical to last entry
  if (traceLines.length > 0 && traceLines[traceLines.length - 1] === entry) return;
  traceLines.push(entry);
  if (traceLines.length > TRACE_MAX) traceLines.shift();
}

function boot({ store, dark }) {
  if (store?.["prompt:theme"]) theme = store["prompt:theme"];
  isDark = dark !== false;
}

function act({ event: e, system, store, dark, needsPaint }) {
  isDark = dark !== false;
  if (store?.["prompt:theme"]) theme = store["prompt:theme"];

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
  }
  if (e.is("keyboard:down:t")) {
    showTrace = !showTrace;
    needsPaint?.();
  }
}

function paint({ wipe, ink, box, line, write, screen, system, wifi }) {
  frame++;
  const w = screen.width, h = screen.height;
  const pad = 6;
  const font = "6x10";
  const charW = 6;
  const lineH = 11;

  // Poll trace data each frame (internally throttled)
  pollTraces(system);

  wipe(...c("bg"));

  // Title
  ink(...c("section"));
  write("machine", { x: pad, y: 4, size: 1, font });

  // Divider under title
  ink(...c("divider"));
  line(pad, 15, w - pad, 15);

  // Reserve space for trace view at bottom
  const traceH = showTrace ? Math.min(TRACE_MAX * (lineH - 1) + 20, Math.floor(h * 0.4)) : 0;
  const contentH = h - traceH;

  // Two-column layout
  const wide = w > 200;
  const colW = wide ? Math.floor((w - pad * 3) / 2) : w - pad * 2;
  const col2X = wide ? pad + colW + pad : pad;

  // --- LEFT: HARDWARE ---
  let ly = 20;
  ink(...c("section"));
  write("hardware", { x: pad, y: ly, size: 1, font });
  ly += lineH + 2;

  const hw = system?.hw;
  const bat = system?.battery;

  if (hw?.model && hw.model !== "unknown") {
    ink(...c("fg"));
    let label = hw.vendor && hw.vendor !== "unknown"
      ? hw.vendor.replace("LENOVO", "Lenovo") + " " + hw.model
      : hw.model;
    const maxC = Math.floor(colW / charW);
    write(label.slice(0, maxC), { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  if (hw?.cpu && hw.cpu !== "unknown") {
    ink(...c("dim"));
    let cpuShort = hw.cpu
      .replace("Intel(R) Core(TM) ", "")
      .replace(" CPU", "")
      .replace(/ +/g, " ");
    const maxC = Math.floor(colW / charW);
    write(cpuShort.slice(0, maxC), { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  if (hw?.cores > 0) {
    ink(...c("dim"));
    write(hw.cores + " cores", { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  if (hw?.ramTotalMB > 0) {
    const usedMB = hw.ramTotalMB - (hw.ramAvailMB || 0);
    const pct = Math.round((usedMB / hw.ramTotalMB) * 100);
    ink(...c("dim"));
    write(`ram ${usedMB}/${hw.ramTotalMB}MB (${pct}%)`, { x: pad, y: ly, size: 1, font });
    ly += lineH;

    const barW = Math.min(colW, 100);
    const fill = Math.round(barW * (usedMB / hw.ramTotalMB));
    ink(...c("barBg"));
    box(pad, ly, barW, 5, true);
    const barCol = pct > 80 ? [220, 80, 80] : c("bar");
    ink(...barCol);
    box(pad, ly, fill, 5, true);
    ly += 9;
  }

  if (bat?.percent >= 0) {
    const charging = bat.charging ? "+" : "";
    const col = bat.percent < 20 ? [220, 80, 80] : bat.charging ? [80, 200, 120] : c("dim");
    ink(...col);
    let batStr = `bat ${charging}${bat.percent}%`;
    if (bat.minutesLeft > 0) batStr += ` ~${bat.minutesLeft}m`;
    write(batStr, { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  const devs = hw?.devices;
  if (devs && devs.length > 0 && ly < contentH - 30) {
    ly += 4;
    ink(...c("dim"));
    write("devices", { x: pad, y: ly, size: 1, font });
    ly += lineH;

    const devMaxC = Math.floor(colW / charW);
    const maxDevs = Math.floor((contentH - ly - 14) / (lineH - 1));
    const visible = Math.min(devs.length, maxDevs);
    let scrollOff = 0;
    if (devs.length > visible) {
      scrollOff = Math.floor(frame * 0.02) % devs.length;
    }

    for (let slot = 0; slot < visible; slot++) {
      const i = (slot + scrollOff) % devs.length;
      const d = devs[i];
      const icon = d.type === "usb" ? "U" : d.type === "input" ? "I"
                 : d.type === "camera" ? "C" : d.type === "audio" ? "A"
                 : d.type === "disk" ? "D" : d.type === "display" ? "M" : "?";
      let label = `${icon} ${d.name || d.id}`;
      if (d.type === "disk" && d.sizeGB > 0) label += ` ${d.sizeGB}GB`;
      if (d.type === "disk" && d.removable) label += " *";
      if (d.type === "display") label += d.connected ? " on" : " off";

      const isActive = (d.type === "display" && d.connected);
      ink(...(isActive ? c("bar") : c("dim")));
      write(label.slice(0, devMaxC), { x: pad, y: ly, size: 1, font });
      ly += lineH - 1;
    }
  }

  // --- RIGHT: SOFTWARE ---
  let ry = wide ? 20 : ly + 8;
  const sx = wide ? col2X : pad;

  ink(...c("section2"));
  write("software", { x: sx, y: ry, size: 1, font });
  ry += lineH + 2;

  const ver = system?.version || "unknown";
  ink(...c("fg"));
  const verMaxC = Math.floor(colW / charW);
  write(ver.slice(0, verMaxC), { x: sx, y: ry, size: 1, font });
  ry += lineH;

  const handle = system?.config?.handle;
  if (handle) {
    ink(...c("accent"));
    write("@" + handle, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  const piece = system?.config?.piece || "notepat";
  ink(...c("dim"));
  write("boot: " + piece, { x: sx, y: ry, size: 1, font });
  ry += lineH;

  if (wifi) {
    if (wifi.connected) {
      ink(...c("bar"));
      write("wifi: " + (wifi.ssid || "connected"), { x: sx, y: ry, size: 1, font });
      ry += lineH;
      if (wifi.ip) {
        ink(...c("dim"));
        write(wifi.ip, { x: sx, y: ry, size: 1, font });
        ry += lineH;
      }
    } else {
      ink(160, 100, 80);
      write("wifi: " + (wifi.status || "off"), { x: sx, y: ry, size: 1, font });
      ry += lineH;
    }
  }

  if (hw?.load1 !== undefined) {
    ink(...c("dim"));
    write(`load ${hw.load1.toFixed(2)} ${(hw.load5 || 0).toFixed(2)} ${(hw.load15 || 0).toFixed(2)}`, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  if (hw?.processes > 0) {
    ink(...c("dim"));
    write(hw.processes + " processes", { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  if (hw?.governor) {
    ink(...c("dim"));
    write("gov: " + hw.governor, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  if (system?.bootDevice) {
    ink(...c("dim"));
    write("boot dev: " + system.bootDevice, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // --- BOTTOM: BPF TRACE BUFFER ---
  if (showTrace && traceH > 0) {
    const ty = contentH;

    ink(...c("barBg"));
    box(0, ty, w, traceH, true);

    ink(...c("divider"));
    line(pad, ty + 1, w - pad, ty + 1);

    ink(...c("accent"));
    write("trace buffer", { x: pad, y: ty + 4, size: 1, font });
    ink(...c("dim"));
    write(`${traceLines.length}/${TRACE_MAX}`, { x: pad + 80, y: ty + 4, size: 1, font });

    const maxVisLines = Math.floor((traceH - 18) / (lineH - 2));
    const startIdx = Math.max(0, traceLines.length - maxVisLines);
    let tly = ty + 16;
    const traceMaxC = Math.floor((w - pad * 2) / charW);

    for (let i = startIdx; i < traceLines.length; i++) {
      const entry = traceLines[i];
      if (entry.startsWith("[perf]"))         ink(...c("dim"));
      else if (entry.startsWith("[frame]"))   ink(...c("accent"));
      else if (entry.startsWith("[alsa]"))    ink(220, 80, 80);
      else if (entry.startsWith("[input]"))   ink(...c("bar"));
      else if (entry.startsWith("[syscall]")) ink(...c("section2"));
      else if (entry.startsWith("[alloc]"))   ink(...c("section"));
      else ink(...c("dim"));

      write(entry.slice(0, traceMaxC), { x: pad, y: tly, size: 1, font });
      tly += lineH - 2;
    }

    if (traceLines.length === 0) {
      ink(...c("dim"));
      write("no trace data (run ac-trace to start)", { x: pad, y: tly, size: 1, font });
    }
  }

  // Footer
  ink(...c("footer"));
  const hint = showTrace ? "esc: back  t: hide trace" : "esc: back  t: trace";
  write(hint, { x: pad, y: h - 12, size: 1, font });
}

export { boot, act, paint };
