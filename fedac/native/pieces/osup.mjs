// osup — unattended OTA. The same lane as `os` (version → kernel →
// initramfs → flashUpdate → reboot) with every confirmation pre-agreed,
// for when the human is at another machine's keyboard and pushes this
// over LAN. Errors halt on screen; only a verified flash reboots.

const BASE = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/";
const VERSION_URL = BASE + "native-notepat-latest.version";
const KERNEL_URL = BASE + "native-notepat-latest.vmlinuz";
const INITRAMFS_URL = BASE + "native-notepat-latest.initramfs.cpio.gz";

let state = "version";
let remoteName = "";
let remoteSize = 0;
let log = [];
let frame = 0;
let flashedFrame = 0;
let lastPct = -1;

function say(m) { log.push(m); if (log.length > 26) log.shift(); }
function fail(m) { state = "error"; say("ERROR: " + m); }

function boot({ system }) {
  say("current: " + (system?.version || "unknown"));
  system?.fetch?.(VERSION_URL);
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape")) system?.jump?.("prompt");
}

function paint({ wipe, ink, write, screen, system }) {
  frame++;

  if (state === "version" && system?.fetchResult != null) {
    const lines = String(system.fetchResult).trim().split("\n");
    const head = (lines[0] || "").trim();
    remoteName = head.split(" ")[0] || "";
    remoteSize = parseInt(lines[1], 10) || 0;
    if (!remoteName) fail("bad version file");
    else if (system?.version?.startsWith(remoteName)) {
      state = "up-to-date";
      say("remote: " + head);
      say("already current");
    } else {
      say("remote: " + head);
      say("kernel → /tmp/vmlinuz.new");
      state = "kernel";
      system?.fetchBinary?.(KERNEL_URL, "/tmp/vmlinuz.new", remoteSize || 93_000_000);
    }
  }

  if (state === "kernel" || state === "initramfs") {
    const pct = Math.round((system?.fetchBinaryProgress ?? 0) * 100);
    if (pct !== lastPct && pct % 10 === 0) { say(state + " " + pct + "%"); lastPct = pct; }
    if (system?.fetchBinaryDone) {
      if (!system?.fetchBinaryOk) {
        fail(state + " download failed");
      } else if (state === "kernel") {
        const got = system?.fileSizeBytes?.("/tmp/vmlinuz.new");
        if (remoteSize > 0 && typeof got === "number" && got !== remoteSize) {
          fail("kernel truncated (" + got + " vs " + remoteSize + ")");
        } else {
          say("initramfs → /tmp/initramfs.cpio.gz.new");
          state = "initramfs";
          lastPct = -1;
          system?.fetchBinary?.(INITRAMFS_URL, "/tmp/initramfs.cpio.gz.new", 336_000_000);
        }
      } else {
        const got = system?.fileSizeBytes?.("/tmp/initramfs.cpio.gz.new");
        if (typeof got === "number" && (got < 50 * 1048576 || got > 600 * 1048576)) {
          fail("initramfs size off (" + got + ")");
        } else {
          say("flashing boot device…");
          state = "flashing";
          system?.flashUpdate?.("/tmp/vmlinuz.new", null, "/tmp/initramfs.cpio.gz.new");
        }
      }
    }
  }

  if (state === "flashing" && system?.flashDone) {
    if (system?.flashOk) {
      const mb = ((system?.flashVerifiedBytes ?? 0) / 1048576).toFixed(1);
      say("verified " + mb + "MB — rebooting in 3s");
      state = "reboot";
      flashedFrame = frame;
    } else {
      fail("flash verify failed — NOT rebooting");
      for (const line of system?.flashLog || []) say("[c] " + line);
    }
  }

  if (state === "reboot" && frame - flashedFrame > 180) system?.reboot?.();

  wipe(10, 10, 14);
  ink(120, 255, 160);
  write("osup → " + (remoteName || "…"), { x: 8, y: 8, size: 1 });
  ink(200, 200, 210);
  log.forEach((m, i) => write(m, { x: 8, y: 24 + i * 12, size: 1 }));
}

function sim() {}

export { boot, paint, act, sim };
