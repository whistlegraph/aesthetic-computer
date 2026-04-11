// tapes.mjs — browse + play MP4 tapes recorded on this device.
//
// Tapes are recorded by pressing PrintScreen anywhere in notepat (or
// any piece that doesn't eat the key). The PrtSc handler in
// fedac/native/src/ac-native.c drives the existing libavcodec
// recorder (recorder.c) to produce an MP4 at
// /mnt/tapes/YYYY.MM.DD.HH.MM.SS.mmm.mp4, then fires an async curl
// upload to the cloud via /api/presigned-upload-url/mp4/<slug>/user
// + /api/track-tape (ext=mp4), which extends the existing
// MongoDB `tapes` collection with a kind="mp4" + source="ac-native"
// record. Web AC tapes (ZIP) and ac-native tapes (MP4) coexist in
// the same collection — the extension + kind field disambiguates.
//
// This piece is the local browser for them. It lists /mnt/tapes/
// with timestamp, duration (from filesystem), and a simple
// keyboard-driven UI to play, delete, or jump out. Cloud browsing
// (handle-scoped listing via the tapes collection API) is a Phase 3
// addition — for now, anything that was uploaded is still viewable
// here locally since the MP4 lives on disk until you delete it.

// Each tape entry: { name, source, ...fields }
//   source: "local"  → has {path, size}
//   source: "cloud"  → has {code, slug, mp4Url, when, thumbnailUrl, kind}
let tapes = [];
let selection = 0;       // index in tapes
let message = "";
let messageFrame = 0;
let frame = 0;
let cloudFetching = false;
let cloudLoadedOnce = false;

function fmtBytes(n) {
  if (!Number.isFinite(n)) return "?";
  if (n < 1024) return `${n}B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)}K`;
  return `${(n / 1024 / 1024).toFixed(1)}M`;
}

function msg(t) { message = t; messageFrame = frame; }

function scanLocal(system) {
  const local = [];
  const listing = system?.listDir?.("/mnt/tapes");
  if (listing) {
    for (const f of listing) {
      if (f.name.endsWith(".mp4") && !f.name.startsWith(".")) {
        local.push({
          source: "local",
          name: f.name,
          path: "/mnt/tapes/" + f.name,
          size: f.size || 0,
        });
      }
    }
  }
  return local;
}

function mergeAndSort(local, cloud) {
  // Newest first. Local filenames are timestamp-slugs; cloud has `when` ISO.
  const all = [...local, ...cloud];
  all.sort((a, b) => {
    const ka = a.source === "local" ? a.name : (a.slug || a.code || "");
    const kb = b.source === "local" ? b.name : (b.slug || b.code || "");
    return kb.localeCompare(ka);
  });
  return all;
}

function scan(system) {
  tapes = mergeAndSort(scanLocal(system), []);
  if (selection >= tapes.length) selection = Math.max(0, tapes.length - 1);
  msg(tapes.length > 0 ? `${tapes.length} local tapes` : "no tapes — press PrtSc in notepat");
}

// Fetch the user's cloud tapes from the new user-tapes endpoint.
// Non-blocking: kicks off the curl fetch via system.fetch and merges
// when the response arrives. Re-calling during an in-flight fetch is
// a no-op.
function fetchCloudTapes(system, handle) {
  if (cloudFetching || !handle || !system?.fetch) return;
  cloudFetching = true;
  const url = `https://aesthetic.computer/api/user-tapes?handle=${encodeURIComponent(handle)}`;
  msg("fetching cloud tapes...");
  system.fetch(url, {}, (err, resp) => {
    cloudFetching = false;
    cloudLoadedOnce = true;
    if (err) {
      msg("cloud fetch failed: " + (err.message || err));
      return;
    }
    try {
      const data = typeof resp === "string" ? JSON.parse(resp) : resp;
      const cloud = (data?.tapes || []).map((t) => ({
        source: "cloud",
        name: t.slug + ".mp4",
        code: t.code,
        slug: t.slug,
        mp4Url: t.mp4Url,
        when: t.when,
        kind: t.kind || "zip",
        cloudSource: t.source,
      }));
      const local = scanLocal(system);
      tapes = mergeAndSort(local, cloud);
      if (selection >= tapes.length) selection = Math.max(0, tapes.length - 1);
      msg(`${local.length} local + ${cloud.length} cloud`);
    } catch (e) {
      msg("cloud parse failed");
    }
  });
}

function boot({ system, sound, handle }) {
  scan(system);
  sound?.speak?.(tapes.length > 0 ? `${tapes.length} tapes` : "no tapes yet");
  // Kick off a cloud fetch if we have a handle — merges on arrival.
  const h = typeof handle === "function" ? handle() : handle;
  if (h) fetchCloudTapes(system, (h || "").replace(/^@/, ""));
}

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    if (key === "arrowup" || key === "k") {
      if (tapes.length > 0) {
        selection = (selection - 1 + tapes.length) % tapes.length;
        msg(tapes[selection].name);
      }
      return;
    }
    if (key === "arrowdown" || key === "j") {
      if (tapes.length > 0) {
        selection = (selection + 1) % tapes.length;
        msg(tapes[selection].name);
      }
      return;
    }
    if (key === "r") {
      scan(system);
      const h = (e?.system?.handle?.() || "").replace(/^@/, "");
      if (h) fetchCloudTapes(system, h);
      return;
    }
    if (key === "enter" || key === " " || key === "space") {
      if (tapes.length > 0) {
        const t = tapes[selection];
        if (t.source === "local") {
          // Play via deck 0 — same mechanism the dj piece uses.
          const ok = sound?.deck?.load?.(0, t.path);
          if (ok) {
            sound.deck.play(0);
            msg("playing local " + t.name);
            sound?.speak?.("playing tape");
          } else {
            msg("failed to play " + t.name);
          }
        } else if (t.source === "cloud" && t.mp4Url) {
          // Cloud tapes need to be fetched to a local temp path first.
          // For MVP, just attempt to play directly if the deck supports
          // URL loading, otherwise instruct the user.
          const ok = sound?.deck?.load?.(0, t.mp4Url);
          if (ok) {
            sound.deck.play(0);
            msg("streaming " + t.slug);
            sound?.speak?.("streaming cloud tape");
          } else {
            msg("cloud streaming not supported yet — see " + t.mp4Url);
          }
        }
      }
      return;
    }
    if (key === "delete" || key === "backspace") {
      if (tapes.length > 0) {
        const t = tapes[selection];
        if (t.source === "local") {
          if (system?.deleteFile?.(t.path)) {
            msg("deleted " + t.name);
            scan(system);
          } else {
            msg("couldn't delete local file");
          }
        } else if (t.source === "cloud" && t.code && system?.fetch) {
          // Soft-delete on the server via POST /api/delete-tape with the
          // user's bearer token. ac-native's system.fetch automatically
          // attaches the Authorization header from /mnt/config.json.
          msg("nuking cloud tape " + t.code + "...");
          system.fetch(
            "https://aesthetic.computer/api/delete-tape",
            {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({ code: t.code }),
            },
            (err, resp) => {
              if (err) {
                msg("delete failed: " + (err.message || err));
                return;
              }
              msg("nuked " + t.code);
              // Remove from local list immediately
              tapes = tapes.filter((x) => !(x.source === "cloud" && x.code === t.code));
              if (selection >= tapes.length) selection = Math.max(0, tapes.length - 1);
            }
          );
        }
      }
      return;
    }
    if (key === "escape") {
      system?.jump?.("prompt");
      return;
    }
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  const w = screen.width;
  const h = screen.height;
  wipe(16, 14, 20);

  // Title
  ink(200, 220, 255);
  write("tapes", { x: 8, y: 8, size: 2, font: "matrix" });
  ink(120, 140, 170);
  write("/mnt/tapes  —  ↑↓ nav  enter play  r rescan  del delete  esc prompt",
        { x: 80, y: 14, size: 1, font: "font_1" });

  // Tape list
  const listY = 40;
  const rowH = 14;
  const visibleRows = Math.max(1, Math.floor((h - listY - 20) / rowH));
  const scrollStart = Math.max(0, selection - Math.floor(visibleRows / 2));
  const scrollEnd = Math.min(tapes.length, scrollStart + visibleRows);

  if (tapes.length === 0) {
    ink(180, 180, 200);
    write("No tapes recorded yet.", { x: 20, y: listY + 8, size: 1, font: "font_1" });
    ink(120, 140, 170);
    write("Press PrintScreen in notepat to record a tape.",
          { x: 20, y: listY + 24, size: 1, font: "font_1" });
  } else {
    for (let i = scrollStart; i < scrollEnd; i++) {
      const t = tapes[i];
      const y = listY + (i - scrollStart) * rowH;
      if (i === selection) {
        ink(40, 60, 100, 200);
        box(4, y - 2, w - 8, rowH, true);
      }
      // Source badge (☁ cloud / 📁 local — shown as small ascii tag)
      const badge = t.source === "cloud" ? "C" : "L";
      const badgeColor = t.source === "cloud" ? [120, 220, 180] : [220, 200, 120];
      ink(badgeColor[0], badgeColor[1], badgeColor[2]);
      write(badge, { x: 10, y: y + 2, size: 1, font: "font_1" });
      // Name
      if (i === selection) ink(255, 255, 255);
      else ink(200, 220, 240);
      write(t.name.replace(/\.mp4$/, ""), { x: 24, y: y + 2, size: 1, font: "font_1" });
      // Right meta: local shows size, cloud shows code
      ink(140, 160, 190);
      const meta = t.source === "local" ? fmtBytes(t.size) : ("!" + (t.code || "?"));
      const metaX = w - meta.length * 6 - 10;
      write(meta, { x: metaX, y: y + 2, size: 1, font: "font_1" });
    }
  }

  // Message line
  if (message && frame - messageFrame < 240) {
    const age = frame - messageFrame;
    const a = Math.max(80, 255 - Math.floor(age * 0.6));
    ink(255, 220, 140, a);
    write(message, { x: 8, y: h - 14, size: 1, font: "font_1" });
  }
}

function sim() {
  frame++;
}

export { boot, act, paint, sim };
