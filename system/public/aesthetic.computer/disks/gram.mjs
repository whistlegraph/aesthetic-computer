// Gram, 2026.8.21
// A handheld vertical feed of !tapes — swipe to surf, scrub sideways,
// tap to pause, heart what you love. (The social sibling of `tv`.)

/* 📝 Notes
  MVP of the gram proposal: streaming MP4 tapes through the bios singleton
  (`tape:play-mp4`), swapped on swipe-commit with each tape's baked
  thumbnail painted as an instant poster frame while the next video spins
  up. Feed comes from /api/tv?types=tape; hearts are the generic
  /api/chat-heart {type, for} toggle.

  `tv.mjs` remains the lean-back/ambient sibling — its gesture-commitment
  core (first 10px decides scrub-vs-scroll) is forked here, but the ZIP
  frame-tape engine is not: every feed item streams byte-ranged MP4 from
  Spaces instead of downloading a frames ZIP.

  Next (with the proposal): a bios <video> pool for gapless swaps, live
  $kidlisp + painting interleave via the sprinkle mix, cursor pagination.
*/

import { createScrubber } from "./common/scrub.mjs";

const SPACES = "https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com";

let tapes = [];
let loadingFeed = true;
let feedError = null;

// Feed position — displayIndex is fractional during drags and transitions.
let currentIndex = 0;
let targetIndex = 0;
let displayIndex = 0;
let isTransitioning = false;
let lastSwipeTime = 0;
const swipeThreshold = 30;
const gestureThreshold = 10; // Pixels before committing to scrub or scroll.

// Gesture state — the first 10px of movement decides the mode.
let gestureMode = null; // null | "scrub" | "scroll"
let gestureStartX = 0;
let gestureStartY = 0;

// Physics scrubber (same lazy-needle feel as tv/video).
let scrubber = createScrubber();
let wasPlayingBeforeScrub = false;
let lastSeekProgress = 0;
const seekThreshold = 0.002;

// Current tape playback state (the MP4 singleton is always "the current one").
let videoReady = false; // First playback-progress heard since the last swap.
let isPaused = false;
let currentProgress = 0;
let playError = null;

// Audio gating — bios falls back to muted autoplay; the first tap unmutes.
let audioActivated = false;
let hasAudioContext = false;

let thumbs = new Map(); // code -> bitmap | "loading" | "failed"
let hearts = new Map(); // code -> { hearted, count } (learned on toggle)
let heartBusy = false;
let heartBtnBox = null; // Rebuilt each paint; tap target for ♥.
let handleBox = null; // Tap target for the owner handle → profile.

let globalSend = null;
let netPreload = null;
let netUserRequest = null;
let showNotice = null;
let paintCount = 0n;

function tapeAt(index) {
  return index >= 0 && index < tapes.length ? tapes[index] : null;
}

// The raw at-blobs Spaces URLs the feed carries 403 right now (private
// ACLs), so tapes stream through the AC media route instead — it redirects
// to the public per-user bucket and supports byte ranges (the same path
// video.mjs trusts). Local dev's media route is unreliable; go to prod.
const MEDIA_ORIGIN =
  typeof location !== "undefined" && /localhost|127\.0\.0\.1/.test(location.host)
    ? "https://aesthetic.computer"
    : "";

function mp4Url(code) {
  return `${MEDIA_ORIGIN}/media/tapes/${code}.mp4`;
}

// Baked thumbnails also sit behind the 403 ACLs with no media route yet —
// requested anyway so posters light up the moment that's fixed; the dark
// !code card carries until then.
function thumbUrl(code) {
  return `${SPACES}/tapes/${code}-thumb.jpg`;
}

function boot({ wipe, send, net, notice, hud }) {
  wipe(0);
  globalSend = send;
  netPreload = net.preload;
  netUserRequest = net.userRequest;
  showNotice = notice;
  hud.label("gram", "white");

  // Local netlify dev can't reach Redis/outbound HTTP from functions, so the
  // feed falls back to production there — same trick chat.mjs's OG previews
  // use. The tapes themselves always stream from Spaces either way.
  const FEED = "/api/tv?types=tape&limit=50";
  fetch(FEED, { signal: AbortSignal.timeout(5000) }) // Local 500s take 30s; fail fast.
    .then((res) => {
      if (!res.ok) throw new Error("feed " + res.status);
      return res.json();
    })
    .catch(() => fetch("https://aesthetic.computer" + FEED).then((res) => {
      if (!res.ok) throw new Error("feed " + res.status);
      return res.json();
    }))
    .then((data) => {
      tapes = data.media?.tapes || [];
      loadingFeed = false;
      if (tapes.length === 0) return;
      playCurrent();
      preloadThumbs(0);
    })
    .catch((err) => {
      console.error("🖼️ gram feed failed:", err);
      feedError = err.message;
      loadingFeed = false;
    });
}

// Swap the MP4 singleton to the current tape. bios tears the old video down
// synchronously, so the poster thumb covers the gap until progress reports.
function playCurrent() {
  const tape = tapeAt(currentIndex);
  if (!tape) return;
  videoReady = false;
  isPaused = false;
  currentProgress = 0;
  playError = null;
  globalSend({
    type: "tape:play-mp4",
    content: {
      code: tape.code,
      mp4Url: mp4Url(tape.code),
      metadata: { kind: "mp4" },
    },
  });
}

// Poster thumbnails for the neighborhood — tiny JPGs, so ±2 is cheap.
function preloadThumbs(center) {
  for (let i = center - 2; i <= center + 2; i++) {
    const tape = tapeAt(i);
    if (!tape?.code || thumbs.has(tape.code)) continue;
    thumbs.set(tape.code, "loading");
    netPreload(thumbUrl(tape.code))
      .then((loaded) => thumbs.set(tape.code, loaded?.img || loaded))
      .catch(() => thumbs.set(tape.code, "failed"));
  }
}

async function toggleHeart() {
  const tape = tapeAt(currentIndex);
  if (!tape || heartBusy) return;
  heartBusy = true;
  const res = await netUserRequest("POST", "/api/chat-heart", {
    type: "tape",
    for: tape.code,
  });
  heartBusy = false;
  if (res?.status === 200) {
    hearts.set(tape.code, { hearted: res.hearted, count: res.count });
  } else {
    showNotice?.("SIGN IN TO HEART", ["yellow"]);
  }
}

function paint($) {
  const { wipe, ink, paste, screen, rec, needsPaint } = $;
  paintCount += 1n;

  if (loadingFeed) {
    wipe(0);
    ink("white").write("LOADING GRAM...", { center: "xy", size: 2 });
    return;
  }
  if (feedError) {
    wipe(0);
    ink("red").write("FEED ERROR", { center: "xy", size: 2 });
    ink("white").write(feedError, { center: "x", y: screen.height / 2 + 20 });
    return;
  }
  if (tapes.length === 0) {
    wipe(0);
    ink("yellow").write("NO TAPES", { center: "xy", size: 2 });
    return;
  }

  // Ease toward the target row; commit the swap when the snap lands.
  if (isTransitioning) {
    displayIndex += (targetIndex - displayIndex) * 0.18;
    if (Math.abs(targetIndex - displayIndex) < 0.01) {
      displayIndex = targetIndex;
      isTransitioning = false;
      if (currentIndex !== targetIndex) {
        currentIndex = targetIndex;
        playCurrent();
        preloadThumbs(currentIndex);
      }
    }
  }

  const slideOffset = (displayIndex - currentIndex) * screen.height;
  const sliding = Math.abs(slideOffset) > 0.5;

  heartBtnBox = null;
  handleBox = null;

  if (sliding || !videoReady) {
    // Poster mode — opaque, thumbnails carry the motion (the MP4 singleton
    // can't slide two videos at once; the pool upgrade fixes that later).
    wipe(20, 10, 24);
    drawPoster($, currentIndex, -slideOffset);
    if (sliding) {
      const incoming = displayIndex > currentIndex ? currentIndex + 1 : currentIndex - 1;
      const yOff = incoming > currentIndex
        ? screen.height - slideOffset
        : -screen.height - slideOffset;
      drawPoster($, incoming, yOff);
      drawChrome($, incoming, yOff);
      const dividerY = Math.floor(incoming > currentIndex ? screen.height - slideOffset : -slideOffset);
      if (dividerY > 0 && dividerY < screen.height) {
        ink(255, 255, 255, 60).line(0, dividerY, screen.width, dividerY);
      }
    }
    if (!videoReady && !sliding && !playError) {
      // Spinner while the swapped-in video buffers behind the poster.
      const angle = (Number(paintCount) * 0.15) % (Math.PI * 2);
      for (let i = 0; i < 8; i++) {
        const a = angle + (i * Math.PI) / 4;
        ink(255, 200, 100, Math.floor(255 * (1 - i / 8))).circle(
          screen.width / 2 + Math.cos(a) * 24,
          screen.height / 2 + Math.sin(a) * 24,
          3,
          true,
        );
      }
    }
    needsPaint();
  } else if (isPaused) {
    wipe(0, 100);
    ink(255, 200).write("||", { center: "xy", size: 2 });
    ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
  } else {
    wipe(0, 0, 0, 0); // Transparent — the bios <video> underlay shows through.
    needsPaint();
  }

  // Scrub overlay — target line (drag) vs needle line (playback, lagging).
  if (scrubber.isScrubbing) {
    ink(0, 0, 0, 100).box(0, 0, screen.width, screen.height);
    const targetX = Math.floor(scrubber.targetProgress * screen.width);
    const needleX = Math.floor(scrubber.needleProgress * screen.width);
    ink(255, 200, 0, 200).line(targetX, 0, targetX, screen.height);
    ink(255, 100, 0, 150).line(needleX, 0, needleX, screen.height);
    ink(255).write(`${Math.floor(scrubber.needleProgress * 100)}%`, {
      center: "xy",
      size: 2,
    });
  }

  if (playError) {
    ink(255, 80, 80).write("TAPE UNAVAILABLE", { center: "xy" });
    ink(150, 150, 150).write("swipe on", { center: "x", y: screen.height / 2 + 16 });
  }

  drawChrome($, currentIndex, sliding ? -slideOffset : 0, !sliding);

  // VHS-style progress bar along the bottom (painted by the runtime).
  if (rec) {
    rec.tapeProgress =
      !sliding && currentProgress > 0 && currentProgress < 1 ? currentProgress : 0;
  }

  // Audio gate — bios may have fallen back to muted autoplay.
  if (!audioActivated && hasAudioContext && videoReady && !sliding) {
    const pulse = Math.abs(Math.sin(Number(paintCount) * 0.1));
    const alpha = Math.floor(128 + pulse * 127);
    ink(0, 0, 0, 120).box(0, screen.height / 2 - 14, screen.width, 28);
    ink(255, 200, 100, alpha).write("TAP FOR SOUND", { center: "xy" });
    needsPaint();
  }
}

function drawPoster($, index, yOff) {
  const { ink, paste, screen } = $;
  const tape = tapeAt(index);
  if (!tape) return;
  if (yOff <= -screen.height || yOff >= screen.height) return;
  const bmp = thumbs.get(tape.code);
  if (bmp && typeof bmp === "object" && bmp.width > 0) {
    // Cover-scale: fill the frame, centered, cropping the overflow.
    const scale = Math.max(screen.width / bmp.width, screen.height / bmp.height);
    paste(
      bmp,
      Math.floor((screen.width - bmp.width * scale) / 2),
      Math.floor(yOff + (screen.height - bmp.height * scale) / 2),
      scale,
    );
  } else {
    ink(30, 20, 40).box(0, yOff, screen.width, screen.height);
    ink(120, 100, 140).write(`!${tape.code}`, {
      center: "x",
      y: yOff + screen.height / 2,
    });
  }
}

function drawChrome($, index, yOff, interactive = false) {
  const { ink, screen } = $;
  const tape = tapeAt(index);
  if (!tape) return;
  const baseY = 22 + yOff; // Below the HUD label row.
  if (baseY <= -screen.height || baseY >= screen.height * 2) return;

  ink("white").write(`!${tape.code}`, { x: 8, y: baseY });
  const handle = tape.owner?.handle;
  if (handle) {
    ink("yellow").write(handle, { x: 8, y: baseY + 14 });
    if (interactive) {
      handleBox = { x: 4, y: baseY + 10, w: handle.length * 6 + 8, h: 14 };
    }
  }
  ink("gray").write(`${index + 1}/${tapes.length}`, { right: 8, y: baseY });

  // ♥ bottom-right, drawn as pixels — no shipped font carries U+2665
  // (/api/bdf-glyph returns null for it in both unifont and MatrixChunky8).
  // Fills pink once you've hearted it; the count is learned on toggle
  // (batch counts arrive with the feed API's next revision).
  const state = hearts.get(tape.code);
  const hx = screen.width - 30;
  const hy = screen.height - 42;
  drawHeart(ink, hx, hy + yOff, 3, state?.hearted ? [255, 100, 180] : [255, 255, 255, 210]);
  if (state?.count > 0) {
    ink(255, 200, 220).write(`${state.count}`, { x: hx + 6, y: hy + yOff + 22 });
  }
  if (interactive) {
    heartBtnBox = { x: hx - 12, y: hy - 12, w: 44, h: 44 };
  }

  const hintY = screen.height - 12 + yOff;
  if (hintY > 0 && hintY < screen.height) {
    ink(255, 255, 255, 90).write("surf - scrub - tap", { center: "x", y: hintY });
  }
}

// A 7×6 pixel heart at `scale` — piece chrome stays in the pixel buffer.
const HEART_ROWS = [0b0110110, 0b1111111, 0b1111111, 0b0111110, 0b0011100, 0b0001000];
function drawHeart(ink, x, y, scale, color) {
  for (let row = 0; row < HEART_ROWS.length; row++) {
    for (let col = 0; col < 7; col++) {
      if (HEART_ROWS[row] & (1 << (6 - col))) {
        ink(...color).box(x + col * scale, y + row * scale, scale, scale);
      }
    }
  }
}

function act({ event: e, screen, jump }) {
  const now = performance.now();
  const hit = (box) =>
    box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;

  if (e.is("touch") && !gestureMode && !isTransitioning) {
    gestureStartX = e.x;
    gestureStartY = e.y;
  }

  // Commit to scrub (horizontal) or scroll (vertical) after 10px of travel.
  if ((e.is("draw") || e.drag) && !gestureMode && !isTransitioning) {
    const dx = Math.abs(e.x - gestureStartX);
    const dy = Math.abs(e.y - gestureStartY);
    if (Math.sqrt(dx * dx + dy * dy) > gestureThreshold) {
      if (dx > dy && videoReady) {
        gestureMode = "scrub";
        wasPlayingBeforeScrub = !isPaused;
        scrubber.start(e, currentProgress, !isPaused);
        lastSeekProgress = currentProgress;
      } else {
        gestureMode = "scroll";
        displayIndex = currentIndex;
      }
    }
  }

  if ((e.is("draw") || e.drag) && gestureMode === "scrub") {
    scrubber.drag(e, screen.width);
    if (Math.abs(scrubber.needleProgress - lastSeekProgress) > seekThreshold) {
      globalSend({
        type: "recorder:present:seek",
        content: { progress: scrubber.needleProgress, scrubbing: true },
      });
      lastSeekProgress = scrubber.needleProgress;
      currentProgress = scrubber.needleProgress;
    }
  }

  if ((e.is("draw") || e.drag) && gestureMode === "scroll") {
    const delta = (e.y - gestureStartY) / screen.height;
    displayIndex = Math.max(0, Math.min(tapes.length - 1, currentIndex - delta));
  }

  if (e.is("lift")) {
    if (gestureMode === "scrub") {
      scrubber.end();
      globalSend({
        type: "recorder:present:seek",
        content: { progress: scrubber.needleProgress, scrubEnd: true },
      });
      if (wasPlayingBeforeScrub) {
        globalSend({ type: "recorder:present:play" });
      }
    } else if (gestureMode === "scroll") {
      const swipe = e.y - gestureStartY;
      if (now - lastSwipeTime < 300 || isTransitioning) {
        gestureMode = null;
        displayIndex = currentIndex;
        return;
      }
      if (swipe < -swipeThreshold && currentIndex < tapes.length - 1) {
        targetIndex = currentIndex + 1;
        isTransitioning = true;
        lastSwipeTime = now;
      } else if (swipe > swipeThreshold && currentIndex > 0) {
        targetIndex = currentIndex - 1;
        isTransitioning = true;
        lastSwipeTime = now;
      } else {
        targetIndex = currentIndex;
        displayIndex = currentIndex;
      }
    } else if (!gestureMode) {
      // Plain tap: heart button, handle, audio unlock, then pause toggle.
      if (hit(heartBtnBox)) {
        toggleHeart();
      } else if (hit(handleBox) && tapeAt(currentIndex)?.owner?.handle) {
        globalSend({ type: "tape:stop" });
        jump(tapeAt(currentIndex).owner.handle); // → their profile.
      } else if (!audioActivated && hasAudioContext) {
        audioActivated = true;
        globalSend({
          type: "audio-context:resume-request",
          content: { userGesture: true, source: "gram-first-tap", forceResume: true },
        });
        globalSend({ type: "recorder:present:play" }); // Unmute via Web Audio handoff.
      } else if (videoReady) {
        globalSend({ type: "tape:toggle-play-mp4" });
      }
    }
    gestureMode = null;
  }
}

function sim({ needsPaint }) {
  if (scrubber.isScrubbing || scrubber.inertiaActive) {
    scrubber.simulate();
    if (Math.abs(scrubber.needleProgress - lastSeekProgress) > seekThreshold) {
      globalSend({
        type: "recorder:present:seek",
        content: { progress: scrubber.needleProgress, scrubbing: true },
      });
      lastSeekProgress = scrubber.needleProgress;
      currentProgress = scrubber.needleProgress;
    }
    if (scrubber.inertiaActive && Math.abs(scrubber.needleVelocity) < 0.0001) {
      scrubber.reset();
    }
    needsPaint?.();
  }
}

function receive({ type, content }) {
  if (type === "tape:playback-progress") {
    currentProgress = content.progress || 0;
    videoReady = true;
    return;
  }
  if (type === "tape:mp4-playing") {
    isPaused = false;
    return;
  }
  if (type === "tape:mp4-paused") {
    isPaused = true;
    return;
  }
  if (type === "tape:audio-context-state") {
    hasAudioContext = true;
    if (content.state === "running") audioActivated = true;
    return;
  }
  if (type === "tape:error") {
    console.warn("🖼️ gram tape error:", content);
    playError = typeof content === "string" ? content : "error";
  }
}

function leave() {
  globalSend?.({ type: "tape:stop" }); // Tear down the underlay video.
}

export { boot, paint, act, sim, receive, leave };
