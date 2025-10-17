// Profile, 2023.6.04.16.58.31
// The default profile page for all users.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Done
  - [x] Add `d` and `p` shortcuts for download and process jumping.
  - [x] Add zooming, similar to `hw`.
  - [x] `profile` should be table to <- -> on a user's paintings 
  - [c] Cache the bitmaps.
  - [x] Add left and right tap buttons.
  - [x] Loading paintings should make a beep.
  - [x] Tap into lightbox for painting / playback.
  - [x] Move mood.
  - [x] Wire up arrow keys.
  - [] Modify `api/profile` request to show a full text response
      if json is not returned.
  + Later
  - ☁️ General thoughts:
    - [] Should @handle eventually be a code piece in the system for every user?
    - [] And then they can edit it?
    - [] Or maybe just a custom paint function?
    - [] What happens when you visit there now?
  - 💡 Ideas for content:
    - [] Most recent user painting.
    - [] Activity log
    - [] Consider public facing vs user facing differences.
      - [] Like the ability to set a handle.
    - [] Settings
      - [] Theme
    - [] Globally warn user if they are inauthenticated somehow.
      - [] Or if they are offline... using tiny LEDs?
#endregion */

const FETCHING = "Fetching";
let debug;
let profile,
  noprofile = FETCHING,
  noprofileAction,
  noprofileBtn;

let timestampBtn, shortCodeBtn, prevBtn, nextBtn;
let ellipsisTicker;
let visiting, code, painting, paintings, index, shortCode;
const paintingMetadataCache = new Map();
let zoomed = false;
let zoomLevel = 1;

const { max, min } = Math;
import * as sfx from "./common/sfx.mjs";

// 📰 Meta
function meta({ piece }) {
  return {
    title: `${piece} • aesthetic.computer`,
    desc: `Welcome to ${piece}'s profile.`,
    // TODO: ^ Replace with user's last status.
  };
}

// 🥾 Boot
async function boot({
  params,
  user,
  gizmo,
  handle,
  debug,
  hud,
  net,
  get,
  debug: d,
}) {
  // Mask from `profile` if we are logged in.
  debug = d;
  const hand = handle();

  visiting = params[0] || hand;

  ellipsisTicker = new gizmo.EllipsisTicker();

  if (visiting) {
    hud.label(visiting);
    net.rewrite(visiting);
  }

  if (visiting) {
    if (visiting === hand) {
      console.log("🤺 Visiting your profile:", visiting);
    } else {
      console.log("🤺 Visiting the profile of...", visiting);
    }
  } else {
    if (user) {
      if (user.email_verified) {
        noprofile = "Create handle.";
        noprofileAction = "handle";
      } else {
        noprofile = "Check email to verify.";
        noprofileAction = "email";
      }
    } else {
      noprofile = "Log in or Sign up";
      noprofileAction = "imnew";
    }
  }

  // 🎆 Check to see if this user's profile actually exists via a server-side call.
  fetch(`/api/profile/${visiting}`, {
    headers: { Accept: "application/json" },
  })
    .then(async (response) => {
      const data = (await response.json()).mood;
      if (response.ok) {
        if (debug) console.log("🙆 Profile found:", data);
        profile = { handle: visiting, mood: data?.mood };
        noprofile = null;
      } else {
        if (debug) console.warn("🙍 Profile not found:", data);
        noprofile = "No profile found for: " + visiting;
      }
    })
    .catch((error) => {
      console.error("Error:", error);
      noprofile = "Error retrieving profile.";
    });

  if (visiting) {
    // Fetch all of a user's paintings...
    fetch(`/media-collection?for=${visiting}/painting`)
      .then((res) => res.json())
      .then((data) => {
        paintings = data?.files;
        if (paintings.length > 0) {
          index = paintings.length - 1;
          loadPainting(get, index, visiting);
        }
      })
      .catch((err) => {
        console.warn("Could not load painting or fetch media.", err);
      });
  }
}

// 🎨 Paint
function paint({ api, geo, wipe, help, ink, pen, screen, ui, text, paste }) {
  if (!pen?.drawing) wipe(98);
  ink(127).line();
  if (profile) ink().line().ink().line().ink().line();
  if (profile) ink().write(profile?.mood || "no mood");

  if (painting) {
    const margin = 34;
    const wScale = (screen.width - margin * 2) / painting.width;
    const hScale = (screen.height - margin * 2) / painting.height;
    let scale = Math.min(wScale, hScale, 1);
    if (wScale >= 2 && hScale >= 2) scale = 2;
    let x = screen.width / 2 - (painting.width * scale) / 2;
    let y = screen.height / 2 - (painting.height * scale) / 2;

    if (pen && zoomed && (scale < 1 || scale === 1)) {
      const imgX = (pen.x - x) / scale;
      const imgY = (pen.y - y) / scale;

      // Adjust scale and position for zoom anchored at pen position
      scale = scale >= 1 ? 1 + zoomLevel : zoomLevel;

      x = pen.x - imgX * scale;
      y = pen.y - imgY * scale;
      ink(0, 64).box(0, 0, screen.width, screen.height);
    }

    paste(painting, x, y, { scale });

    // const x = screen.width / 2 - painting.width / 2;
    // const y = screen.height / 2 - painting.height / 2;
    // ink(64).box(x, y, painting.width, painting.height);
    // paste(painting, x, y);
    // ink().box(x, y, painting.width, painting.height, "outline");
  }

  const retrieving = noprofile === FETCHING;
  if (!profile) {
    let text = noprofile;

    if (!noprofileAction) {
      if (retrieving) text += ellipsisTicker.text(help.repeat);
      ink(255).write(text, { center: "xy" }, retrieving ? 64 : "black");
    } else {
      // Make button based on the action.
      noprofileBtn ||= new ui.TextButton(text, { center: "xy", screen });
      noprofileBtn.paint(api);
    }
  }

  if (profile) {
    ink(255).write(
      profile.mood || "no mood",
      { center: "x", y: 6 },
      "black",
      screen.width - 8,
    );
  }

  if (profile && !painting && !paintings) {
    ink(255).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { center: "xy" },
      "black",
    );
  }

  if (profile && !painting && paintings?.length === 0) {
    ink(255).write(`No paintings completed.`, { center: "xy" }, "black");
  }

  if (paintings?.length > 0) {
    ink(0).line(0, screen.height - 1, screen.width, screen.height - 1);

    if (paintings.length > 1) {
      ink("yellow").line(
        0,
        screen.height - 1,
        (index / (paintings.length - 1)) * screen.width,
        screen.height - 1,
      );
    }

    const prevNextMarg = 32;
    const prevNextWidth = 32;

    // Timestamp button (bottom-left, clickable)
    if (code) {
      const pos = { x: 3, y: screen.height - 13 };
      const metricsBox = text.box(code, pos).box || {};
      const textWidth = metricsBox.width ?? metricsBox.w ?? 0;
      const textHeight = metricsBox.height ?? metricsBox.h ?? 12;
      const blockWidth = 6;
      
      const buttonBox = new geo.Box(
        pos.x - blockWidth,
        pos.y,
        textWidth + blockWidth * 2,
        textHeight,
      );

      if (!timestampBtn) timestampBtn = new ui.Button(buttonBox);
      timestampBtn.box = buttonBox;
      timestampBtn.paint((btn) => {
        ink(btn.down ? "orange" : 255).write(code, pos);
      });
    }

    // Short code button (bottom-right, clickable)
    if (shortCode && shortCode.trim().length > 0) {
      const shortDisplay = `#${shortCode}`;
      const codeMetrics = text.box(shortDisplay).box || {};
      const codeWidth = codeMetrics.width ?? codeMetrics.w ?? 0;
      const codeHeight = codeMetrics.height ?? codeMetrics.h ?? 12;
      const codePos = {
        x: screen.width - codeWidth - 3,
        y: screen.height - 13,
      };
      
      const blockWidth = 6;
      const shortCodeBox = new geo.Box(
        codePos.x - blockWidth,
        codePos.y,
        codeWidth + blockWidth * 2,
        codeHeight,
      );

      if (!shortCodeBtn) shortCodeBtn = new ui.Button(shortCodeBox);
      shortCodeBtn.box = shortCodeBox;
      shortCodeBtn.paint((btn) => {
        ink(btn.down ? "orange" : 255).write(shortDisplay, codePos);
      });
    } else if (code && paintings?.length > 0) {
      // Fallback: show "pending" while waiting for metadata (non-clickable)
      shortCodeBtn = null; // Clear button if no code
      const fallbackDisplay = "...";
      const codeMetrics = text.box(fallbackDisplay).box || {};
      const codeWidth = codeMetrics.width ?? codeMetrics.w ?? 0;
      const codePos = {
        x: screen.width - codeWidth - 3,
        y: screen.height - 13,
      };
      ink(127).write(fallbackDisplay, codePos);
    } else {
      shortCodeBtn = null;
    }

    // Prev & Next Buttons

    if (!prevBtn) {
      prevBtn = new ui.Button();
      if (index === 0) prevBtn.disabled = true;
    }

    prevBtn.box = new geo.Box(
      0,
      prevNextMarg,
      prevNextWidth,
      screen.height - prevNextMarg * 2,
    );

    if (!prevBtn.disabled) {
      prevBtn.paint((btn) => {
        ink(btn.down ? "orange" : 255).write("<", {
          x: 6,
          y: screen.height / 2 - 4,
        });
      });
      ink(255, 255, 0, 8).box(prevBtn.box);
    }

    if (!nextBtn) {
      nextBtn = new ui.Button();
      if (index === paintings.length - 1) nextBtn.disabled = true;
    }

    nextBtn.box = new geo.Box(
      screen.width - prevNextWidth,
      prevNextMarg,
      screen.width,
      screen.height - prevNextMarg * 2,
    );

    if (!nextBtn.disabled) {
      nextBtn.paint((btn) => {
        ink(btn.down ? "orange" : 255).write(">", {
          x: screen.width - 10,
          y: screen.height / 2 - 4,
        });
      });
      ink(255, 255, 0, 8).box(nextBtn.box);
    }
  }

  // return false;
}

// 🎪 Act
function act({
  event: e,
  get,
  send,
  jump,
  screen,
  sound,
  download,
  user,
  net,
  notice,
  store,
}) {
  function process() {
    sfx.push(sound);
    jump(`painting ${visiting}/${code}`);
  }

  function jumpToShortCode() {
    const trimmedCode = shortCode?.trim();
    if (!trimmedCode || trimmedCode.length === 0) {
      console.warn("⚠️ Cannot jump to short code: not available (shortCode:", shortCode, ")");
      return;
    }
    sfx.push(sound);
    const route = `painting~#${trimmedCode}`;
    console.log(`🔗 Jumping to painting with short code: ${route}`);
    jump(route);
  }

  timestampBtn?.act(e, process);
  shortCodeBtn?.act(e, jumpToShortCode);

  function next() {
    if (index === paintings.length - 1) return;
    sfx.push(sound);
    index = min(index + 1, paintings.length - 1);
    loadPainting(get, index, visiting);
    prevBtn.disabled = false;
    nextBtn.disabled = false;
    if (index === paintings.length - 1) nextBtn.disabled = true;
  }

  function prev() {
    if (index === 0) return;
    sfx.push(sound);
    index = max(0, index - 1);
    loadPainting(get, index, visiting);
    prevBtn.disabled = false;
    nextBtn.disabled = false;
    if (index === 0) prevBtn.disabled = true;
  }

  nextBtn?.act(e, next);
  prevBtn?.act(e, prev);

  if (e.is("reframed")) noprofileBtn?.reposition({ center: "xy", screen });

  noprofileBtn?.act(e, {
    push: () => {
      let slug;
      if (noprofileAction === "handle") {
        slug = "prompt~handle";
      } else if (noprofileAction === "email") {
        slug = "prompt~email~" + user?.email;
      } else if (noprofileAction === "imnew") {
        slug = "prompt";
        store["prompt:splash"] = true;
      } else {
        console.warn("🔴 No action specified:", noprofileAction);
      }
      if (slug) jump(slug);
    },
  });

  if (e.is("keyboard:down:arrowleft")) prev();
  if (e.is("keyboard:down:arrowright")) next();

  // Zooming
  if (
    e.is("touch:1") &&
    !timestampBtn?.down &&
    !shortCodeBtn?.down &&
    !prevBtn?.down &&
    !nextBtn?.down &&
    e.button === 0
  ) {
    zoomed = true;
  }

  if (e.is("lift:1")) zoomed = false;

  if (e.is("keyboard:down:space")) {
    zoomLevel += 1;
    if (zoomLevel > 3) zoomLevel = 1;
  }

  if (e.is("keyboard:down:p")) process();

  if (
    painting &&
    (e.is("keyboard:down:d") ||
      (e.is("touch") && e.device === "mouse" && e.button === 2))
  ) {
    // Download a scaled version of the painting...
    download(`painting-${visiting}-${code}.png`, painting, { scale: 6 });
  }

  // Nuke a painting using an authorized user request.
  if (painting && e.is("keyboard:down:n") && user) {
    console.log("💣 Nuking painting:", code, user);
    net
      .userRequest("PUT", "/api/track-media", { slug: code, nuke: true })
      .then((res) => {
        console.log(res);
        if (res.status === 200) {
          console.log("🖌️ Painting record updated:", res);
          notice("NUKED :>", ["yellow", "red"]);
        } else {
          throw new Error(res.status);
        }
      })
      .catch((err) => {
        console.warn("🖌️ Painting record update failure:", err);
        notice(`${err.message} ERROR :(`, ["white", "red"]);
      });
  }
}

// 🧮 Sim
function sim() {
  ellipsisTicker?.sim();
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

// Load a painting from paintings via the index.
let controller = null;
async function loadPainting(get, index, from) {
  painting = undefined; // Clear the current picture.

  if (controller) controller.abort(); // Abort any ongoing requests.
  // Create a new controller for the current request.
  controller = new AbortController();
  const signal = controller.signal;

  try {
    shortCode = undefined;
    const entry = paintings[index];
    const { slug, code: entryCode } = parsePaintingReference(entry);

    if (!slug) throw new Error("Unable to parse painting reference.");

    code = slug; // Maintain existing semantics where `code` represents the slug/timestamp.
    if (entryCode) {
      shortCode = entryCode;
      console.log(`📌 Short code from entry: ${shortCode}`);
    }

    const got = await get.painting(slug).by(from, { signal }); // Assuming `get.painting` is based on fetch and can accept a signal
    painting = got.img;

    // After painting loads, fetch metadata to get short code
    if (!shortCode) {
      try {
        const normalizedHandle = normalizeHandle(from);
        const metadata = await fetchPaintingMetadata(slug, normalizedHandle, signal);
        console.log(`📊 Metadata response:`, metadata);
        if (metadata?.code) {
          shortCode = `${metadata.code}`.replace(/^#/, "").trim();
          console.log(`✅ Loaded painting ${slug}, short code: "${shortCode}" (length: ${shortCode.length})`);
        } else {
          console.log(`⚠️ Loaded painting ${slug}, no short code in database (needs migration)`);
        }
      } catch (metaErr) {
        if (metaErr.name === "AbortError") throw metaErr;
        if (debug) console.warn("Painting metadata lookup failed:", metaErr);
      }
    }
  } catch (err) {
    if (err.name === "AbortError") {
      if (debug) console.log("❌ Request was aborted.");
    } else {
      console.error("Painting load failure:", err);
    }
  } finally {
    controller = null;
  }
}

function parsePaintingReference(entry) {
  let slug;
  let code;

  if (typeof entry === "string") {
    slug = extractSlug(entry);
  } else if (entry && typeof entry === "object") {
    if (typeof entry.slug === "string") slug = entry.slug;
    if (typeof entry.code === "string") code = entry.code;
    if (!slug && typeof entry.url === "string") slug = extractSlug(entry.url);
    if (!slug && typeof entry.path === "string") slug = extractSlug(entry.path);
  }

  if (slug) {
    slug = slug.split("?")[0];
    // If the slug already includes an extension, trim it.
    slug = slug.replace(/\.(png|zip)$/i, "");
    // Allow embedded codes within the slug (e.g., slug#code).
    if (slug.includes("#")) {
      const [slugPart, codePart] = slug.split("#");
      slug = slugPart;
      if (!code && codePart) code = codePart;
    }
  }

  if (code) code = `${code}`.replace(/^#/, "");

  return { slug, code };
}

function extractSlug(source) {
  if (!source) return undefined;
  const parts = `${source}`.split("/");
  const last = parts.pop();
  if (!last) return undefined;
  const filename = last.split("?")[0];
  return filename.replace(/\.(png|zip)$/i, "");
}

function normalizeHandle(handle) {
  if (!handle) return undefined;
  const trimmed = `${handle}`.trim();
  if (!trimmed) return undefined;
  return trimmed.replace(/^@+/, "");
}

async function fetchPaintingMetadata(slug, handle, signal) {
  if (!slug) return null;

  const normalizedHandle = handle && handle !== "anon" ? handle : undefined;
  const cacheKey = `${normalizedHandle || ""}:${slug}`;
  if (paintingMetadataCache.has(cacheKey)) {
    const cached = paintingMetadataCache.get(cacheKey);
    console.log(`📦 Cache hit for ${cacheKey}:`, cached);
    return cached;
  }

  try {
    const params = new URLSearchParams({ slug });
    if (normalizedHandle) params.set("handle", normalizedHandle);

    const url = `/api/painting-metadata?${params}`;
    console.log(`🌐 Fetching metadata from ${url}`);
    const response = await fetch(url, { signal });

    if (!response.ok) {
      if (response.status === 404) {
        console.log(`❌ 404: No metadata found for ${cacheKey}`);
        paintingMetadataCache.set(cacheKey, null);
        return null;
      }
      throw new Error(`HTTP ${response.status}`);
    }

    const data = await response.json();
    console.log(`✅ Metadata fetched for ${cacheKey}:`, data);
    paintingMetadataCache.set(cacheKey, data);
    return data;
  } catch (error) {
    if (error.name === "AbortError") throw error;
    console.warn("Painting metadata request failed:", error);
    return null;
  }
}
