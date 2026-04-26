// Link, 2026.04.25
// Pair this account with an aesthetic computer device.
//
// Two flows, mirroring the native `link.mjs`:
//   `link`        — web generates a 6-char code, claims it with the current
//                   user's auth, keeps it on screen. Device polls
//                   device-pair?code=… on the other side.
//   `link CODE`   — claim a code that was already generated on the device
//                   (device-initiated pairing). The web user with auth
//                   completes the handshake by claiming.

const API_URL = "/.netlify/functions/device-pair";
const CODE_TTL_MS = 10 * 60 * 1000; // matches device-pair TTL (600s)

let code = null;
let userHandle = null;
let expiresAt = 0;
let status = "init"; // init | requesting | ready | expired | error | unauthed
let errorMsg = "";
let frame = 0;
let copiedFrame = -9999;
let newBtn = null;
let copyBtn = null;
let signInBtn = null;

async function generate(net) {
  status = "requesting";
  errorMsg = "";
  code = null;
  try {
    const createRes = await fetch(API_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ action: "create" }),
    });
    const createData = await createRes.json();
    if (!createData.code) {
      throw new Error(createData.message || "create failed");
    }

    const claimRes = await net.userRequest("POST", API_URL, {
      action: "claim",
      code: createData.code,
    });
    if (!claimRes?.handle) {
      throw new Error(claimRes?.message || "claim failed");
    }

    code = createData.code;
    expiresAt = Date.now() + CODE_TTL_MS;
    status = "ready";
    console.log(`🔗 Link code ${code} (10 min)`);
  } catch (err) {
    status = "error";
    errorMsg = (err && err.message) || "network error";
  }
}

// Claim a code that the device generated (device-initiated pairing).
// Different from generate() — no create step, just the auth-bound claim.
async function claimExisting(net, providedCode) {
  status = "requesting";
  errorMsg = "";
  code = (providedCode || "").toUpperCase();
  try {
    const claimRes = await net.userRequest("POST", API_URL, {
      action: "claim",
      code,
    });
    if (!claimRes?.handle) {
      throw new Error(claimRes?.message || "claim failed");
    }
    expiresAt = Date.now() + CODE_TTL_MS;
    status = "claimed";
    console.log(`🔗 Claimed device code ${code} as @${claimRes.handle}`);
  } catch (err) {
    status = "error";
    errorMsg = (err && err.message) || "network error";
  }
}

async function boot({ net, handle, user, hud, params, colon }) {
  hud?.labelBack?.();
  userHandle = handle?.() || null;
  const signedIn = !!(userHandle || user?.email);
  if (!signedIn) {
    status = "unauthed";
    return;
  }
  // `link CODE` (or `link:CODE`) → claim the device-generated code.
  const provided = (params?.[0] || colon?.[0] || "").trim();
  if (provided && provided.length >= 4) {
    await claimExisting(net, provided);
    return;
  }
  await generate(net);
}

function fmtCountdown(ms) {
  const total = Math.max(0, ms);
  const mm = Math.floor(total / 60000);
  const ss = Math.floor((total % 60000) / 1000)
    .toString()
    .padStart(2, "0");
  return `${mm}:${ss}`;
}

function paint({ wipe, ink, write, screen, ui }) {
  frame++;
  wipe(12, 14, 22);

  const cx = (screen.width / 2) | 0;
  let y = 18;

  ink(170, 200, 255).write("link", { center: "x", y, size: 2, screen });
  y += 28;
  ink(140, 150, 180).write(
    "pair this account with a device",
    { center: "x", y, screen },
  );
  y += 16;

  if (userHandle) {
    ink(120, 180, 255).write(`@${userHandle}`, { center: "x", y, screen });
    y += 16;
  }

  const codeY = Math.max(y + 40, ((screen.height / 2) | 0) - 20);

  if (status === "unauthed") {
    ink(255, 160, 100).write("sign in first", {
      center: "x",
      y: codeY,
      screen,
    });
    ink(180, 180, 200).write(
      "log in on the prompt to generate a link code",
      { center: "x", y: codeY + 18, screen },
    );

    if (!signInBtn) signInBtn = new ui.TextButton("Sign in");
    signInBtn.reposition({ center: "x", y: codeY + 44, screen });
    signInBtn.paint(
      { ink, box: (b) => ink().box(b) },
      [
        [60, 90, 140],
        [120, 160, 220],
        255,
      ],
    );
    return;
  }

  if (status === "requesting") {
    const dots = ".".repeat(((frame / 15) | 0) % 4);
    ink(180, 200, 255).write(`requesting code${dots}`, {
      center: "x",
      y: codeY,
      screen,
    });
    return;
  }

  if (status === "error") {
    ink(255, 110, 110).write("error", {
      center: "x",
      y: codeY,
      screen,
    });
    ink(220, 160, 160).write(errorMsg || "unknown", {
      center: "x",
      y: codeY + 18,
      screen,
    });
  }

  if (status === "claimed") {
    // Device-initiated pairing succeeded — the device is now polling and
    // will write its config + reboot momentarily.
    ink(140, 220, 140).write(code || "------", {
      center: "x",
      y: codeY,
      size: 3,
      screen,
    });
    ink(180, 230, 200).write("claimed — device is linking…", {
      center: "x",
      y: codeY + 40,
      screen,
    });
    ink(160, 200, 220).write("you can close this", {
      center: "x",
      y: codeY + 58,
      screen,
    });
  }

  if (status === "ready" || status === "expired") {
    const c = code || "------";
    const expired = status === "expired";

    if (expired) {
      ink(140, 100, 100).write(c, {
        center: "x",
        y: codeY,
        size: 3,
        screen,
      });
      ink(255, 140, 140).write("expired", {
        center: "x",
        y: codeY + 40,
        screen,
      });
    } else {
      // Pulsing green for the code so it reads as live.
      const pulse = 200 + (((Math.sin(frame / 20) + 1) / 2) * 55) | 0;
      ink(120, pulse, 200).write(c, {
        center: "x",
        y: codeY,
        size: 3,
        screen,
      });
      ink(170, 200, 220).write(
        `expires in ${fmtCountdown(expiresAt - Date.now())}`,
        { center: "x", y: codeY + 40, screen },
      );
      ink(140, 150, 180).write(`on the device:  link ${c}`, {
        center: "x",
        y: codeY + 58,
        screen,
      });
    }
  }

  // Action buttons.
  if (!newBtn) newBtn = new ui.TextButton("New code");
  newBtn.reposition({ x: 8, bottom: 8, screen });
  const newColor = [
    [20, 60, 120],
    [60, 140, 220],
    255,
  ];
  newBtn.paint({ ink, box: (b) => ink().box(b) }, newColor);

  if (status === "ready" && code) {
    if (!copyBtn) copyBtn = new ui.TextButton("Copy");
    copyBtn.reposition({ right: 8, bottom: 8, screen });
    const justCopied = frame - copiedFrame < 90;
    const copyColor = justCopied
      ? [
          [40, 120, 40],
          [80, 200, 80],
          255,
        ]
      : newColor;
    copyBtn.paint({ ink, box: (b) => ink().box(b) }, copyColor);
    if (justCopied) {
      ink(180, 255, 180).write("copied", {
        center: "x",
        y: screen.height - 28,
        screen,
      });
    }
  }
}

function sim() {
  if (status === "ready" && expiresAt && Date.now() > expiresAt) {
    status = "expired";
  }
}

function act({ event: e, net, jump }) {
  if (status === "unauthed") {
    signInBtn?.act(e, () => {
      jump("prompt");
    });
    return;
  }
  newBtn?.act(e, () => {
    generate(net);
  });
  if (status === "ready" && code) {
    copyBtn?.act(e, () => {
      try {
        navigator.clipboard?.writeText?.(code);
        copiedFrame = frame;
      } catch (_) {
        /* clipboard unavailable */
      }
    });
  }
}

function meta() {
  return {
    title: "Link",
    desc: "Pair this account with an aesthetic computer device.",
  };
}

export { boot, paint, sim, act, meta };
