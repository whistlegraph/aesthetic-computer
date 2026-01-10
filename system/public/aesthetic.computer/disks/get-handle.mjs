// Get Handle, 2026.01.10
// A friendly landing page for claiming/setting your @handle.

let loginBtn;
let signupBtn;
let openPromptBtn;

function meta() {
  return {
    title: "get-handle • aesthetic.computer",
    desc: "Claim or set your @handle.",
  };
}

async function boot({ wipe, hud, ui, screen }) {
  wipe(12);
  hud?.label?.("get-handle");

  // Buttons (styled similarly to prompt.mjs / wallet.mjs)
  loginBtn = new ui.TextButton("Log in", { center: "x", y: 0, screen });
  loginBtn.stickyScrubbing = true;

  signupBtn = new ui.TextButton("I'm new", { center: "x", y: 0, screen });
  signupBtn.stickyScrubbing = true;

  openPromptBtn = new ui.TextButton("Set handle", { center: "x", y: 0, screen });
  openPromptBtn.stickyScrubbing = true;
}

function paint({ wipe, ink, write, box, screen, user, handle, help }) {
  wipe(12);

  const marginX = 12;
  const isSmall = screen.height < 180;
  let y = isSmall ? 14 : 22;

  // Title
  ink(255).write("Get an @handle", { x: marginX, y }, undefined, undefined, false, "MatrixChunky8");
  y += isSmall ? 14 : 18;

  const currentHandle = typeof handle === "function" ? handle() : null;
  const verified = !!user?.email_verified;

  // Description
  ink(180).write("Your name on Aesthetic Computer.", { x: marginX, y });
  y += isSmall ? 12 : 16;

  // Examples with syntax highlighting
  ink(100).write("Examples:", { x: marginX, y });
  const exX = marginX + 54;
  ink(255, 100, 180).write("@jeffrey", { x: exX, y }); // pink
  ink(100).write(",", { x: exX + 50, y });
  ink(100, 220, 255).write("@fifi", { x: exX + 58, y }); // cyan
  ink(100).write(",", { x: exX + 90, y });
  ink(180, 255, 100).write("@prutti", { x: exX + 98, y }); // lime
  y += isSmall ? 16 : 24;

  if (!user) {
    ink(255, 220, 0).write("Step 1:", { x: marginX, y });
    ink(255).write("Log in or sign up", { x: marginX + 48, y });
    y += isSmall ? 10 : 14;
    ink(140).write("Then you can set your handle.", { x: marginX, y });
    y += isSmall ? 14 : 20;
  } else if (!verified) {
    ink(255, 220, 0).write("Step 1:", { x: marginX, y });
    ink(255).write("Verify your email", { x: marginX + 48, y });
    y += isSmall ? 10 : 14;
    ink(140).write("Check your inbox, then come back.", { x: marginX, y });
    y += isSmall ? 14 : 20;
  } else if (!currentHandle) {
    ink(0, 255, 120).write("Ready!", { x: marginX, y });
    ink(255).write("Set your handle", { x: marginX + 42, y });
    y += isSmall ? 10 : 14;
    ink(140).write("Type:", { x: marginX, y });
    ink(255, 220, 100).write("handle", { x: marginX + 34, y });
    ink(180).write("yourname", { x: marginX + 76, y });
    y += isSmall ? 14 : 20;
  } else {
    ink(0, 255, 120).write("You have:", { x: marginX, y });
    ink(255, 100, 180).write(`@${currentHandle}`, { x: marginX + 60, y });
    y += isSmall ? 10 : 14;
    ink(140).write("Change it:", { x: marginX, y });
    ink(255, 220, 100).write("handle", { x: marginX + 64, y });
    ink(180).write("newname", { x: marginX + 106, y });
    y += isSmall ? 14 : 20;
  }

  // Compute button positions based on available space
  const footerHeight = isSmall ? 10 : 14;
  const buttonHeight = 22;
  const buttonGap = isSmall ? 4 : 8;
  
  // Reserve space: footer tip at bottom, then buttons above
  const tipY = screen.height - footerHeight;
  
  // Buttons layout - position from bottom up
  let buttonRowY;
  if (!user) {
    // Two rows of buttons needed
    buttonRowY = tipY - buttonHeight * 2 - buttonGap * 2 - 4;
  } else {
    // One row
    buttonRowY = tipY - buttonHeight - buttonGap - 4;
  }

  // If logged out, show login + signup side-by-side.
  if (!user) {
    loginBtn.reposition({ center: "x", y: buttonRowY, screen });
    signupBtn.reposition({ center: "x", y: buttonRowY, screen });

    const offset = 6;
    signupBtn.btn.box.x += signupBtn.btn.box.w / 2 + offset;
    loginBtn.btn.box.x -= loginBtn.btn.box.w / 2 + offset;

    loginBtn.paint({ ink, box, write }, [[0, 0, 64], 255, 255, [0, 0, 64]]);
    signupBtn.paint({ ink, box, write }, [[0, 64, 0], 255, 255, [0, 64, 0]]);

    // Open prompt below
    openPromptBtn.reposition({ center: "x", y: buttonRowY + buttonHeight + buttonGap, screen });
    openPromptBtn.paint({ ink, box, write }, [[64, 0, 64], 255, 255, [64, 0, 64]]);
  } else {
    // If logged in, just show Open prompt.
    openPromptBtn.reposition({ center: "x", y: buttonRowY, screen });
    openPromptBtn.paint({ ink, box, write }, [[64, 0, 64], 255, 255, [64, 0, 64]]);

    // Subtext under button (small pulse)
    if (!isSmall) {
      const pulse = 0.65 + Math.sin(help.repeat * 0.07) * 0.2;
      ink(120, Math.floor(255 * pulse)).write(
        "(type: handle yourname)",
        { center: "x", y: buttonRowY + buttonHeight + 4, screen },
      );
    }
  }
}

function act({ event: e, net, jump, screen, store, user }) {
  if (e.is("reframed")) {
    // Reposition in paint on next frame.
  }

  if (!user) {
    loginBtn?.btn?.act(e, { push: () => net.login() });
    signupBtn?.btn?.act(e, { push: () => net.signup() });
  }

  openPromptBtn?.btn?.act(e, {
    push: () => {
      // Use prompt's prefill route used elsewhere (e.g. profile.mjs)
      store["prompt:splash"] = true;
      jump("prompt~handle ");
    },
  });

  // Keyboard shortcut: enter opens prompt
  if (e.is("keyboard:down:enter")) {
    store["prompt:splash"] = true;
    jump("prompt~handle ");
  }

  // Escape returns to prompt
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

export { boot, paint, act, meta };
