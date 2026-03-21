// Token, 2026.02.12.08.38
// Display and copy your AC authentication token for API/MCP use
// Requires three taps to reveal (security pattern)

export const nohud = true;

let taps = 0;
let revealed = false;
let copied = false;
let copiedTimer = 0;

export function boot($) {
  taps = 0;
  revealed = false;
  $.wipe(20, 20, 30);
}

export function paint($) {
  const { screen } = $;

  $.wipe(20, 20, 30);

  const token = localStorage.getItem("ac-token");

  const centerX = screen.width / 2;
  let y = 60;

  // Title
  $.ink(255, 255, 255);
  $.write("Your AC Token", { center: "x", size: 2 }, [centerX, y]);
  y += 40;

  if (!token) {
    $.ink(255, 100, 100);
    $.write("Not logged in", { center: "x" }, [centerX, y]);
    y += 30;
    $.ink(150, 150, 150);
    $.write("Please sign in first", { center: "x", size: 0.8 }, [centerX, y]);
    return;
  }

  if (!revealed) {
    // Security prompt - require 3 taps
    $.ink(255, 200, 100);
    $.write(`Tap ${3 - taps} more time${3 - taps !== 1 ? 's' : ''} to reveal`, { center: "x" }, [centerX, y]);
    y += 30;
    $.ink(150, 150, 150);
    $.write("(Security measure)", { center: "x", size: 0.8 }, [centerX, y]);
    return;
  }

  // Token display (truncated for security)
  $.ink(100, 200, 255);
  const truncated = token.substring(0, 20) + "..." + token.substring(token.length - 20);
  $.write(truncated, { center: "x", size: 0.8 }, [centerX, y]);
  y += 40;

  // Copy button
  const buttonWidth = 120;
  const buttonHeight = 40;
  const buttonX = centerX - buttonWidth / 2;
  const buttonY = y;

  const hover = $.event.is("move") &&
                $.event.x >= buttonX &&
                $.event.x <= buttonX + buttonWidth &&
                $.event.y >= buttonY &&
                $.event.y <= buttonY + buttonHeight;

  if (copied && copiedTimer > 0) {
    $.ink(100, 255, 100, 200);
  } else if (hover) {
    $.ink(100, 200, 255, 220);
  } else {
    $.ink(80, 160, 220, 180);
  }

  $.box(buttonX, buttonY, buttonWidth, buttonHeight);

  $.ink(255, 255, 255);
  const buttonText = copied && copiedTimer > 0 ? "Copied!" : "Copy Token";
  $.write(buttonText, { center: "xy" }, [centerX, buttonY + buttonHeight / 2]);

  y += 60;

  // Instructions
  $.ink(150, 150, 150);
  $.write("Use this token for:", { center: "x", size: 0.9 }, [centerX, y]);
  y += 25;

  $.ink(200, 200, 200);
  $.write("• API requests with Bearer auth", { center: "x", size: 0.8 }, [centerX, y]);
  y += 20;
  $.write("• MCP server AC_TOKEN env var", { center: "x", size: 0.8 }, [centerX, y]);
  y += 20;
  $.write("• Development and automation", { center: "x", size: 0.8 }, [centerX, y]);

  y += 40;
  $.ink(255, 200, 100);
  $.write("⚠️  Keep this token secret!", { center: "x", size: 0.9 }, [centerX, y]);
}

export function sim($) {
  if (copiedTimer > 0) {
    copiedTimer--;
    if (copiedTimer === 0) {
      copied = false;
    }
  }
}

export function act($) {
  const { event, screen } = $;

  if (event.is("lift")) {
    const token = localStorage.getItem("ac-token");
    if (!token) return;

    // If not yet revealed, count taps
    if (!revealed) {
      taps++;
      if (taps >= 3) {
        revealed = true;
        console.log("🔓 Token revealed");
      }
      return;
    }

    // If revealed, check for copy button click
    const centerX = screen.width / 2;
    const buttonWidth = 120;
    const buttonHeight = 40;
    const buttonX = centerX - buttonWidth / 2;
    const buttonY = 100;

    if (event.x >= buttonX &&
        event.x <= buttonX + buttonWidth &&
        event.y >= buttonY &&
        event.y <= buttonY + buttonHeight) {

      // Copy to clipboard
      navigator.clipboard.writeText(token).then(() => {
        copied = true;
        copiedTimer = 60; // 1 second at 60fps
        console.log("✅ Token copied to clipboard");
      }).catch(err => {
        console.error("❌ Failed to copy token:", err);
      });
    }
  }
}
