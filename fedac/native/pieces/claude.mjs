// claude.mjs — "code" command: Claude Code with browser OAuth
// If valid credentials exist → launch terminal with claude.
// Otherwise → pop cage+firefox browser for OAuth, then launch.

let mode = "auth"; // auth | terminal
let state = "checking"; // checking | login | waiting | approved | error
let error = "";
let frame = 0;
let browserLaunched = false;

function boot() {
  console.log("[code] boot");
}

function paint({ wipe, ink, box, write, screen, system, wifi }) {
  frame++;
  if (mode === "terminal") return;

  var W = screen.width;
  var H = screen.height;
  var font = "6x10";

  wipe(20, 20, 30);
  ink(100, 180, 255);
  write("code", { x: 10, y: 10, size: 2, font: "matrix" });

  // One-time credential check
  if (state === "checking") {
    console.log("[code] checking credentials...");
    var hasCreds = false;

    try {
      var raw = system.readFile("/tmp/.claude/.credentials.json");
      if (raw && raw.length > 20) {
        var parsed = JSON.parse(raw);
        if (parsed && parsed.claudeAiOauth && parsed.claudeAiOauth.accessToken) {
          hasCreds = true;
        }
      }
    } catch (e) {}

    if (!hasCreds) {
      try {
        var raw2 = system.readFile("/mnt/claude-credentials.json");
        if (raw2 && raw2.length > 20) {
          var parsed2 = JSON.parse(raw2);
          if (parsed2 && parsed2.claudeAiOauth && parsed2.claudeAiOauth.accessToken) {
            system.writeFile("/tmp/.claude/.credentials.json", raw2);
            hasCreds = true;
          }
        }
      } catch (e) {}
    }

    if (hasCreds) {
      mode = "terminal";
      system.jump("terminal:claude");
      return;
    }

    if (wifi && wifi.connected) {
      state = "login";
    } else {
      state = "error";
      error = "connect to wifi first";
    }
    return;
  }

  if (state === "login") {
    ink(220, 220, 230);
    write("press enter to login", { x: 10, y: 50, size: 1, font: font });
    ink(100, 100, 120);
    write("opens a browser for claude auth", { x: 10, y: 68, size: 1, font: font });
    write("esc: back", { x: 10, y: H - 16, size: 1, font: font });

  } else if (state === "waiting") {
    if (!browserLaunched) {
      browserLaunched = true;
      console.log("[code] launching browser for auth");

      // Run claude auth login in background — it starts a localhost
      // callback server. Then open the browser to Claude's OAuth page.
      // When auth completes, the callback writes credentials.
      var ok = system.openBrowser(
        "https://claude.ai/login?returnTo=/settings/api"
      );
      console.log("[code] browser returned: " + ok);

      // Check if credentials appeared after browser closed
      var credsNow = false;
      try {
        var raw3 = system.readFile("/tmp/.claude/.credentials.json");
        if (raw3 && raw3.length > 20) {
          var parsed3 = JSON.parse(raw3);
          if (parsed3 && parsed3.claudeAiOauth && parsed3.claudeAiOauth.accessToken) {
            credsNow = true;
            try { system.writeFile("/mnt/claude-credentials.json", raw3); } catch (e) {}
          }
        }
      } catch (e) {}

      if (credsNow) {
        state = "approved";
      } else {
        state = "error";
        error = "login not completed - try again";
        browserLaunched = false;
      }
    } else {
      ink(220, 220, 230);
      write("browser open...", { x: 10, y: 50, size: 1, font: font });
    }

  } else if (state === "approved") {
    ink(80, 255, 120);
    write("logged in!", { x: 10, y: 50, size: 2, font: "matrix" });
    ink(140, 140, 160);
    write("launching code...", { x: 10, y: 80, size: 1, font: font });
    if (frame % 90 === 0) {
      mode = "terminal";
      system.jump("terminal:claude");
    }

  } else if (state === "error") {
    ink(255, 80, 80);
    write(error, { x: 10, y: 50, size: 1, font: font });
    ink(100, 100, 120);
    write("enter: retry  esc: back", { x: 10, y: 70, size: 1, font: font });
  }
}

function act({ event: e, system, wifi }) {
  if (mode === "terminal") return;
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    if (state !== "approved" && state !== "waiting") {
      system.jump("prompt");
    }
  }
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (state === "login") {
      state = "waiting";
      browserLaunched = false;
    }
    if (state === "error") {
      if (wifi && wifi.connected) {
        state = "login";
      } else {
        error = "connect to wifi first";
      }
    }
  }
}

export { boot, paint, act };
