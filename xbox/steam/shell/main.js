// The Steam shell for oskiewar: an Electron window around the same browser
// runtime oskiewar.com serves and the reel factory renders three times a day.
// Nothing here is a fourth runtime — the game file is byte-identical to the
// one xbox/live serves, and this process only supplies the host it expects.

const { app, BrowserWindow, protocol, net } = require("electron");
const { readFileSync } = require("node:fs");
const { extname, join, normalize, resolve } = require("node:path");
const { pathToFileURL } = require("node:url");

// Packaged, the runtime lives beside the app (stage.mjs put it there) and the
// out-of-tree assets have already been flattened into the same root. In the
// working tree we read xbox/live directly, so a save is the next reload.
const packaged = app.isPackaged;
const staged = join(process.resourcesPath || "", "staged");
const live = packaged ? staged : resolve(__dirname, "../../live");
const repo = packaged ? staged : resolve(live, "../..");

// The shell reaches outside xbox/live for the QR encoder, analytics, cursors
// and the typeface; oskiewar.com resolves those through lith. Kept in step
// with xbox/tools/serve-live.mjs — if that map grows, this one does too.
const elsewhere = new Map([
  ["/aesthetic.computer/dep/@akamfoad/qr/qr.mjs",
    "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs"],
  ["/aesthetic.computer/lib/product-analytics.mjs",
    "system/public/aesthetic.computer/lib/product-analytics.mjs"],
  ["/aesthetic.computer/lib/oskiewar-analytics.mjs",
    "system/public/aesthetic.computer/lib/oskiewar-analytics.mjs"],
  ["/aesthetic.computer/cursors/precise.svg",
    "system/public/aesthetic.computer/cursors/precise.svg"],
  ["/aesthetic.computer/cursors/active.svg",
    "system/public/aesthetic.computer/cursors/active.svg"],
  ["/ComicRelief-Regular.ttf",
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf"],
]);

// A module script will not load over a scheme Chromium treats as opaque, and
// the page is all modules, so app:// has to be declared before app ready.
protocol.registerSchemesAsPrivileged([{
  scheme: "app",
  privileges: { standard: true, secure: true, supportFetchAPI: true, stream: true },
}]);

function fileFor(pathname) {
  if (pathname === "/" || pathname === "/index.html")
    return join(live, packaged ? "index.html" : "mac-test.html");
  // stage.mjs already flattened the out-of-tree six to their URL paths, so a
  // packaged build needs no map — everything resolves under one root.
  if (!packaged) {
    const mapped = elsewhere.get(pathname);
    if (mapped) return join(repo, mapped);
  }
  const target = normalize(join(live, pathname));
  return target.startsWith(live) ? target : "";
}

async function serve(request) {
  const { pathname } = new URL(request.url);
  // The page asks for its analytics config at boot and tolerates a failure,
  // but an empty object is quieter than a 404 in the console every launch.
  if (pathname === "/api/product-analytics-config")
    return new Response("{}", { headers: { "content-type": "application/json" } });
  const path = fileFor(pathname);
  if (!path) return new Response("outside xbox/live", { status: 403 });
  try {
    return await net.fetch(pathToFileURL(path).toString());
  } catch {
    // Every remote the page reaches for is wrapped in a catch, so a miss is
    // survivable — but an unnamed one is how an asset silently stops shipping.
    console.log(`[shell] missing: ${pathname}`);
    return new Response("not found", { status: 404 });
  }
}

// Steam's overlay hooks the GPU present call, which Electron's out-of-process
// GPU defeats. steamworks.js applies the in-process-gpu switch for us, but the
// dependency is optional so the shell still runs before anyone has installed
// it — and appid 480 (Spacewar) stands in until oskiewar has its own.
function initSteam() {
  let steamworks;
  try {
    steamworks = require("steamworks.js");
  } catch {
    console.log("[steam] steamworks.js absent — running unwired");
    return null;
  }
  try {
    steamworks.electronEnableSteamOverlay();
    const appId = Number(readFileSync(join(__dirname, "steam_appid.txt"), "utf8").trim());
    const client = steamworks.init(appId);
    console.log(`[steam] initialized against appid ${appId}`);
    return client;
  } catch (error) {
    console.log(`[steam] not initialized: ${error.message}`);
    return null;
  }
}

const steam = initSteam();

app.whenReady().then(() => {
  protocol.handle("app", serve);

  const win = new BrowserWindow({
    width: 1280,
    height: 720,
    useContentSize: true,
    backgroundColor: "#07081c",
    title: "oskiewar",
    webPreferences: {
      preload: join(__dirname, "preload.js"),
      contextIsolation: true,
      sandbox: false,
    },
  });

  // The game pins vertical to 1080 logical units and derives width from the
  // box aspect, so a 16:9 window *is* 1920x1080 to the game. There is no
  // set-resolution host call; this is the whole mechanism.
  win.setAspectRatio(16 / 9);
  win.setMenuBarVisibility(false);
  win.loadURL("app://local/");

  // The frame driver stops on visibilitychange, which a backgrounded desktop
  // window never fires — without this it free-runs behind other apps.
  const visible = (state) =>
    win.webContents.executeJavaScript(
      `globalThis.__oskiewarDriver?.setVisible(${state})`, true).catch(() => {});
  win.on("focus", () => visible(true));
  win.on("blur", () => visible(false));

  if (steam) console.log(`[steam] player: ${steam.localplayer.getName()}`);
});

app.on("window-all-closed", () => app.quit());
