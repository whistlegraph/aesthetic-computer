// laklok-touch-probe — phone-shaped headless run of the raster piece with
// synthesized touch scrolls, to reproduce the "last message hides under the
// input bar" report. Modeled on toolchain/laklok-sisters/sisters.mjs.
// Usage: node tmp/laklok-touch-probe.mjs <outPrefix> [url]
import puppeteer from "puppeteer";

const OUT = process.argv[2] || "/tmp/laklok";
const URL_ = process.argv[3] || "https://aesthetic.computer/laklok";

const executablePath =
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const browser = await puppeteer.launch({
  executablePath,
  headless: "new",
  acceptInsecureCerts: true,
  args: [
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--mute-audio",
    "--ignore-certificate-errors",
  ],
});

try {
  const page = await browser.newPage();
  await page.setViewport({
    width: 390,
    height: 844,
    deviceScaleFactor: 2,
    isMobile: true,
    hasTouch: true,
  });

  // Capture 📜🔍 debug lines from the page AND its workers (the disk runs in
  // a worker, whose console surfaces through auto-attached sessions).
  const logLine = (txt) => {
    if (txt.includes("📜🔍") || /error/i.test(txt)) console.log(txt.slice(0, 300));
  };
  page.on("console", (msg) => logLine(msg.text()));
  page.on("pageerror", (e) => console.log("PAGEERROR:", String(e).slice(0, 300)));
  const cdpB = await page.browserContext().browser().target().createCDPSession();
  await cdpB.send("Target.setAutoAttach", {
    autoAttach: true,
    waitForDebuggerOnStart: false,
    flatten: true,
  });
  cdpB.on("sessionattached", async (session) => {
    try {
      await session.send("Runtime.enable");
      session.on("Runtime.consoleAPICalled", (ev) => {
        const txt = (ev.args || [])
          .map((a) => a.value ?? a.description ?? "")
          .join(" ");
        logLine(txt);
      });
    } catch {}
  });

  await page.goto(URL_, { waitUntil: "domcontentloaded", timeout: 60000 });
  await sleep(30000); // AC boot + chat connect + first paint (local cold start is slow)
  await page.screenshot({ path: `${OUT}-rest.png` });
  console.log("--- rest shot taken");

  const cdp = await page.createCDPSession();
  const drag = async (x, y0, y1, steps = 14) => {
    await cdp.send("Input.dispatchTouchEvent", {
      type: "touchStart",
      touchPoints: [{ x, y: y0 }],
    });
    for (let i = 1; i <= steps; i++) {
      await cdp.send("Input.dispatchTouchEvent", {
        type: "touchMove",
        touchPoints: [{ x, y: y0 + ((y1 - y0) / steps) * i }],
      });
      await sleep(16);
    }
    await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
  };

  console.log("--- drag up into history");
  await drag(195, 300, 700); // finger down → scroll up into history
  await sleep(1500);
  await page.screenshot({ path: `${OUT}-history.png` });

  console.log("--- drag back toward newest (x2, past edge)");
  await drag(195, 700, 200); // finger up → back toward newest
  await sleep(200);
  await drag(195, 700, 150); // and past the edge (elastic)
  await sleep(3000); // settle
  await page.screenshot({ path: `${OUT}-bottom.png` });
  console.log("--- bottom shot taken");
} finally {
  await browser.close();
}
