// Icon, 23.07.31.12.57
// Generates the favicon for a given piece.

// TODO: ❤️‍🔥 GET PUPPETEER WORKING LOCALLY!!!!!!!!!!!!!!!!

// Usage:
// https://aesthetic.computer/icon/widthxheight/command~any~params.png

const { builder } = require("@netlify/functions");
const puppeteer = require("puppeteer-core");
import { setTimeout } from "node:timers/promises";
const dev = process.env.CONTEXT === "dev";

// Only allow a few given resolutions to prevent spam.
const acceptedResolutions = ["128x128"];

async function handler(event, context) {
  const [resolution, ...filepath] = event.path.replace("/icon/", "").split("/"); // yields nxn and the command, if it exists

  console.log("🖼️ Getting icon...", filepath.join("/"));

  // Ditch if we don't hit the accepted resolution whitelist.
  if (
    acceptedResolutions.indexOf(resolution) === -1 ||
    !filepath[filepath.length - 1].endsWith(".png")
  ) {
    return { statusCode: 500 };
  }

  // Parse "IntxInt" to get the correct resolution to take a screenshot by.
  const [width, height] = resolution.split("x").map((n) => parseInt(n));

  const ops = {
    defaultViewport: {
      width: Math.ceil(width / 2),
      height: Math.ceil(height / 2),
      deviceScaleFactor: 2,
    },
    headless: "true",
  };

  if (dev) ops.ignoreHTTPSErrors = true;
  if (!dev)
    ops.browserWSEndpoint = `wss://chrome.browserless.io?token=${process.env.BROWSERLESS_API_KEY}`;

  console.log("🥰 Launching puppeteer...", ops);

  let browser;

  try {
    browser = !dev ? await puppeteer.connect(ops) : await puppeteer.launch(ops);
  } catch (err) {
    console.log("Error launching puppeteer:", error);
  }

  console.log("🌟 Making new page...");

  const page = await browser.newPage();

  let url;

  if (dev) {
    console.log("🟡 Development");
    url = "https://localhost:8888"; // This is used for testing pages locally.
  } else {
    url = "https://aesthetic.computer";
  }

  try {
    const fullUrl = `${url}/${
      filepath.join("/").replace(".png", "") || ""
    }?icon=${width}x${height}`;
    console.log("📃 Visiting page:", fullUrl);
    await page.goto(fullUrl, { waitUntil: "networkidle2", timeout: 5000 });
  } catch (err) {
    console.log("🔴 Failed to stop networking:", err);
  }

  try {
    await page.waitForFunction("window.preloaded === true", { timeout: 8000 });
  } catch {
    console.log("🔴 Failed window.preloaded timer.");
  }

  await setTimeout(500);

  console.log("🖼️ Taking sceenshot...");

  const buffer = await page.screenshot({ type: "png" });

  console.log("Closing browser...");

  await browser.close();

  return {
    statusCode: 200,
    headers: {
      "Content-Type": "image/png",
      "Content-Length": buffer.length.toString(),
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Resource-Policy": "cross-origin",
    },
    body: buffer.toString("base64"),
    ttl: 60,
    isBase64Encoded: true,
  };
}

exports.handler = builder(handler);
