// Generates an image thumbnail of the starting screen of a piece.
// See also: `/thumbnail-server` for an alternate implementation.

// Usage:
// https://aesthetic.computer/thumbnail/widthxheight/command~any~params.jpg

// TODO: Combine this endpoint / share code with icon.

const { builder } = require("@netlify/functions");
import { setTimeout } from "node:timers/promises";
import { shell } from "../../backend/shell.mjs";
const puppeteer = require("puppeteer-core");
const dev = process.env.CONTEXT === "dev";

// Only allow a few given resolutions to prevent spam.
const acceptedResolutions = ["1200x630", "1800x900"]; // og:image, twitter:image

async function handler(event, context) {
  const [resolution, ...filepath] = event.path
    .replace("/thumbnail/", "")
    .split("/"); // yields nxn and the command, if it exists

  shell.log("️🐠 Getting thumbnail...", filepath.join("/"));

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

  const browser = !dev
    ? await puppeteer.connect(ops)
    : await puppeteer.launch(ops);

  const page = await browser.newPage();

  let url;

  if (dev) {
    shell.log("🟡 Development");
    url = "https://localhost:8888"; // This is used for testing pages locally.
  } else {
    url = "https://aesthetic.computer";
  }

  // TODO: Parse this better.
  if (
    filepath[filepath.length - 1].startsWith("wand") &&
    filepath[filepath.length - 1].match(/~/g).length >= 1
  ) {
    if (filepath[filepath.length - 1].match(/~/g).length > 1) {
      const lastIndex = filepath[filepath.length - 1].lastIndexOf("~");
      const trimmed = filepath[filepath.length - 1].slice(0, lastIndex);
      filepath[filepath.length - 1] = trimmed + "~0"; // Make wand thumbnails show up instantly if a wand is run with just 1 parameter (loading a demo).
    } else {
      filepath[filepath.length - 1] += "~0"; // Make wand thumbnails show up instantly if a wand is run with just 1 parameter (loading a demo).
    }
  }

  try {
    await page.goto(
      `${url}/${
        filepath.join("/").replace(".png", "") || ""
      }?preview=${width}x${height}`,
      {
        waitUntil: "networkidle2",
        timeout: 5000,
      },
    );
  } catch {
    shell.log("🔴 Failed to stop networking.");
  }

  try {
    await page.waitForFunction("window.preloaded === true", {
      timeout: 8000,
    });
  } catch {
    shell.log("🔴 Failed window.preloaded timer.");
  }

  await setTimeout(500);

  const buffer = await page.screenshot({ type: "png" });

  await browser.close();

  return {
    statusCode: 200,
    headers: {
      "Content-Type": "image/png",
      "Content-Length": buffer.length.toString(),
    },
    body: buffer.toString("base64"),
    ttl: 60,
    isBase64Encoded: true,
  };
}

exports.handler = builder(handler);
