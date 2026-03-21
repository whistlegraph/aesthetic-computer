// Generates an image thumbnail of the starting screen of a piece.
// See also: `/thumbnail-server` for an alternate implementation.

// Fastify docs: https://www.fastify.io/docs/latest/Guides/Getting-Started

// https://aesthetic.computer/thumbnail/widthxheight/command~any~params.jpg

import Fastify from 'fastify'

const fastify = Fastify({
  logger: true
})

import { chromium } from 'playwright';
const acceptedResolutions = ["1200x630", "1800x900"]; // og:image, twitter:image

fastify.get('/thumbnail/:resolution/:command.jpg', async (request, reply) => {

  const { resolution, command } = request.params;

  // Parse "IntxInt" to get the correct resolution to take a screenshot by.
  const [width, height] = resolution.split("x").map((n) => parseInt(n));

  // Ditch if we don't hit the accepted resolution whitelist.
  if (acceptedResolutions.indexOf(resolution) === -1) {
    reply.code(500);
    reply.send("error");
  }

  // Option 2: Playwright
  const browser = await chromium.launch({ channel: "chrome" });

  const context = await browser.newContext({
    viewport: {
      width: Math.ceil(width / 2),
      height: Math.ceil(height / 2),
    },
    deviceScaleFactor: 2,
  });

  const page = await context.newPage();

  page.on("console", (message) => {
    console.log(message.text());
  });

  page.on("pageerror", (err) => {
    console.log(err.message);
  });

  // TODO: Rewrite the URL below so that I can test locally without hitting
  //       aesthetic.computer's production deployment. 22.07.17.22.30
  //       - `https://${event.headers['x-forwarded-host']}/${command || ""}`
 
  try {
    await page.goto(`https://aesthetic.computer/${command || ""}`, {
      waitUntil: "networkidle",
      timeout: 3000
    });
  } catch {
    console.log("Failed idle network...");
  }

  // Add a potential extra 2 seconds until preloading is ready.
  try {
    await page.waitForFunction(() => preloaded === true, {timeout: 3000});
  } catch {
    console.log("Failed preloaded check...");
  }

  console.log("Waiting for 1s...");
  await page.waitForTimeout(1000); // A bit of extra time.

  const buffer = await page.screenshot();

  const buffer = await page.screenshot({
    type: "jpeg",
    quality: 80
  });

  await browser.close();

  reply.headers({
      "Content-Type": "image/jpeg",
      "Content-Length": buffer.length.toString(),
  });

  reply.code(200);
  reply.send(buffer);
})

const start = async () => {
  try {
    await fastify.listen({ port: 8081 })
  } catch (err) {
    fastify.log.error(err)
    process.exit(1)
  }
}

start();
