#!/usr/bin/env node
// Resumable YouTube Studio uploader for projects whose Data API uploads are
// temporarily forced private while their YouTube API audit is pending.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import puppeteer from "puppeteer";

const manifestArg = process.argv[2];
const portArg = process.argv.find((arg) => arg.startsWith("--port="));
const startArg = process.argv.find((arg) => arg.startsWith("--start="));
const limitArg = process.argv.find((arg) => arg.startsWith("--limit="));
const port = Number(portArg?.split("=")[1] || 9444);
const start = Number(startArg?.split("=")[1] || 0);
const limit = Number(limitArg?.split("=")[1] || Infinity);

if (!manifestArg) {
  console.error("usage: node toolchain/youtube/studio-batch.mjs <manifest.json> [--port=9444] [--start=0] [--limit=N]");
  process.exit(1);
}

const manifestPath = resolve(process.cwd(), manifestArg);
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const baseDir = resolve(dirname(manifestPath), manifest.baseDir || ".");
const channelId = manifest.channelId || "UCKmElnk1yfjlIFu13AOQMnA";
const contentUrl = `https://studio.youtube.com/channel/${channelId}/videos/upload`;

const sleep = (ms) => new Promise((resolveSleep) => setTimeout(resolveSleep, ms));
const receiptPath = (filePath) => filePath.replace(/\.[^.]+$/, "") + ".youtube.json";

async function waitEnabled(page, selector, timeout = 180_000) {
  await page.waitForFunction(
    (target) => {
      const element = document.querySelector(target);
      return element && !element.disabled && element.getAttribute("aria-disabled") !== "true";
    },
    { timeout },
    selector,
  );
}

async function clickCenter(page, handle) {
  const box = await handle.boundingBox();
  if (!box) throw new Error("element is not visible");
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2);
}

async function closePublishedDialog(page) {
  const visibleClose = await page.evaluateHandle(() =>
    [...document.querySelectorAll('button[aria-label="Close"]')]
      .find((element) => element.getBoundingClientRect().width > 0),
  );
  if (visibleClose.asElement()) {
    await clickCenter(page, visibleClose.asElement());
    await sleep(500);
  }
}

async function openUploadDialog(page) {
  await page.goto(contentUrl, { waitUntil: "domcontentloaded", timeout: 45_000 });
  await page.bringToFront();
  await sleep(5_000);

  const create = await page.$('button[aria-label="Create"]');
  if (!create) throw new Error("Studio Create button not found");
  await create.click();
  await sleep(250);

  const uploadItem = await page.evaluateHandle(() =>
    [...document.querySelectorAll("tp-yt-paper-item")]
      .find((element) => (element.innerText || "").trim() === "Upload videos"),
  );
  if (!uploadItem.asElement()) throw new Error("Upload videos menu item not found");
  await clickCenter(page, uploadItem.asElement());
  await page.waitForFunction(
    () => document.body.innerText.includes("Drag and drop video files to upload"),
    { timeout: 15_000 },
  );
}

async function uploadFile(page, filePath) {
  const input = await page.$('input[type="file"][name="Filedata"]');
  if (!input) throw new Error("Studio upload file input not found");
  await input.uploadFile(filePath);
  await page.waitForFunction(
    () => document.body.innerText.includes("Title (required)") && document.body.innerText.includes("Video link"),
    { timeout: 60_000 },
  );
  await sleep(750);
}

async function setText(page, selector, value) {
  const field = await page.$(selector);
  if (!field) throw new Error(`field not found: ${selector}`);
  await field.click({ clickCount: 3 });
  await page.keyboard.type(value);
}

async function choosePlaylistAndAudience(page, playlistTitle) {
  await page.evaluate(() => {
    const scroll = document.querySelector("ytcp-uploads-dialog #scrollable-content");
    scroll.scrollTop = 650;
    scroll.dispatchEvent(new Event("scroll"));
  });
  await sleep(350);

  const playlists = await page.$('[aria-label="Select playlists"]');
  if (!playlists) throw new Error("playlist selector not found");
  await playlists.evaluate((element) => element.click());
  await page.waitForFunction(
    (title) => document.body.innerText.includes(title) && document.body.innerText.includes("Done"),
    { timeout: 15_000 },
    playlistTitle,
  );

  const checkbox = await page.evaluateHandle((title) =>
    [...document.querySelectorAll("span")]
      .find((element) => (element.innerText || "").trim() === title)
      ?.closest("label")
      ?.querySelector('[role="checkbox"]'),
  playlistTitle);
  if (!checkbox.asElement()) throw new Error(`playlist not found: ${playlistTitle}`);
  if (await checkbox.asElement().evaluate((element) => element.getAttribute("aria-checked") !== "true")) {
    await clickCenter(page, checkbox.asElement());
  }

  const done = await page.$('button[aria-label="Done"]');
  if (!done) throw new Error("playlist Done button not found");
  await done.evaluate((element) => element.click());
  await sleep(300);

  const notForKids = await page.$('[name="VIDEO_MADE_FOR_KIDS_NOT_MFK"]');
  if (!notForKids) throw new Error("not-for-kids control not found");
  await notForKids.click();
}

async function publishUnlisted(page, title) {
  for (let step = 0; step < 3; step++) {
    await waitEnabled(page, 'button[aria-label="Next"]');
    await (await page.$('button[aria-label="Next"]')).click();
    await sleep(700);
  }

  const unlisted = await page.$('[name="UNLISTED"]');
  if (!unlisted) throw new Error("Unlisted visibility control not found");
  await unlisted.click();
  await waitEnabled(page, 'button[aria-label="Save"]');
  await (await page.$('button[aria-label="Save"]')).click();
  await page.waitForFunction(
    () => document.body.innerText.includes("Video published") || document.body.innerText.includes("Video processing"),
    { timeout: 30_000 },
  );
  const body = await page.evaluate(() => document.body.innerText);
  const url = body.match(/https:\/\/(?:youtu\.be\/|youtube\.com\/shorts\/)([A-Za-z0-9_-]+)/);
  if (body.includes("Video published") && url) return { videoId: url[1], watchUrl: url[0] };

  // Studio may accept Unlisted but hold the completion modal open until SD
  // processing finishes. The row becomes Unlisted automatically; wait for it
  // instead of submitting another upload.
  const processingClose = await page.evaluateHandle(() =>
    [...document.querySelectorAll("button")]
      .find((element) => (element.innerText || "").trim() === "Close" && element.getBoundingClientRect().width > 0),
  );
  if (processingClose.asElement()) await processingClose.asElement().click();
  if (url) {
    await sleep(5_000);
    return { videoId: url[1], watchUrl: url[0] };
  }
  await page.waitForFunction(
    (videoTitle) => {
      const main = document.querySelector("main")?.innerText || document.body.innerText;
      return main.includes(videoTitle) && main.includes("Unlisted");
    },
    { timeout: 180_000, polling: 2_000 },
    title,
  );
  const href = await page.evaluate((videoTitle) =>
    [...document.querySelectorAll("a")]
      .find((element) => (element.innerText || "").trim() === videoTitle)
      ?.getAttribute("href"),
  title);
  const id = href?.match(/\/video\/([A-Za-z0-9_-]+)\/edit/)?.[1];
  if (!id) throw new Error("processed video ID not found");
  return { videoId: id, watchUrl: `https://youtu.be/${id}` };
}

const browser = await puppeteer.connect({ browserURL: `http://127.0.0.1:${port}` });
try {
  const page = (await browser.pages()).find((candidate) => candidate.url().includes("studio.youtube.com"));
  if (!page) throw new Error(`YouTube Studio tab not found on port ${port}`);

  const videos = manifest.videos.slice(start, start + limit);
  for (const [offset, video] of videos.entries()) {
    const index = start + offset;
    const filePath = resolve(baseDir, video.file);
    const output = receiptPath(filePath);
    if (!existsSync(filePath)) throw new Error(`video not found: ${filePath}`);
    if (existsSync(output)) {
      const receipt = JSON.parse(readFileSync(output, "utf8"));
      if (receipt.videoId) {
        console.log(`↷ ${index + 1}/${manifest.videos.length} ${video.file} already published`);
        continue;
      }
    }

    console.log(`▸ ${index + 1}/${manifest.videos.length} ${video.file}`);
    await closePublishedDialog(page);
    console.log("  opening upload dialog");
    await openUploadDialog(page);
    console.log("  uploading file");
    await uploadFile(page, filePath);
    console.log("  applying metadata");
    await setText(page, '[aria-label="Add a title that describes your video (type @ to mention a channel)"]', video.title);
    await setText(page, '[aria-label="Tell viewers about your video (type @ to mention a channel)"]', video.description || "");
    await choosePlaylistAndAudience(page, manifest.playlist.title);
    console.log("  publishing unlisted");
    const published = await publishUnlisted(page, video.title);

    const receipt = {
      channel: manifest.channel,
      channelId,
      playlist: manifest.playlist.title,
      file: video.file,
      title: video.title,
      description: video.description || "",
      language: video.language || manifest.defaults?.language || null,
      privacy: "unlisted",
      madeForKids: false,
      ...published,
      uploadedAt: new Date().toISOString(),
      method: "youtube-studio-cdp",
    };
    writeFileSync(output, JSON.stringify(receipt, null, 2) + "\n");
    console.log(`✓ ${published.watchUrl}`);
  }
} finally {
  await browser.disconnect();
}
