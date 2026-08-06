#!/usr/bin/env node
// Bounded visual evidence for Loopboy/DM workflows.
// URL captures accept one ordinary CSS selector and apply Captutor's spotlight.
// Frame captures remain read-only and may be cropped to an explicit rectangle.

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { dirname, extname, isAbsolute, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer";
import { spotlight } from "../../captutor/lib/effects.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const FRAME = join(HERE, "frame.mjs");
const STATE_DIR = join(homedir(), ".local", "share", "slab", "visual-evidence");

function option(args, name, fallback = null) {
  const index = args.indexOf(name);
  return index >= 0 ? args[index + 1] : fallback;
}

function numberOption(args, name, fallback) {
  const value = Number(option(args, name, fallback));
  if (!Number.isFinite(value) || value <= 0) throw new Error(`${name} must be a positive number`);
  return value;
}

function outputPath(requested, prefix, extension = ".jpg") {
  const path = requested
    ? (isAbsolute(requested) ? requested : resolve(requested))
    : join(STATE_DIR, `${prefix}-${Date.now()}${extension}`);
  mkdirSync(dirname(path), { recursive: true });
  return path;
}

function receipt(path, extra = {}) {
  const bytes = readFileSync(path);
  return {
    ...extra,
    path,
    bytes: bytes.length,
    sha256: createHash("sha256").update(bytes).digest("hex"),
  };
}

function boundedClip(rects, viewport, margin = 44) {
  const valid = rects.filter((r) => r && r.width > 0 && r.height > 0);
  if (!valid.length) throw new Error("capture target has no visible bounds");
  let left = Math.min(...valid.map((r) => r.x)) - margin;
  let top = Math.min(...valid.map((r) => r.y)) - margin;
  let right = Math.max(...valid.map((r) => r.x + r.width)) + margin;
  let bottom = Math.max(...valid.map((r) => r.y + r.height)) + margin;
  const minimumWidth = Math.min(640, viewport.width);
  const minimumHeight = Math.min(420, viewport.height);
  if (right - left < minimumWidth) {
    const grow = (minimumWidth - (right - left)) / 2;
    left -= grow; right += grow;
  }
  if (bottom - top < minimumHeight) {
    const grow = (minimumHeight - (bottom - top)) / 2;
    top -= grow; bottom += grow;
  }
  left = Math.max(0, Math.min(left, viewport.width - minimumWidth));
  top = Math.max(0, Math.min(top, viewport.height - minimumHeight));
  right = Math.min(viewport.width, Math.max(right, left + minimumWidth));
  bottom = Math.min(viewport.height, Math.max(bottom, top + minimumHeight));
  return {
    x: Math.round(left), y: Math.round(top),
    width: Math.round(right - left), height: Math.round(bottom - top),
  };
}

async function captureUrl(args) {
  const rawUrl = option(args, "--url");
  const selector = option(args, "--selector");
  const label = String(option(args, "--label", "Look here")).trim().slice(0, 80);
  if (!rawUrl || !selector) throw new Error("url capture requires --url and --selector");
  if (/^(js|text)=/i.test(selector)) throw new Error("only an ordinary CSS selector is allowed");
  let url;
  try { url = new URL(rawUrl); } catch { throw new Error("--url must be a valid URL"); }
  if (!new Set(["http:", "https:"]).has(url.protocol)) throw new Error("--url must use http or https");
  const width = Math.min(1920, Math.round(numberOption(args, "--width", 1280)));
  const height = Math.min(1200, Math.round(numberOption(args, "--height", 900)));
  const path = outputPath(option(args, "--out"), "url", ".jpg");
  const executablePath = existsSync("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
    ? "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    : undefined;
  const browser = await puppeteer.launch({ headless: true, executablePath });
  try {
    const page = await browser.newPage();
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.goto(url.href, { waitUntil: "networkidle2", timeout: 45000 });
    await page.waitForSelector(selector, { visible: true, timeout: 15000 });
    const cdp = { eval: (expression) => page.evaluate(expression) };
    await spotlight(cdp, selector, {
      label,
      durationMs: 0,
      dim: 0.58,
      padding: 14,
      scrollIntoView: true,
    });
    await new Promise((resolveWait) => setTimeout(resolveWait, 850));
    const bounds = await page.evaluate((targetSelector) => {
      const box = (node) => {
        if (!node) return null;
        const r = node.getBoundingClientRect();
        return { x: r.x, y: r.y, width: r.width, height: r.height };
      };
      return {
        target: box(document.querySelector(targetSelector)),
        label: box(document.querySelector("#__captutor_fx")?.shadowRoot?.querySelector(".label")),
        title: document.title,
      };
    }, selector);
    const clip = boundedClip([bounds.target, bounds.label], { width, height });
    // Chrome's clipped screenshot path can drop fixed shadow-DOM overlays
    // after scrollIntoView. Capture the viewport first, then crop the pixels;
    // this guarantees the Captutor ring and label survive in the artifact.
    const temporary = mkdtempSync(join(tmpdir(), "visual-url-"));
    try {
      const viewportPath = join(temporary, "viewport.jpg");
      await page.screenshot({ path: viewportPath, type: "jpeg", quality: 91 });
      execFileSync("/usr/bin/sips", [
        "-c", String(clip.height), String(clip.width),
        "--cropOffset", String(clip.y), String(clip.x),
        viewportPath, "--out", path,
      ], { encoding: "utf8" });
    } finally {
      rmSync(temporary, { recursive: true, force: true });
    }
    return receipt(path, {
      kind: "url",
      url: url.href,
      selector,
      label,
      pageTitle: bounds.title,
      target: bounds.target,
      labelBounds: bounds.label,
      clip,
    });
  } finally {
    await browser.close();
  }
}

function parseRegion(value) {
  if (!value) return null;
  const parts = String(value).split(",").map(Number);
  if (parts.length !== 4 || parts.some((n) => !Number.isFinite(n)) || parts.some((n) => n < 0) || parts[2] <= 0 || parts[3] <= 0) {
    throw new Error("--region must be x,y,width,height in image pixels");
  }
  return parts.map(Math.round);
}

function captureFrame(args) {
  const machine = option(args, "--machine", "local");
  const region = parseRegion(option(args, "--region"));
  const path = outputPath(option(args, "--out"), "frame", ".jpg");
  const temporary = region ? mkdtempSync(join(tmpdir(), "visual-frame-")) : null;
  const rawPath = temporary ? join(temporary, `raw${extname(path) || ".jpg"}`) : path;
  try {
    const raw = execFileSync(process.execPath, [
      FRAME, machine, "--no-ocr", "--quiet-overlay", "--json", "--out", rawPath,
    ], { encoding: "utf8", maxBuffer: 32 * 1024 * 1024 });
    const frame = JSON.parse(raw.trim() || "{}");
    if (region) {
      const [x, y, width, height] = region;
      execFileSync("/usr/bin/sips", [
        "-c", String(height), String(width), "--cropOffset", String(y), String(x),
        rawPath, "--out", path,
      ], { encoding: "utf8" });
    }
    return receipt(path, {
      kind: "frame",
      machine,
      region,
      frontmost: frame?.meta?.frontmost || null,
      window: (frame?.meta?.windows || [])[0] || null,
    });
  } finally {
    if (temporary) rmSync(temporary, { recursive: true, force: true });
  }
}

const [mode, ...args] = process.argv.slice(2);
try {
  const result = mode === "url"
    ? await captureUrl(args)
    : mode === "frame"
      ? captureFrame(args)
      : (() => { throw new Error("usage: visual-evidence.mjs <url|frame> [options]"); })();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
} catch (error) {
  process.stderr.write(`${error.message || error}\n`);
  process.exit(1);
}
