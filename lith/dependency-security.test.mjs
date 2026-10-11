// Run after production dependency installs in lith/, system/, and oven/.
// Browser smoke is opt-in via PUPPETEER_EXECUTABLE_PATH; no download is needed.
import assert from "node:assert/strict";
import { createRequire } from "node:module";
import test from "node:test";
import { pathToFileURL } from "node:url";
import { teiaPackage } from "../system/backend/whistlegraph-teia.mjs";

const lith = createRequire(new URL("./package.json", import.meta.url));
const system = createRequire(new URL("../system/package.json", import.meta.url));
const oven = createRequire(new URL("../oven/package.json", import.meta.url));

for (const [name, require] of [["lith", lith], ["oven", oven]]) {
  test(`${name}: malformed mapped-IPv6 trust subnet does not trust public IPv4`, () => {
    const proxyaddr = require("proxy-addr");
    assert.equal(proxyaddr.compile(["::ffff:10.0.0.0/8"])("198.51.100.23", 0), false);
    assert.equal(proxyaddr.compile(["10.0.0.0/8"])("10.0.0.23", 0), true);
  });
}

for (const [name, require] of [["lith", lith], ["oven", oven]]) {
  test(`${name}: Express parses JSON and still rejects oversized request bodies`, async () => {
    const express = require("express");
    const app = express();
    app.use(express.json({ limit: "64b" }));
    app.post("/fixture", (req, res) => res.json(req.body));
    app.use((err, req, res, next) => res.status(err.status || 500).end());
    const server = await new Promise(resolve => {
      const listener = app.listen(0, "127.0.0.1", () => resolve(listener));
    });
    const url = `http://127.0.0.1:${server.address().port}/fixture`;
    try {
      const ok = await fetch(url, { method: "POST", headers: { "Content-Type": "application/json" }, body: '{"fixture":true}' });
      assert.deepEqual(await ok.json(), { fixture: true });
      const oversized = await fetch(url, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ text: "x".repeat(100) }) });
      assert.equal(oversized.status, 413);
    } finally {
      server.closeAllConnections();
      await new Promise(resolve => server.close(resolve));
    }
  });
}

test("outbound mail composes and inbound SMTP parses a local multipart message", async () => {
  const { SMTPServer } = lith("smtp-server");
  const { simpleParser } = lith("mailparser");
  const nodemailer = system("nodemailer");
  let received;
  const server = new SMTPServer({
    authOptional: true, disabledCommands: ["STARTTLS"],
    onData(stream, session, callback) {
      simpleParser(stream, { skipImageLinks: true }).then(message => {
        received = message;
        callback();
      }, callback);
    },
  });
  await new Promise(resolve => server.listen(0, "127.0.0.1", resolve));
  const transport = nodemailer.createTransport({
    host: "127.0.0.1", port: server.server.address().port,
    secure: false, ignoreTLS: true, connectionTimeout: 2000,
  });
  try {
    const result = await transport.sendMail({
      from: { name: "Fixture via AC", address: "sender@example.invalid" },
      to: "recipient@example.invalid", replyTo: "reply@example.invalid",
      subject: "Fixture ✓", text: "Hello fixture", html: "<p>Hello fixture</p>",
      attachments: [{ filename: "fixture.txt", content: Buffer.from("fixture bytes") }],
    });
    assert.deepEqual(result.accepted, ["recipient@example.invalid"]);
    assert.equal(received.subject, "Fixture ✓");
    assert.equal(received.replyTo.value[0].address, "reply@example.invalid");
    assert.match(received.text, /Hello fixture/);
    assert.equal(received.attachments[0].content.toString(), "fixture bytes");
  } finally {
    transport.close();
    await new Promise(resolve => server.close(resolve));
  }
});

for (const [name, require] of [["system", system], ["oven", oven]]) {
  test(`${name}: ZIP package remains readable and truncated declared size is rejected`, () => {
    const AdmZip = require("adm-zip");
    const pack = teiaPackage("<html><head></head><body>fixture</body></html>", {
      gif: Buffer.from("fixture gif"), thumbnail: Buffer.from("fixture png"), frames: 2,
    });
    const zip = new AdmZip(pack.zip);
    assert.deepEqual(zip.getEntries().map(entry => entry.entryName).sort(),
      ["cover.gif", "index.html", "thumbnail.png"]);
    assert.match(zip.readAsText("index.html"), /Content-Security-Policy/);
    const entry = zip.getEntry("index.html");
    entry.header.size = 0;
    assert.throws(() => entry.getData());
  });
  test(`${name}: image resizing and SVG rasterization preserve supported formats`, async () => {
    const module = require("sharp");
    const sharp = module.default || module;
    const svg = Buffer.from('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="8"><rect width="16" height="8" fill="red"/></svg>');
    for (const format of ["png", "jpeg", "webp"]) {
      const bytes = await sharp(svg).resize(8, 4).toFormat(format).toBuffer();
      const meta = await sharp(bytes).metadata();
      assert.equal(meta.width, 8);
      assert.equal(meta.height, 4);
      assert.equal(meta.format, format);
    }
  });
}

test("Oven browser captures a page and exposes the connected property", {
  skip: !process.env.PUPPETEER_EXECUTABLE_PATH,
  timeout: 15000,
}, async () => {
  const { default: puppeteer } = await import(pathToFileURL(oven.resolve("puppeteer")).href);
  const browser = await puppeteer.launch({
    headless: true, executablePath: process.env.PUPPETEER_EXECUTABLE_PATH,
    args: ["--disable-background-networking", "--disable-component-update"],
  });
  try {
    assert.equal(browser.connected, true);
    const page = await browser.newPage();
    await page.setViewport({ width: 32, height: 32 });
    await page.setContent('<html><body style="background:red">fixture</body></html>');
    const png = await page.screenshot({ type: "png" });
    assert.ok(png.length > 20);
  } finally {
    await browser.close();
  }
  assert.equal(browser.connected, false);
});
