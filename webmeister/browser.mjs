#!/usr/bin/env node
import puppeteer from "puppeteer-core";
import http from "http";

const CDP_HOST = "host.docker.internal";
const CDP_PORT = 9222;

function httpGet(url, headers = {}) {
  return new Promise((resolve, reject) => {
    const u = new URL(url);
    const req = http.request({
      hostname: u.hostname,
      port: u.port,
      path: u.pathname,
      method: "GET",
      headers: { Host: "localhost", ...headers }
    }, (res) => {
      let data = "";
      res.on("data", chunk => data += chunk);
      res.on("end", () => resolve(data));
    });
    req.on("error", reject);
    req.end();
  });
}

async function getWSEndpoint() {
  const data = await httpGet(`http://${CDP_HOST}:${CDP_PORT}/json/version`);
  const json = JSON.parse(data);
  return json.webSocketDebuggerUrl.replace("ws://localhost", `ws://${CDP_HOST}:${CDP_PORT}`);
}

export async function getBrowser() {
  const wsEndpoint = await getWSEndpoint();
  return puppeteer.connect({
    browserWSEndpoint: wsEndpoint,
    defaultViewport: null,
  });
}

export async function listPages() {
  const browser = await getBrowser();
  const pages = await browser.pages();
  const info = await Promise.all(pages.map(async (p, i) => ({
    index: i,
    url: p.url(),
    title: await p.title(),
  })));
  await browser.disconnect();
  return info;
}

export async function getPage(pattern) {
  const browser = await getBrowser();
  const pages = await browser.pages();
  let page = typeof pattern === "number" ? pages[pattern] : pages.find(p => p.url().includes(pattern));
  if (!page) { await browser.disconnect(); throw new Error("No page: " + pattern); }
  return { browser, page };
}

export class PageController {
  constructor(page, browser) { this.page = page; this.browser = browser; }
  static async connect(pattern) {
    const { browser, page } = await getPage(pattern);
    return new PageController(page, browser);
  }
  async goto(url, o = {}) { await this.page.goto(url, { waitUntil: "networkidle2", ...o }); return this; }
  async click(s, o = {}) { await this.page.waitForSelector(s, { timeout: 10000, ...o }); await this.page.click(s); return this; }
  async type(s, text, o = {}) { await this.page.waitForSelector(s, { timeout: 10000 }); await this.page.click(s); await this.page.type(s, text, o); return this; }
  async fill(s, v) { await this.page.waitForSelector(s, { timeout: 10000 }); await this.page.$eval(s, (el, val) => { el.value = val; el.dispatchEvent(new Event("input", { bubbles: true })); el.dispatchEvent(new Event("change", { bubbles: true })); }, v); return this; }
  async getContent() { return this.page.evaluate(() => document.body.innerText); }
  async getHtml() { return this.page.content(); }
  async wait(ms) { await new Promise(r => setTimeout(r, ms)); return this; }
  async waitNav(o = {}) { await this.page.waitForNavigation({ waitUntil: "networkidle2", ...o }); return this; }
  async waitFor(s, o = {}) { await this.page.waitForSelector(s, { timeout: 10000, ...o }); return this; }
  async screenshot(path) { await this.page.screenshot({ path, fullPage: true }); return this; }
  async eval(fn, ...args) { return this.page.evaluate(fn, ...args); }
  url() { return this.page.url(); }
  async title() { return this.page.title(); }
  async close() { await this.browser.disconnect(); }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const [cmd, arg] = process.argv.slice(2);
  if (cmd === "list") {
    const pages = await listPages();
    console.log("Tabs:");
    pages.forEach(p => console.log("  [" + p.index + "] " + p.title.slice(0,40) + " - " + p.url.slice(0,60)));
  } else if (cmd === "content") {
    const ctrl = await PageController.connect(isNaN(arg) ? arg : parseInt(arg) || 0);
    console.log(await ctrl.getContent());
    await ctrl.close();
  } else {
    console.log("Usage: node browser.mjs list | content [pattern]");
  }
}
