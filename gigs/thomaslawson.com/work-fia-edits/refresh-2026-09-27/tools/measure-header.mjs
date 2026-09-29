// measure-header.mjs — where the site header's logo/nav sit vs the split header.
import puppeteer from "puppeteer-core";
const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const b = await puppeteer.launch({ executablePath: chrome, headless: "new" });
for (const w of [1440, 1920, 1280, 390]) {
  const p = await b.newPage();
  await p.setViewport({ width: w, height: 900 });
  await p.goto("https://www.thomaslawson.com/bookshelf/?nc=" + Date.now(), { waitUntil: "networkidle2" });
  const r = await p.evaluate(() => {
    const box = (el) => el && (({ x, y, width, height }) => ({ x: Math.round(x), y: Math.round(y), w: Math.round(width), h: Math.round(height) }))(el.getBoundingClientRect());
    const logo = document.querySelector("header img, .elementor-location-header img, [data-elementor-type=header] img");
    const hdr = document.querySelector("[data-elementor-type=header], .elementor-location-header, header");
    const inner = hdr && hdr.querySelector(".e-con-inner, .elementor-container");
    return { logo: box(logo), header: box(hdr), headerInner: box(inner), split: box(document.querySelector(".tl-split")), media: box(document.querySelector(".tl-split-media")), text: box(document.querySelector(".tl-split-text")) };
  });
  console.log(w, JSON.stringify(r));
  await p.close();
}
await b.close();
