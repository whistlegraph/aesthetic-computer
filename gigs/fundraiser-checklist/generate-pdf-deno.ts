#!/usr/bin/env -S deno run --allow-net --allow-read --allow-write --allow-env --allow-run

import puppeteer from "https://deno.land/x/puppeteer@16.2.0/mod.ts";

const browser = await puppeteer.launch({
  headless: true,
  args: ['--no-sandbox', '--disable-setuid-sandbox', '--font-render-hinting=none']
});

const page = await browser.newPage();
await page.goto('http://localhost:8000/layout-test.html', {
  waitUntil: 'networkidle2'
});

await page.pdf({
  path: 'fundraiser-poster.pdf',
  format: 'Letter',
  printBackground: true,
  margin: {
    top: '0',
    right: '0',
    bottom: '0',
    left: '0'
  }
});

await browser.close();
console.log('✓ PDF generated with Puppeteer: fundraiser-poster.pdf');
