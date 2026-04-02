#!/usr/bin/env node

import fs from "node:fs/promises";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer";
import QRCode from "qrcode";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const codes = process.argv.slice(2);
const cardCodes = codes.length ? codes : ["kl1", "24m", "duv"];
const sourceWrapColumn = 34;
const screenshotSize = 1000;
const ovenFrameDelayMs = 2200;

const browser = await puppeteer.launch({
  headless: true,
  executablePath: process.env.PUPPETEER_EXECUTABLE_PATH || "/usr/sbin/chromium-browser",
  args: ["--no-sandbox", "--disable-setuid-sandbox"],
});

for (const code of cardCodes) {
  const cardData = await fetchCardData(code);
  await captureScreenshot(browser, cardData);
  await writeQrCode(cardData);
  await writeCardTex(cardData);
  renderPdf(cardData.code);
  console.log(`wrote ${cardData.code}.pdf`);
}

await browser.close();

async function fetchCardData(code) {
  const response = await fetch(
    `https://aesthetic.computer/api/store-kidlisp?code=${encodeURIComponent(code)}`,
  );

  if (!response.ok) {
    throw new Error(`Failed to fetch $${code}: ${response.status} ${response.statusText}`);
  }

  const payload = await response.json();
  if (!payload.source || !payload.ipfsMedia?.thumbnailUri) {
    throw new Error(`Missing source or thumbnail metadata for $${code}`);
  }

  return {
    code,
    source: payload.source,
  };
}

async function captureScreenshot(browser, { code }) {
  const screenshotPath = path.join(__dirname, `${code}-screenshot.png`);
  const ovenUrl = new URL(
    `/grab/webp/${screenshotSize}/${screenshotSize}/${encodeURIComponent(`$${code}`)}`,
    "https://oven.aesthetic.computer",
  );
  ovenUrl.searchParams.set("duration", "2200");
  ovenUrl.searchParams.set("fps", "8");
  ovenUrl.searchParams.set("quality", "90");
  ovenUrl.searchParams.set("density", "1");
  ovenUrl.searchParams.set("nowait", "true");
  ovenUrl.searchParams.set("source", "sosoft-card");

  const page = await browser.newPage();
  page.setDefaultTimeout(120000);
  page.setDefaultNavigationTimeout(120000);
  await page.setViewport({
    width: screenshotSize,
    height: screenshotSize,
    deviceScaleFactor: 1,
  });
  await page.setContent(
    `<!doctype html>
    <html>
      <body style="margin:0;background:#fff;width:${screenshotSize}px;height:${screenshotSize}px;overflow:hidden">
        <img
          id="piece"
          src="${ovenUrl.toString()}"
          width="${screenshotSize}"
          height="${screenshotSize}"
          style="display:block;width:${screenshotSize}px;height:${screenshotSize}px"
        />
      </body>
    </html>`,
    { waitUntil: "domcontentloaded", timeout: 120000 },
  );
  await page.waitForFunction(() => {
    const piece = document.getElementById("piece");
    return piece && piece.complete && piece.naturalWidth > 0;
  }, { timeout: 120000 });
  await new Promise((resolve) => setTimeout(resolve, ovenFrameDelayMs));
  await page.screenshot({
    path: screenshotPath,
    clip: { x: 0, y: 0, width: screenshotSize, height: screenshotSize },
  });
  await page.close();
}

async function writeQrCode({ code }) {
  await QRCode.toFile(
    path.join(__dirname, `qr-${code}.png`),
    `https://aesthetic.computer/${code}`,
    {
      margin: 0,
      width: 512,
      color: {
        dark: "#000000",
        light: "#ffffff",
      },
    },
  );
}

async function writeCardTex({ code, source }) {
  const sourceRows = wrapSource(source)
    .map((line) => `\\rule{0pt}{11pt}${escapeTex(line)}\\\\`)
    .join("\n");

  const tex = `% Generated KidLisp score card for $${code}
\\documentclass[12pt]{article}
\\usepackage[
  paperwidth=2.75in,
  paperheight=4.75in,
  top=0.3in,
  bottom=0.3in,
  left=0.3in,
  right=0.3in,
]{geometry}
\\usepackage{fontspec}
\\usepackage{xcolor}
\\usepackage{tikz}
\\usepackage{graphicx}

\\pagestyle{empty}

\\newfontfamily\\cardcodefont[
  Path=fonts/,
  Extension=.ttf,
  UprightFont=ComicRelief-Regular,
  BoldFont=ComicRelief-Bold,
]{ComicRelief-Regular}
\\newfontfamily\\cardlabelfont[
  Path=fonts/,
  Extension=.ttf,
  UprightFont=ComicRelief-Bold,
  BoldFont=ComicRelief-Bold,
  FakeBold=1.8,
]{ComicRelief-Bold}

\\definecolor{ink}{RGB}{0,0,0}
\\newcommand{\\safetyinset}{0.22in}
\\newcommand{\\safewidth}{2.31in}

\\begin{document}

\\vspace*{-0.26in}

\\noindent\\hspace*{-0.08in}%
\\begin{tikzpicture}
  \\clip (0,0) rectangle (\\safewidth,\\safewidth);
  \\node[anchor=south west, inner sep=0pt] at (0,0)
    {\\includegraphics[width=\\safewidth, height=\\safewidth]{${escapeTex(code)}-screenshot.png}};
  \\draw[black, line width=2.4pt] (0,0) rectangle (\\safewidth,\\safewidth);
\\end{tikzpicture}\\par

\\vspace{0.03in}

\\begingroup\\offinterlineskip
\\cardcodefont\\bfseries\\fontsize{9.5pt}{10.5pt}\\selectfont\\color{ink}
\\noindent\\hspace*{-0.08in}\\begin{tabular}{@{}l@{}}
${sourceRows}
\\end{tabular}
\\endgroup

\\vspace*{\\fill}

\\begin{tikzpicture}[remember picture, overlay]
  \\node[anchor=south east, inner sep=0pt]
    at ([xshift=-0.23in, yshift=0.75in]current page.south east)
    {\\colorbox{black}{%
      \\rule[-1pt]{0pt}{6.5pt}%
      \\kern-1pt{\\cardlabelfont\\bfseries\\fontsize{8pt}{8pt}\\selectfont\\color{white}\\$${escapeTex(code)}}\\kern-0.5pt%
    }};
  \\node[anchor=south east, inner sep=1pt, fill=black]
    at ([xshift=-\\safetyinset, yshift=\\safetyinset]current page.south east)
    {\\rule{0.48in}{0.48in}};
  \\node[anchor=south east, inner sep=1pt, fill=white]
    at ([xshift=-0.24in, yshift=0.24in]current page.south east)
    {\\includegraphics[width=0.48in]{qr-${escapeTex(code)}.png}};
\\end{tikzpicture}

\\end{document}
`;

  await fs.writeFile(path.join(__dirname, `${code}.tex`), tex);
}

function renderPdf(code) {
  const args = ["-interaction=nonstopmode", "-halt-on-error", `${code}.tex`];
  execFileSync("xelatex", args, { cwd: __dirname, stdio: "ignore" });
  execFileSync("xelatex", args, { cwd: __dirname, stdio: "ignore" });
  execFileSync(
    "pdftoppm",
    ["-f", "1", "-singlefile", "-r", "200", "-png", `${code}.pdf`, code],
    { cwd: __dirname, stdio: "ignore" },
  );
}

function wrapSource(source) {
  return source
    .split(/\n+/)
    .flatMap((line) => wrapLine(line.trim()))
    .filter(Boolean);
}

function wrapLine(line) {
  if (line.length <= sourceWrapColumn) {
    return [line];
  }

  const commaParts = line.split(/\s*,\s*/).filter(Boolean);
  if (commaParts.length > 1) {
    return commaParts.flatMap((part) => wrapWords(part));
  }

  return wrapWords(line);
}

function wrapWords(line) {
  if (line.length <= sourceWrapColumn) {
    return [line];
  }

  const wrapped = [];
  let current = "";
  for (const word of line.split(/\s+/)) {
    const candidate = current ? `${current} ${word}` : word;
    if (candidate.length > sourceWrapColumn && current) {
      wrapped.push(current);
      current = word;
    } else {
      current = candidate;
    }
  }
  if (current) {
    wrapped.push(current);
  }
  return wrapped;
}

function escapeTex(value) {
  return value.replace(/[\\{}#$%&_~^]/g, (char) => {
    switch (char) {
      case "\\":
        return "\\textbackslash{}";
      case "{":
        return "\\{";
      case "}":
        return "\\}";
      case "#":
        return "\\#";
      case "$":
        return "\\$";
      case "%":
        return "\\%";
      case "&":
        return "\\&";
      case "_":
        return "\\_";
      case "~":
        return "\\textasciitilde{}";
      case "^":
        return "\\textasciicircum{}";
      default:
        return char;
    }
  });
}
