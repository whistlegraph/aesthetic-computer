// build-cv.mjs — Curatorial Projects + Exhibitions, from Fía's CV sheet.
//
// Source: "2026_Lawson, Tom_Website_CV" (Google Sheet 12oiVJ2H…PrpE, sent by
// Fía 2026-09-27), exported to fia-2026-09-27/tl-cv.csv. Re-export the sheet
// to that path to refresh; nothing here edits her rows — titles, venues and
// years print as she typed them. Rows are placed by her Category column:
//   Curatorial Projects  ← any category naming "Curatorial Projects"
//   Exhibitions          ← "Solo Exhibitions" / "Group Exhibitions"
//                          (a row tagged both lands under the first one named)
//
// Exports buildCv() → { pages: { slug: { title, html } }, summary }.
// build-plugin.mjs bakes the HTML into the plugin (server-rendered at
// /curatorial-projects/ and /exhibitions/); `node build-cv.mjs --preview`
// writes preview-cv.js for headless preview on the live site.
import { readFile, writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const UP = "https://www.thomaslawson.com/wp-content/uploads/";

/* Existing site pages carry an image each (their Art in a Broader Context
   index card); the curatorial list reuses it. */
const THUMBS = {
    "art-in-context-dissent": "2023/08/Sondra-Perry-install-768x471.png",
    "art-in-context-hot-coffee": "2023/08/CIA_20111119_6006-768x512.jpg",
    "art-in-context-shimmer": "2023/08/Steven-Hull-color-corrected-768x491.jpg",
    "art-in-context-nostalgia-as-reference": "2023/08/Sherrie-Levine-After-Rodchenko-4-1987-768x931.png",
    "art-in-context-hot-coffee-2": "2023/08/97-hotcoffee-exhibitionimage-2560x-768x581.jpg",
    "art-in-context-livin-in-the-usa": "2023/09/Richards-Jarden-TV-Fragment-Early-Morning-Coffee-laminated-wax-relief-1983-768x394.png",
    "critical-perspectives-art-in-context": "2023/08/SAlome-Blue-Boys-1981-240x200-cm.png",
    "reallife-magazine-presents-whitecolumns-art-in-context": "2023/08/Mark-Innerst-Untitled-Two-Ships-oil-on-board-1984-768x432.jpg",
    "art-in-context-reallife-presents": "2023/08/Greenwood-Robinson-768x509.jpg",
};
/* The CV row for REALLIFE at Nigel Greenwood has no site link, but the site
   has that show's page; match it by venue + year so the entry opens it. */
const EXTRA_LINKS = [
    { year: "1981", venue: /Nigel Greenwood/i, slug: "art-in-context-reallife-presents" },
];

/* Header images, from the site's own library. */
export const HEADERS = {
    /* Not the Dissent view: that file is a slider screenshot with its arrows baked in. */
    "curatorial-projects": { src: UP + "2023/08/CIA_20111119_6006-1536x1024.jpg", w: 1536, h: 1024, alt: "The Experimental Impulse, REDCAT, Los Angeles, 2011" },
    "exhibitions": { src: UP + "2022/06/Suburban-install3-1024x768.jpg", w: 1024, h: 768, alt: "Scenes from a Widespread Conspiracy, Suburban, Chicago, 2002" },
};

function parseCsv(text) {
    const rows = [];
    let row = [], field = "", quoted = false;
    for (let i = 0; i < text.length; i++) {
        const c = text[i];
        if (quoted) {
            if (c === '"' && text[i + 1] === '"') { field += '"'; i++; }
            else if (c === '"') quoted = false;
            else field += c;
        } else if (c === '"') quoted = true;
        else if (c === ",") { row.push(field); field = ""; }
        else if (c === "\n" || c === "\r") {
            if (c === "\r" && text[i + 1] === "\n") i++;
            row.push(field); rows.push(row); row = []; field = "";
        } else field += c;
    }
    if (field || row.length) { row.push(field); rows.push(row); }
    return rows;
}

const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
const tidy = (s) => (s || "").replace(/ /g, " ").replace(/[ \t]+/g, " ").trim();
const lines = (s) => tidy(s).split(/\s*\n\s*/).filter(Boolean).map(esc).join("<br>");
const siteSlug = (url) => {
    const m = /^https?:\/\/(?:www\.)?thomaslawson\.com\/([^/?#]+)\/?$/i.exec(tidy(url));
    return m && !/^wp-content$/i.test(m[1]) ? m[1] : null;
};

export async function readCv() {
    const text = await readFile(resolve(here, "fia-2026-09-27", "tl-cv.csv"), "utf8");
    const [head, ...rows] = parseCsv(text);
    const col = (name) => head.indexOf(name);
    const C = { year: col("Year"), month: col("Month"), title: col("Title"), where: col("Location/Publication"),
        web: col("Website(s)"), page: col("Page on TL site (sometimes PDF)"), cat: col("Category") };
    return rows.filter((r) => r.length > 1).map((r) => ({
        year: tidy(r[C.year]), month: tidy(r[C.month]), title: r[C.title] || "", where: r[C.where] || "",
        web: tidy(r[C.web]).split(/\s+/)[0] || "", page: tidy(r[C.page]),
        cats: tidy(r[C.cat]).split(/\s*,\s*/).filter(Boolean),
    }));
}

function entryLink(e) {
    const slug = siteSlug(e.page) || (EXTRA_LINKS.find((x) => x.year === e.year && x.venue.test(e.where)) || {}).slug || null;
    if (slug) return { href: `/${slug}/`, slug, external: false };
    if (/^https?:\/\//i.test(e.web)) return { href: e.web, slug: null, external: true };
    return null;
}

function split(key, { title, eyebrow, lede }) {
    const h = HEADERS[key];
    return `<header class="tl-split tl-split-static" style="--tl-ar:${h.w}/${h.h}">
<figure class="tl-split-media"><img src="${h.src}" width="${h.w}" height="${h.h}" alt="${esc(h.alt)}" decoding="async"></figure>
<div class="tl-split-text"><a class="tl-eyebrow" href="/beyond-the-studio/">${esc(eyebrow)}</a><h1 class="tl-split-title">${esc(title)}</h1><p class="tl-split-lede">${lede}</p></div>
</header>`;
}

function titleHtml(e, link) {
    const t = tidy(e.title) ? lines(e.title) : "";
    if (!t) return "";
    if (!link) return t;
    return `<a href="${esc(link.href)}"${link.external ? ' class="tl-opens" target="_blank" rel="noopener"' : ""}>${t}</a>`;
}

function whereHtml(e, link, titled) {
    const w = lines(e.where);
    if (titled || !link) return w;
    /* Untitled solo shows: the venue is the entry, so it carries the link. */
    return `<a href="${esc(link.href)}"${link.external ? ' class="tl-opens" target="_blank" rel="noopener"' : ""}>${w}</a>`;
}

export async function buildCv() {
    const cv = await readCv();
    const years = (list) => {
        const ys = list.map((e) => parseInt(e.year, 10)).filter(Number.isFinite);
        return [Math.min(...ys), Math.max(...ys)];
    };

    /* ---- Curatorial Projects ---- */
    const curatorial = cv.filter((e) => e.cats.includes("Curatorial Projects"));
    const [c0, c1] = years(curatorial);
    const curItems = curatorial.map((e) => {
        const link = entryLink(e);
        const thumb = link && link.slug && THUMBS[link.slug];
        const cover = thumb
            ? `<a class="tl-cv-thumb" href="${esc(link.href)}" tabindex="-1" aria-hidden="true"><img src="${UP + thumb}" alt="" loading="lazy" decoding="async"></a>`
            : `<span class="tl-cv-thumb tl-cv-thumb-empty" aria-hidden="true"></span>`;
        return `<article class="tl-cv-card">${cover}<div class="tl-cv-card-text"><p class="tl-cv-year">${esc(e.year)}</p><h2 class="tl-cv-title">${titleHtml(e, link)}</h2><p class="tl-cv-where">${lines(e.where)}</p></div></article>`;
    }).join("\n");
    const curHtml = split("curatorial-projects", {
        title: "Curatorial Projects", eyebrow: "Beyond the Studio",
        lede: `${curatorial.length} exhibitions curated by Thomas Lawson, ${c0}&thinsp;&ndash;&thinsp;${c1}.`,
    }) + `\n<section class="tl-cv-body tl-cv-cards">\n${curItems}\n</section>`;

    /* ---- Exhibitions ---- */
    const first = (e) => e.cats.find((c) => c === "Solo Exhibitions" || c === "Group Exhibitions");
    const solo = cv.filter((e) => first(e) === "Solo Exhibitions");
    const group = cv.filter((e) => first(e) === "Group Exhibitions");
    const [e0, e1] = years([...solo, ...group]);
    const list = (entries) => {
        let last = null;
        return entries.map((e) => {
            const link = entryLink(e);
            const titled = !!tidy(e.title);
            const yr = e.year === last ? "" : esc(e.year);
            last = e.year;
            const when = e.month ? `<span class="tl-cv-month">${esc(e.month)}</span>` : "";
            return `<li class="tl-cv-row${yr ? " tl-cv-row-year" : ""}"><span class="tl-cv-year">${yr}</span><span class="tl-cv-entry">${titled ? `<span class="tl-cv-title">${titleHtml(e, link)}</span>` : ""}<span class="tl-cv-where">${whereHtml(e, link, titled)}${when}</span></span></li>`;
        }).join("\n");
    };
    const exHtml = split("exhibitions", {
        title: "Exhibitions", eyebrow: "Beyond the Studio",
        lede: `Solo and group exhibitions, ${e0}&thinsp;&ndash;&thinsp;${e1}.`,
    }) + `\n<section class="tl-cv-body tl-cv-lists">
<nav class="tl-cv-jump"><a href="#solo">Solo Exhibitions</a><a href="#group">Group Exhibitions</a></nav>
<h2 class="tl-cv-head" id="solo">Solo Exhibitions</h2>
<ol class="tl-cv-list">\n${list(solo)}\n</ol>
<h2 class="tl-cv-head" id="group">Group Exhibitions</h2>
<ol class="tl-cv-list">\n${list(group)}\n</ol>
</section>`;

    return {
        pages: {
            "curatorial-projects": { title: "Curatorial Projects", html: curHtml },
            "exhibitions": { title: "Exhibitions", html: exHtml },
        },
        summary: {
            curatorial: { n: curatorial.length, from: c0, to: c1, img: UP + "2023/08/CIA_20111119_6006-1024x683.jpg" },
            exhibitions: { solo: solo.length, group: group.length, from: e0, to: e1, img: UP + "2022/06/Suburban-install3-1024x768.jpg" },
        },
    };
}

/* JS the plugin prepends to refresh.js: counts for the Beyond the Studio cards. */
export function summaryJs(summary) {
    return `/* Curatorial Projects + Exhibitions — generated by build-cv.mjs from Fía's CV sheet */\nwindow.TL_CV = ${JSON.stringify(summary)};\n`;
}

/* Headless preview: the live site 404s these paths until the plugin ships,
   so the preview swaps the 404's #primary for exactly what the plugin prints. */
if (process.argv.includes("--preview")) {
    const { pages, summary } = await buildCv();
    const js = summaryJs(summary) + `(function(){var P=${JSON.stringify(pages)};var slug=location.pathname.replace(/^\\/+|\\/+$/g,'');var p=P[slug];if(!p||!document.body.classList.contains('error404'))return;var b=document.body.classList;b.remove('error404','ast-separate-container','ast-two-container');b.add('page','ast-page-builder-template','tl-cv-page','tl-cv-'+slug);document.title=p.title+' – Thomas Lawson';var old=document.getElementById('primary');var d=document.createElement('div');d.innerHTML='<div id="primary" class="content-area primary"><main id="main" class="site-main"><article class="page type-page tl-cv-article"><div class="entry-content clear">'+p.html+'</div></article></main></div>';old.replaceWith(d.firstChild);})();\n`;
    await writeFile(resolve(here, "preview-cv.js"), js);
    console.log(resolve(here, "preview-cv.js"), Object.keys(pages).map((k) => `${k}: ${pages[k].html.length}b`).join(", "));
}
