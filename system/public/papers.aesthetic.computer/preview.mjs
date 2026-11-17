const table = document.getElementById("pubs");
const dialog = document.createElement("dialog");
dialog.className = "paper-preview";
dialog.setAttribute("aria-description", "Scroll through the paper. Text can be selected and links followed. Use the zoom controls to enlarge pages. Press Escape or tap the dark background to close.");
dialog.innerHTML = `
  <nav class="preview-formats" aria-label="Paper format and language" hidden></nav>
  <div class="preview-feed" tabindex="0" aria-label="Paper pages" autofocus>
    <p class="preview-status" role="status">Loading paper…</p>
    <div class="preview-pages"></div>
  </div>
  <nav class="preview-zoom" aria-label="Paper zoom">
    <button type="button" data-zoom="out" aria-label="Zoom out">−</button>
    <button type="button" data-zoom="fit" aria-label="Fit page to screen">Fit</button>
    <button type="button" data-zoom="in" aria-label="Zoom in">+</button>
    <span class="preview-count" aria-hidden="true"></span>
  </nav>
  <nav class="preview-actions" aria-label="Paper options">
    <a class="preview-download" target="_blank" rel="noopener">Download ↗</a>
  </nav>`;
document.body.append(dialog);
const feed = dialog.querySelector(".preview-feed");
const pages = dialog.querySelector(".preview-pages");
const status = dialog.querySelector(".preview-status");
const zoomOut = dialog.querySelector('[data-zoom="out"]');
const zoomIn = dialog.querySelector('[data-zoom="in"]');
const formats = dialog.querySelector('.preview-formats');
const count = dialog.querySelector('.preview-count');
let opener, current, pdfLibrary;
let selection;

// Every version of one paper (the paper, its card, its translations) sits in
// the top switcher. Changing version keeps your place: selected text is looked
// up in the new version, otherwise the same page comes back.
formats.addEventListener('click', event => {
  const href = event.target.closest('[data-href]')?.dataset.href;
  const variant = selection?.variants.find(item => item.href === href);
  if (!variant || selection.active === href) return;
  const carry = position();
  selection.active = href;
  openPaper(new URL(href), selection.title + (variant.key === 'paper' ? '' : ` · ${variant.name}`), opener, carry);
});

function position() {
  const state = current;
  if (!state?.cards.length) return null;
  const top = feed.getBoundingClientRect().top;
  const card = state.cards.find(item => item.element.getBoundingClientRect().bottom > top + 40) || state.cards[0];
  const rect = card.element.getBoundingClientRect();
  return { text: String(getSelection()), page: card.number, pages: state.cards.length,
    within: Math.max(0, (top - rect.top) / rect.height) };
}

function scrollToCard(card, within = 0, behavior = "auto") {
  const rect = card.element.getBoundingClientRect();
  feed.scrollBy({ top: rect.top - feed.getBoundingClientRect().top + within * rect.height - 24, behavior });
}

// Letters and digits only, so line breaks and hyphenation never block a match.
const fold = text => text.toLowerCase().replace(/[^\p{L}\p{N}]+/gu, "");

async function restore(state, carry) {
  if (!carry) return;
  // Land in the same place by proportion at once, since a translation or a
  // card deck paginates differently, then refine to the selected passage.
  const place = (carry.page - 1 + Math.min(1, carry.within)) / carry.pages * state.cards.length;
  const near = Math.min(state.cards.length - 1, Math.floor(place));
  if (place > 0) scrollToCard(state.cards[near], place - near);
  const selected = fold(carry.text);
  if (selected.length < 12) return;
  // A card can break a passage in two, so shorter pieces count as well.
  const middle = Math.max(0, Math.floor(selected.length / 2) - 15);
  const needles = [selected.slice(0, 60), selected.slice(0, 30), selected.slice(middle, middle + 30)].filter(needle => needle.length > 11);
  // The passage is usually close to the proportional guess, so look outward from it.
  const outward = [...state.cards].sort((a, b) => Math.abs(a.number - 1 - near) - Math.abs(b.number - 1 - near));
  for (const card of outward) {
    const content = await (await state.pdf.getPage(card.number)).getTextContent();
    if (current !== state) return;
    const text = fold(content.items.map(item => item.str || "").join(" "));
    if (!needles.some(needle => text.includes(needle))) continue;
    scrollToCard(card);
    card.element.dataset.found = "true";
    setTimeout(() => { delete card.element.dataset.found; }, 1800);
    return;
  }
}

function updateCount() {
  const state = current;
  if (!state?.cards.length) { count.textContent = ""; return; }
  const middle = feed.getBoundingClientRect().top + feed.clientHeight / 2;
  const card = state.cards.find(item => item.element.getBoundingClientRect().bottom > middle) || state.cards.at(-1);
  count.textContent = `${card.number} / ${state.cards.length}`;
}
let counting = false;
feed.addEventListener("scroll", () => {
  if (counting) return;
  counting = true;
  requestAnimationFrame(() => { counting = false; updateCount(); });
}, { passive: true });

function library() {
  return pdfLibrary ||= import("./vendor/pdfjs/legacy/build/pdf.min.mjs").then(pdfjs => {
    pdfjs.GlobalWorkerOptions.workerSrc = new URL("./vendor/pdfjs/legacy/build/pdf.worker.min.mjs", import.meta.url).href;
    return pdfjs;
  }).catch(error => { pdfLibrary = null; throw error; });
}

function dispose() {
  const state = current;
  current = null;
  if (!state) return;
  clearTimeout(state.resizeTimer);
  state.observer?.disconnect();
  state.cards.forEach(card => { card.task?.cancel(); card.canvas.width = card.canvas.height = 0; });
  state.loading?.destroy().catch(() => {});
  pages.replaceChildren();
}

dialog.addEventListener("close", () => {
  dispose();
  document.documentElement.classList.remove("preview-open");
  opener?.focus({ preventScroll: true });
});
// Only a click that starts and ends on the dark surround dismisses the paper.
const isSurround = target => target === dialog || target === feed || target === pages;
let backdropDown = false;
dialog.addEventListener("pointerdown", event => { backdropDown = isSurround(event.target); });
dialog.addEventListener("click", event => {
  if (backdropDown && isSurround(event.target)) dialog.close();
  backdropDown = false;
});

function pageWidth(state, aspect = state.aspect) {
  return Math.min(780, Math.max(160, feed.clientWidth - 40), Math.max(160, feed.clientHeight - 150) * aspect) * state.zoom;
}

function layout(state) {
  pages.style.width = pageWidth(state) + "px";
  state.cards.forEach(card => {
    const width = pageWidth(state, card.aspect);
    card.element.style.width = width + 'px';
    // The text overlay sizes its glyphs from this, so it tracks every zoom.
    if (card.base) card.element.style.setProperty("--total-scale-factor", width / card.base.width);
  });
  updateCount();
  zoomOut.disabled = state.zoom <= 1;
  zoomIn.disabled = state.zoom >= 3;
  dialog.querySelector('[data-zoom="fit"]').textContent = state.zoom === 1 ? "Fit" : Math.round(state.zoom * 100) + "%";
}

function zoomTo(value, point) {
  const state = current;
  if (!state) return;
  const next = Math.max(1, Math.min(3, value));
  if (next === state.zoom) return;
  const rect = pages.getBoundingClientRect();
  const x = point?.x ?? feed.clientWidth / 2;
  const y = point?.y ?? feed.clientHeight / 2;
  const relX = (x - rect.left) / rect.width;
  const relY = (y - rect.top) / rect.height;
  state.zoom = next;
  layout(state);
  const after = pages.getBoundingClientRect();
  feed.scrollLeft += after.left + relX * after.width - x;
  feed.scrollTop += after.top + relY * after.height - y;
  scheduleRender(state);
}

dialog.querySelector(".preview-zoom").addEventListener("click", event => {
  const action = event.target.closest("[data-zoom]")?.dataset.zoom;
  if (!action || !current) return;
  zoomTo(action === "fit" ? 1 : current.zoom + (action === "in" ? .5 : -.5));
});
feed.addEventListener("dblclick", event => {
  // A double click on words selects the word; on the page margin it zooms.
  if (!event.target.closest(".preview-card") || event.target.closest(".textLayer span, .preview-link") || !current) return;
  zoomTo(current.zoom === 1 ? 2 : 1, { x: event.clientX, y: event.clientY });
});
// Trackpad pinch and touch pinch scale the paper, leaving the controls in place.
feed.addEventListener("wheel", event => {
  if (!event.ctrlKey || !current) return;
  event.preventDefault();
  zoomTo(current.zoom * Math.exp(-event.deltaY / 300), { x: event.clientX, y: event.clientY });
}, { passive: false });
let pinch;
const touchDistance = touches => Math.hypot(touches[0].clientX - touches[1].clientX, touches[0].clientY - touches[1].clientY);
feed.addEventListener("touchstart", event => {
  if (event.touches.length === 2 && current) pinch = { distance: touchDistance(event.touches), zoom: current.zoom };
}, { passive: true });
feed.addEventListener("touchmove", event => {
  if (!pinch || event.touches.length !== 2) return;
  event.preventDefault();
  zoomTo(pinch.zoom * touchDistance(event.touches) / pinch.distance, {
    x: (event.touches[0].clientX + event.touches[1].clientX) / 2,
    y: (event.touches[0].clientY + event.touches[1].clientY) / 2,
  });
}, { passive: false });
feed.addEventListener("touchend", () => { pinch = null; });
feed.addEventListener("touchcancel", () => { pinch = null; });

function scheduleRender(state) {
  clearTimeout(state.resizeTimer);
  state.resizeTimer = setTimeout(() => {
    if (current !== state) return;
    state.cards.forEach(card => {
      card.task?.cancel();
      card.width = 0;
      if (card.near) state.queue.add(card);
    });
    renderNext(state);
  }, 120);
}
new ResizeObserver(() => {
  if (!current) return;
  layout(current);
  scheduleRender(current);
}).observe(feed);

async function renderNext(state) {
  if (state.rendering || current !== state) return;
  state.rendering = true;
  try {
    while (state.queue.size && current === state) {
      const card = state.queue.values().next().value;
      state.queue.delete(card);
      if (!card.near) continue;
      const width = pageWidth(state, card.aspect);
      if (card.width === width) continue;
      try {
        const page = await state.pdf.getPage(card.number);
        if (current !== state || !card.near) continue;
        const base = page.getViewport({ scale: 1 });
        card.aspect = base.width / base.height;
        card.base = base;
        card.element.style.width = pageWidth(state, card.aspect) + 'px';
        card.element.style.setProperty("--total-scale-factor", pageWidth(state, card.aspect) / base.width);
        card.element.style.aspectRatio = `${base.width} / ${base.height}`;
        const viewport = page.getViewport({ scale: pageWidth(state, card.aspect) / base.width });
        const pixelRatio = Math.min(devicePixelRatio || 1, 2, 2400 / viewport.width);
        const canvas = card.canvas;
        canvas.width = Math.ceil(viewport.width * pixelRatio);
        canvas.height = Math.ceil(viewport.height * pixelRatio);
        card.task = page.render({ canvasContext: canvas.getContext("2d"), viewport,
          transform: [pixelRatio, 0, 0, pixelRatio, 0, 0] });
        await card.task.promise;
        if (current !== state) continue;
        card.width = width;
        card.element.dataset.loaded = "true";
        if (!card.overlaid) {
          card.overlaid = true;
          overlay(state, card, page, base).catch(() => { card.overlaid = false; });
        }
      } catch (error) {
        if (current !== state || error.name === "RenderingCancelledException") continue;
        card.element.dataset.error = "true";
        card.text.textContent = `Page ${card.number} could not load. Open the PDF to read it.`;
      } finally { card.task = null; }
    }
  } finally { state.rendering = false; }
}

// The page is a picture; this lays real DOM over it: the page's words as
// selectable text, and every PDF link as an anchor in the same spot.
async function overlay(state, card, page, base) {
  const pdfjs = await library();
  const content = await page.getTextContent();
  const notes = await page.getAnnotations();
  if (current !== state) return;
  const words = document.createElement("div");
  words.className = "textLayer";
  card.element.append(words);
  await new pdfjs.TextLayer({ textContentSource: content, container: words, viewport: base }).render();
  const links = document.createElement("div");
  links.className = "preview-links";
  for (const note of notes) {
    if (note.subtype !== "Link" || !(note.url || note.dest)) continue;
    const [x1, y1] = base.convertToViewportPoint(note.rect[0], note.rect[1]);
    const [x2, y2] = base.convertToViewportPoint(note.rect[2], note.rect[3]);
    const anchor = document.createElement("a");
    anchor.className = "preview-link";
    anchor.style.left = Math.min(x1, x2) / base.width * 100 + "%";
    anchor.style.top = Math.min(y1, y2) / base.height * 100 + "%";
    anchor.style.width = Math.abs(x2 - x1) / base.width * 100 + "%";
    anchor.style.height = Math.abs(y2 - y1) / base.height * 100 + "%";
    if (note.url) {
      anchor.href = note.url;
      anchor.target = "_blank";
      anchor.rel = "noopener";
      anchor.title = note.url;
    } else {
      anchor.href = "#";
      anchor.dataset.dest = JSON.stringify(note.dest);
      anchor.setAttribute("aria-label", "Jump within this paper");
    }
    links.append(anchor);
  }
  card.element.append(links);
}

// A link inside the paper to a footnote, figure, section, or reference.
async function jump(state, dest) {
  try {
    if (typeof dest === "string") dest = await state.pdf.getDestination(dest);
    if (!Array.isArray(dest) || current !== state) return;
    const index = Number.isInteger(dest[0]) ? dest[0] : await state.pdf.getPageIndex(dest[0]);
    const card = state.cards[index];
    if (!card || current !== state) return;
    const base = (await state.pdf.getPage(index + 1)).getViewport({ scale: 1 });
    const kind = dest[1]?.name;
    const y = kind === "XYZ" ? dest[3] : kind === "FitH" || kind === "FitBH" ? dest[2] : null;
    const within = y == null ? 0 : Math.max(0, base.convertToViewportPoint(0, y)[1] / base.height);
    if (current === state) scrollToCard(card, within, "smooth");
  } catch {}
}

pages.addEventListener("click", event => {
  const anchor = event.target.closest("a.preview-link");
  if (!anchor || !current) return;
  if (anchor.dataset.dest) {
    event.preventDefault();
    jump(current, JSON.parse(anchor.dataset.dest));
    return;
  }
  // Another paper on this site opens here; anything else opens a new tab.
  if (event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  const url = new URL(anchor.href);
  if (!/(^|\.)papers\.aesthetic\.computer$/.test(url.hostname) && url.origin !== location.origin) return;
  if (!url.pathname.endsWith(".pdf")) return;
  event.preventDefault();
  const local = new URL(url.pathname, location.origin);
  const row = [...(table?.querySelectorAll('a[href$=".pdf"]') || [])].find(item => new URL(item.href).pathname === local.pathname);
  if (row) openFromLink(row, local);
  else { selection = null; openPaper(local, decodeURIComponent(local.pathname.slice(1)), opener); }
});

async function openPaper(url, title, link, carry) {
  dispose();
  opener = link;
  const state = current = { cards: [], queue: new Set(), zoom: 1, rendering: false, aspect: 8.5 / 11 };
  const variants = selection?.variants || [];
  formats.hidden = variants.length < 2;
  formats.replaceChildren(...variants.map(variant => {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.href = variant.href;
    button.textContent = variant.label;
    button.title = variant.name;
    button.setAttribute("aria-pressed", String(variant.href === selection.active));
    return button;
  }));
  formats.querySelector('[aria-pressed="true"]')?.scrollIntoView({ block: "nearest", inline: "nearest" });
  count.textContent = "";
  dialog.setAttribute("aria-label", title);
  dialog.querySelector(".preview-download").href = url.href;
  status.textContent = "Loading paper…";
  status.hidden = false;
  document.documentElement.classList.add("preview-open");
  if (!dialog.open) dialog.showModal();
  feed.scrollTo(0, 0);
  layout(state);
  try {
    const pdfjs = await library();
    if (current !== state) return;
    const assets = new URL("./vendor/pdfjs/", import.meta.url).href;
    state.loading = pdfjs.getDocument({ url: url.href, isEvalSupported: false,
      cMapUrl: assets + "cmaps/", cMapPacked: true,
      standardFontDataUrl: assets + "standard_fonts/", wasmUrl: assets + "wasm/" });
    state.pdf = await state.loading.promise;
    if (current !== state) return;
    const first = await state.pdf.getPage(1);
    if (current !== state) return;
    const size = first.getViewport({ scale: 1 });
    state.aspect = size.width / size.height;
    const fragment = document.createDocumentFragment();
    for (let number = 1; number <= state.pdf.numPages; number++) {
      const element = document.createElement("article");
      element.className = "preview-card";
      element.setAttribute("aria-label", `Page ${number} of ${state.pdf.numPages}`);
      element.style.aspectRatio = `${size.width} / ${size.height}`;
      const canvas = document.createElement("canvas");
      canvas.setAttribute("aria-hidden", "true");
      const text = document.createElement("p");
      text.className = "preview-page-text";
      element.append(canvas, text);
      const card = { element, canvas, text, number, width: 0, aspect: state.aspect };
      state.cards.push(card);
      fragment.append(element);
    }
    pages.append(fragment);
    layout(state);
    status.hidden = true;
    state.observer = new IntersectionObserver(entries => {
      for (const entry of entries) {
        const card = state.cards.find(card => card.element === entry.target);
        card.near = entry.isIntersecting;
        if (card.near) state.queue.add(card);
        else {
          card.task?.cancel();
          card.canvas.width = card.canvas.height = 0;
          card.width = 0;
          delete card.element.dataset.loaded;
        }
      }
      renderNext(state);
    }, { root: feed, rootMargin: "900px 0px" });
    state.cards.forEach(card => state.observer.observe(card.element));
    updateCount();
    await restore(state, carry);
  } catch (error) {
    if (current !== state) return;
    status.textContent = "Preview unavailable. You can still open or download the PDF.";
    status.hidden = false;
  }
}

table?.addEventListener("click", event => {
  const link = event.target.closest("a[href]");
  // Preserve modified clicks, middle click, and the native link context menu.
  if (!link || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  const url = new URL(link.href);
  if (url.origin !== location.origin || !url.pathname.endsWith(".pdf")) return;
  event.preventDefault();
  openFromLink(link, url);
});

function openFromLink(link, url) {
  const row = link.closest("tr");
  const title = (row?.querySelector(".paper-title") || row?.querySelector(".ti > a"))?.textContent.trim() || link.textContent.trim();
  const paper = row?.querySelector('.paper-title a[href$=".pdf"], .ti > a[href$=".pdf"]');
  const variants = paper ? [{ key: "paper", label: "Paper", name: "Paper", href: paper.href }] : [];
  for (const other of row?.querySelectorAll('.fm a[href$=".pdf"]') || []) {
    const cards = other.href.endsWith("-cards.pdf");
    const text = other.textContent.trim();
    variants.push({ key: cards ? "cards" : text, href: other.href,
      label: cards ? "Cards" : text.length <= 3 ? text.toUpperCase() : text,
      name: cards ? "Cards" : other.title || text });
  }
  selection = { title, variants, active: url.href };
  const variant = variants.find(item => item.href === url.href);
  openPaper(url, title + (variant && variant.key !== "paper" ? ` · ${variant.name}` : ""), link);
}
