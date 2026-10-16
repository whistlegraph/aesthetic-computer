const table = document.getElementById("pubs");
const dialog = document.createElement("dialog");
dialog.className = "paper-preview";
dialog.setAttribute("aria-description", "Scroll through the paper. Use the zoom controls to enlarge pages. Press Escape or tap the dark background to close.");
dialog.innerHTML = `
  <nav class="preview-formats" aria-label="Paper format" hidden>
    <button type="button" data-format="paper" aria-pressed="true">Paper</button>
    <button type="button" data-format="cards" aria-pressed="false">Cards</button>
  </nav>
  <div class="preview-feed" tabindex="0" aria-label="Paper pages" autofocus>
    <p class="preview-status" role="status">Loading paper…</p>
    <div class="preview-pages"></div>
  </div>
  <nav class="preview-zoom" aria-label="Paper zoom">
    <button type="button" data-zoom="out" aria-label="Zoom out">−</button>
    <button type="button" data-zoom="fit" aria-label="Fit page to screen">Fit</button>
    <button type="button" data-zoom="in" aria-label="Zoom in">+</button>
  </nav>
  <nav class="preview-actions" aria-label="Paper options">
    <a class="preview-open" target="_blank" rel="noopener">Open in tab ↗</a>
    <a class="preview-download" download>Download PDF</a>
  </nav>`;
document.body.append(dialog);
const feed = dialog.querySelector(".preview-feed");
const pages = dialog.querySelector(".preview-pages");
const status = dialog.querySelector(".preview-status");
const zoomOut = dialog.querySelector('[data-zoom="out"]');
const zoomIn = dialog.querySelector('[data-zoom="in"]');
const formats = dialog.querySelector('.preview-formats');
let opener, current, pdfLibrary;
let selection;

formats.addEventListener('click', event => {
  const format = event.target.closest('[data-format]')?.dataset.format;
  if (!selection || !selection[format] || selection.active === format) return;
  selection.active = format;
  openPaper(new URL(selection[format]), selection.title + (format === 'cards' ? ' · Cards' : ''), opener);
});

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
  state.cards.forEach(card => { card.element.style.width = pageWidth(state, card.aspect) + 'px'; });
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
  if (!event.target.closest(".preview-card") || !current) return;
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
        card.element.style.width = pageWidth(state, card.aspect) + 'px';
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
        if (!card.text.textContent) {
          const content = await page.getTextContent();
          if (current === state) card.text.textContent = content.items.map(item => item.str || "").join(" ");
        }
      } catch (error) {
        if (current !== state || error.name === "RenderingCancelledException") continue;
        card.element.dataset.error = "true";
        card.text.textContent = `Page ${card.number} could not load. Open the PDF to read it.`;
      } finally { card.task = null; }
    }
  } finally { state.rendering = false; }
}

async function openPaper(url, title, link) {
  dispose();
  opener = link;
  const state = current = { cards: [], queue: new Set(), zoom: 1, rendering: false, aspect: 8.5 / 11 };
  formats.hidden = !selection?.paper || !selection?.cards;
  for (const button of formats.querySelectorAll('button')) button.setAttribute('aria-pressed', String(button.dataset.format === selection?.active));
  dialog.setAttribute("aria-label", title);
  dialog.querySelector(".preview-open").href = url.href;
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
  const row = link.closest("tr");
  const title = (row?.querySelector(".paper-title") || row?.querySelector(".ti > a"))?.textContent.trim() || link.textContent.trim();
  const paper = row?.querySelector('.paper-title a[href$=".pdf"], .ti > a[href$=".pdf"]');
  const cards = row?.querySelector('.fm a[href$="-cards.pdf"]');
  selection = { title, paper: paper?.href, cards: cards?.href,
    active: url.href === cards?.href ? 'cards' : url.href === paper?.href ? 'paper' : null };
  const format = link.closest(".fm") ? ` · ${link.title || link.textContent.trim()}` : "";
  openPaper(url, title + format, link);
});
