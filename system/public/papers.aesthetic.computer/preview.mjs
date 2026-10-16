const table = document.getElementById("pubs");
const dialog = document.createElement("dialog");
dialog.className = "paper-preview";
dialog.setAttribute("aria-labelledby", "preview-title");
dialog.innerHTML = `
  <header class="preview-bar">
    <h2 id="preview-title"></h2>
    <button type="button" class="preview-close" aria-label="Close preview" autofocus>×</button>
  </header>
  <div class="preview-page">
    <img alt="">
    <p class="preview-status" role="status">Loading cover…</p>
  </div>
  <nav class="preview-actions" aria-label="Paper options">
    <a class="preview-open" target="_blank" rel="noopener">Open in tab ↗</a>
    <a class="preview-download" download>Download PDF</a>
  </nav>`;
document.body.append(dialog);
const image = dialog.querySelector("img");
const status = dialog.querySelector(".preview-status");
let opener;

image.addEventListener("load", () => { status.hidden = true; image.hidden = false; });
image.addEventListener("error", () => {
  image.hidden = true;
  status.hidden = false;
  status.textContent = "Cover unavailable. You can still open the PDF.";
});
dialog.querySelector(".preview-close").addEventListener("click", () => dialog.close());
dialog.addEventListener("click", event => {
  if (event.target !== dialog) return;
  const rect = dialog.getBoundingClientRect();
  if (event.clientX < rect.left || event.clientX > rect.right ||
      event.clientY < rect.top || event.clientY > rect.bottom) dialog.close();
});
dialog.addEventListener("close", () => {
  document.documentElement.classList.remove("preview-open");
  opener?.focus({ preventScroll: true });
});

table?.addEventListener("click", event => {
  const link = event.target.closest("a[href]");
  // Keep command/control-click, middle click, and the link's native context menu.
  if (!link || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  const url = new URL(link.href);
  if (url.origin !== location.origin || !url.pathname.endsWith(".pdf")) return;
  event.preventDefault();
  opener = link;
  const row = link.closest("tr");
  const title = (row?.querySelector(".paper-title") || row?.querySelector(".ti > a"))?.textContent.trim() || link.textContent.trim();
  const format = link.closest(".fm") ? ` · ${link.title || link.textContent.trim()}` : "";
  dialog.querySelector("h2").textContent = title + format;
  dialog.querySelector(".preview-open").href = url.href;
  dialog.querySelector(".preview-download").href = url.href;
  image.hidden = true;
  image.alt = `First page of ${title}${format}`;
  status.textContent = "Loading cover…";
  status.hidden = false;
  image.src = "/previews/" + url.pathname.split("/").at(-1).replace(/\.pdf$/, ".jpg");
  document.documentElement.classList.add("preview-open");
  dialog.showModal();
});
