// Reserve only the part of the scrolling page covered by the fixed preview.
// A shaped float lets ordinary prose use the full page again below it.
(() => {
  const view = document.getElementById("conversation"),
    content = document.getElementById("notebook-content");
  const preview = document.getElementById("artifact-shell"),
    title = document.getElementById("qr-label");
  if (!view || !content || !preview) return;
  const exclusion = document.createElement("div");
  exclusion.id = "notebook-preview-space";
  exclusion.setAttribute("aria-hidden", "true");
  content.prepend(exclusion);
  let frame = 0;
  const set = (name, value) => {
    if (exclusion.style.getPropertyValue(name) !== value)
      exclusion.style.setProperty(name, value);
  };
  const layout = () => {
    frame = 0;
    const box = preview.getBoundingClientRect(),
      page = content.getBoundingClientRect();
    const visible =
      !preview.hidden &&
      !view.hidden &&
      !document.body.classList.contains("preview-fullscreen") &&
      box.width > 0 &&
      box.height > 0;
    const gap = 16,
      bottom = box.bottom - page.top + gap;
    const width = Math.min(
      page.width,
      Math.max(0, page.right - box.left + gap),
    );
    const reserve = page.width - width < 220 ? page.width : width;
    exclusion.hidden = !visible || bottom <= 0 || width <= 0;
    if (!exclusion.hidden) {
      set("width", `${Math.ceil(reserve)}px`);
      set("height", `${Math.ceil(bottom)}px`);
      set(
        "shape-outside",
        `inset(${Math.max(0, Math.floor(box.top - page.top - gap))}px 0 0 0)`,
      );
    }
    if (title) {
      const available = visible
        ? Math.max(0, box.left - title.getBoundingClientRect().left - gap)
        : view.clientWidth - 28;
      title.style.maxWidth = `${available}px`;
    }
  };
  window.layoutNotebookPreview = () => {
    if (!frame) frame = requestAnimationFrame(layout);
  };
  const observer = new ResizeObserver(window.layoutNotebookPreview);
  observer.observe(preview);
  observer.observe(view);
  new MutationObserver(window.layoutNotebookPreview).observe(preview, {
    attributes: true,
    attributeFilter: ["hidden", "style", "class"],
  });
  new MutationObserver(window.layoutNotebookPreview).observe(document.body, {
    attributes: true,
    attributeFilter: ["class"],
  });
  view.addEventListener("scroll", window.layoutNotebookPreview, {
    passive: true,
  });
  window.addEventListener("resize", window.layoutNotebookPreview);
  window.layoutNotebookPreview();
})();
