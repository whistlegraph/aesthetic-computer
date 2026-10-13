import { qrcode, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";

let active;
export function openShare(href, { opener = document.activeElement } = {}) {
  const url = new URL(href);
  if (url.origin !== "https://mime.ac" || !/^\/(wg[A-Za-z0-9]+|p\/[^/]+)$/.test(url.pathname)) return;
  url.search = url.hash = "";
  if (active?.open) { active.focus(); return active; }
  const dialog = document.createElement("dialog");
  dialog.className = "mime-share";
  dialog.setAttribute("aria-label", "Share this post");
  const style = document.createElement("style");
  style.textContent = `dialog.mime-share{box-sizing:border-box;width:min(330px,calc(100% - 32px));max-height:calc(100dvh - 24px);padding:18px;margin:auto;border:1px solid #e3d8df;border-radius:18px;background:#fffdf7;color:#24202c;box-shadow:inset 0 0 0 3px #f1e7ed,0 4px 0 #b5a0b1,0 18px 48px #0009;overflow:auto;text-align:center;font:14px/1.4 system-ui,sans-serif;overscroll-behavior:contain;animation:mime-share-rise 180ms ease-out}dialog.mime-share::backdrop{background:#080d14bb;backdrop-filter:blur(3px)}.mime-share svg{display:block;width:min(240px,100%);height:auto;max-height:calc(100dvh - 180px);margin:0 auto 10px;background:white;shape-rendering:crispEdges}.mime-share .share-url{display:block;color:#43369a;font-weight:700;overflow-wrap:anywhere;text-decoration:none;user-select:all}.mime-share .share-copy{font:inherit;margin:12px 0 0;padding:7px 18px;min-height:36px;border:1px solid #c5b4c4;border-radius:8px;background:#f2e9f1;color:#342938;cursor:pointer}.mime-share .share-status{font-size:12px;margin:8px 0 0}.mime-share .share-status:empty{display:none}.mime-share .share-dismiss{position:absolute;clip-path:inset(50%);width:1px;height:1px;overflow:hidden}.mime-share .share-dismiss:focus-visible{clip-path:none;width:auto;height:auto;bottom:8px;right:8px}.mime-share :focus-visible{outline:2px solid #953884;outline-offset:3px}@keyframes mime-share-rise{from{opacity:0;transform:translateY(16px)}to{opacity:1;transform:none}}@media(prefers-reduced-motion:reduce){dialog.mime-share{animation:none}}`;
  const close = document.createElement("button");
  close.className = "share-dismiss";
  close.textContent = "Put card away";
  close.onclick = () => dialog.close();

  const modules = qrcode(url.href, { errorCorrectLevel: ErrorCorrectLevel.M }).modules;
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", `0 0 ${modules.length + 8} ${modules.length + 8}`);
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", `QR code for ${url.href}`);
  const path = document.createElementNS(svg.namespaceURI, "path");
  const squares = [];
  modules.forEach((row, y) => row.forEach((dark, x) => { if (dark) squares.push(`M${x + 4} ${y + 4}h1v1h-1z`); }));
  path.setAttribute("d", squares.join(""));
  path.setAttribute("fill", "#000");
  svg.append(path);
  const link = document.createElement("a");
  link.className = "share-url";
  link.href = url.href;
  link.textContent = url.host + url.pathname;
  const copy = document.createElement("button");
  copy.className = "share-copy";
  copy.textContent = "Copy link";
  const status = document.createElement("p");
  status.className = "share-status";
  status.setAttribute("role", "status");
  copy.onclick = async () => {
    try {
      await navigator.clipboard.writeText(url.href);
      status.textContent = "Copied";
    } catch {
      const range = document.createRange();
      range.selectNodeContents(link);
      getSelection().removeAllRanges(); getSelection().addRange(range);
      status.textContent = "Select and copy the link.";
    }
  };
  dialog.append(style, close, svg, link, copy, status);
  const overflow = document.documentElement.style.overflow;
  const keys = event => {
    if (!dialog.open) return;
    if (event.key === "Escape") { event.preventDefault(); dialog.close(); }
    if (["ArrowDown", "ArrowUp", "PageDown", "PageUp", "Home", "End", " "].includes(event.key) && event.target !== copy && event.target !== close) event.preventDefault();
    if (event.type === "keydown" && event.key === "Tab") {
      if (event.shiftKey && document.activeElement === close) { event.preventDefault(); copy.focus(); }
      else if (!event.shiftKey && document.activeElement === copy) { event.preventDefault(); close.focus(); }
    }
    event.stopImmediatePropagation();
  };
  window.addEventListener("keydown", keys, true);
  window.addEventListener("keyup", keys, true);
  let backdropDown;
  dialog.addEventListener("pointerdown", event => { backdropDown = event.target === dialog; });
  dialog.addEventListener("click", event => {
    const rect = dialog.getBoundingClientRect();
    if (backdropDown && event.target === dialog && (event.clientX < rect.left || event.clientX > rect.right || event.clientY < rect.top || event.clientY > rect.bottom)) dialog.close();
  });
  dialog.addEventListener("close", () => {
    window.removeEventListener("keydown", keys, true);
    window.removeEventListener("keyup", keys, true);
    document.documentElement.style.overflow = overflow;
    dialog.remove(); active = null;
    if (opener?.isConnected) opener.focus({ preventScroll: true });
    window.dispatchEvent(new Event("ac-share-close"));
  }, { once: true });
  document.body.append(dialog);
  active = dialog;
  dialog.showModal();
  copy.focus({ preventScroll: true });
  document.documentElement.style.overflow = "hidden";
  window.dispatchEvent(new Event("ac-share-open"));
  return dialog;
}
