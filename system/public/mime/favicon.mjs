// Swap cached PNG frames, retaining a normal PNG icon for bookmarks
// and clients that do not display favicon updates.
const tabMotion = matchMedia("(prefers-reduced-motion: reduce)");
const favicon = document.getElementById("mime-favicon");
const staticFavicon = favicon.href;
let faviconFrames = [], faviconTimer;
function syncFavicon() {
  clearInterval(faviconTimer);
  document.documentElement.toggleAttribute("data-page-hidden", document.hidden);
  favicon.href = staticFavicon;
  if (document.hidden || tabMotion.matches || !faviconFrames.length) return;
  let frame = 0;
  faviconTimer = setInterval(() => { favicon.href = faviconFrames[frame++ % faviconFrames.length]; }, 160);
}
async function prepareFavicon() {
  try {
    const response = await fetch(new URL("./favicon.svg?v=4", import.meta.url));
    if (!response.ok) return;
    const svg = new DOMParser().parseFromString(await response.text(), "image/svg+xml").documentElement;
    svg.setAttribute("width", "64"); svg.setAttribute("height", "64");
    const letters = [...svg.querySelectorAll(".favicon-letter")];
    if (letters.length !== 4) return;
    const canvas = document.createElement("canvas"); canvas.width = canvas.height = 32;
    const context = canvas.getContext("2d"), frames = [];
    for (let frame = 0; frame < 12; frame++) {
      letters.forEach((letter, i) => {
        const phase = frame / 12 * Math.PI * 2 + i * 1.7;
        letter.setAttribute("transform", `translate(${letter.dataset.x} ${Number(letter.dataset.y) + Math.sin(phase) * 3}) rotate(${Math.cos(phase) * 8})`);
      });
      const picture = new Image();
      picture.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(svg));
      await picture.decode();
      context.clearRect(0, 0, 32, 32); context.drawImage(picture, 0, 0, 32, 32);
      frames.push(canvas.toDataURL("image/png"));
    }
    faviconFrames = frames; syncFavicon();
  } catch { /* The static comic icon is already in the document. */ }
}
tabMotion.addEventListener("change", syncFavicon);
document.addEventListener("visibilitychange", syncFavicon);
window.addEventListener("pagehide", () => { clearInterval(faviconTimer); favicon.href = staticFavicon; });
window.addEventListener("pageshow", syncFavicon);
prepareFavicon();
