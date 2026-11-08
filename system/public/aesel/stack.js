// Where it sits: wires between the stack map's cards, redrawn whenever the
// layout moves. The cards are ordinary DOM; this only draws the lines and
// their labels into the SVG behind them. Under 860px the zones stack and the
// wires are hidden by CSS, so nothing is drawn.

const map = document.querySelector(".stack-map");
const svg = map?.querySelector(".wires");
const NS = "http://www.w3.org/2000/svg";
const R = 10; // corner radius

// [from, to, kind, label, route]
const WIRES = [
  ["n-aesel", "n-cf", "https", "https", "side"],
  ["n-cf", "n-lith", "https", "", "down"],
  ["n-inf", "n-hosted", "eng", "stream", "side"],
  ["n-aesel", "n-claude", "eng", "straight from aesel: the claude and codex CLIs over stdio · open over https with your key", "over"],
  ["n-aesel", "n-codex", "eng", "", "over"],
  ["n-aesel", "n-open", "eng", "", "over"],
  ["n-work", "n-live", "live", "each save", "side"],
  ["n-live", "n-phone", "live", "the channel", "back"],
  ["n-pub", "n-spaces", "https", "PUT", "outside"],
  ["n-spaces", "n-site", "https", "served", "side"],
  ["n-auth", "n-aesel", "auth", "sign-in", "back"],
  ["n-aesel", "n-work", "disk", "", "down"],
  ["n-work", "n-tools", "disk", "", "down"],
  ["n-tools", "n-made", "disk", "", "down"],
];

const el = (name, attrs) => { const node = document.createElementNS(NS, name); for (const k in attrs) node.setAttribute(k, attrs[k]); return node; };

// An orthogonal path through points with rounded corners.
function rounded(points) {
  let d = `M${points[0][0]},${points[0][1]}`;
  for (let i = 1; i < points.length - 1; i++) {
    const [px, py] = points[i - 1], [x, y] = points[i], [nx, ny] = points[i + 1];
    const r = Math.min(R, Math.hypot(x - px, y - py) / 2, Math.hypot(nx - x, ny - y) / 2);
    const ax = x - Math.sign(x - px) * r, ay = y - Math.sign(y - py) * r;
    const bx = x + Math.sign(nx - x) * r, by = y + Math.sign(ny - y) * r;
    d += ` L${ax},${ay} Q${x},${y} ${bx},${by}`;
  }
  const [lx, ly] = points[points.length - 1];
  return d + ` L${lx},${ly}`;
}

function draw() {
  svg.replaceChildren();
  if (getComputedStyle(svg).display === "none") return;
  const base = map.getBoundingClientRect();
  const box = (id) => { const r = document.getElementById(id).getBoundingClientRect(); return { l: r.left - base.left, r: r.right - base.left, t: r.top - base.top, b: r.bottom - base.top, cx: (r.left + r.right) / 2 - base.left, cy: (r.top + r.bottom) / 2 - base.top }; };
  const eng = map.querySelector(".z-eng").getBoundingClientRect();
  const busX = eng.right - base.left + 16, busY = 14;
  const defs = el("defs", {});
  for (const kind of ["https", "eng", "live", "auth", "disk"]) {
    const m = el("marker", { id: `tip-${kind}`, viewBox: "0 0 10 10", refX: 9, refY: 5, markerWidth: 7, markerHeight: 7, orient: "auto-start-reverse" });
    m.append(el("path", { d: "M0,0 L10,5 L0,10 z", class: `tip w-${kind}`, style: "stroke:none;fill:currentColor" }));
    m.style.color = getComputedStyle(map).getPropertyValue(kind === "https" ? "--ink" : kind === "live" ? "--teal" : kind === "disk" ? "--pink" : "--violet") || "#393047";
    defs.append(m);
  }
  svg.append(defs);
  const tags = [];
  let busLabelled = false;
  for (const [from, to, kind, label, route] of WIRES) {
    const a = box(from), b = box(to);
    let pts, at;
    if (route === "down") pts = [[a.cx, a.b], [a.cx, b.t]];
    else if (route === "side") { const y1 = Math.max(a.t + 14, Math.min(a.b - 14, b.cy)), mid = (a.r + b.l) / 2 + (from === "n-work" ? 14 : 0); pts = [[a.r, y1], [mid, y1], [mid, b.cy], [b.l, b.cy]]; at = [mid, (y1 + b.cy) / 2]; if (Math.abs(y1 - b.cy) < 2) { pts = [[a.r, b.cy], [b.l, b.cy]]; at = [mid, b.cy]; } }
    else if (route === "back") { const gut = (box("n-aesel").r + box("n-lith").l) / 2 + (from === "n-auth" ? -14 : 14), y2 = b.cy + (to === "n-aesel" ? 16 : 0); pts = [[a.l, a.cy], [gut, a.cy], [gut, y2], [b.r, y2]]; at = [gut, y2]; }
    else if (route === "outside") { const x = a.r + 22; pts = [[a.r, a.cy], [x, a.cy], [x, b.cy], [b.r, b.cy]]; at = [x, (a.cy + b.cy) / 2]; }
    else if (route === "over") { pts = [[a.cx, a.t], [a.cx, busY], [busX, busY], [busX, b.cy], [b.r, b.cy]]; if (!busLabelled) { at = [(a.cx + busX) / 2, busY]; busLabelled = true; } }
    const marker = kind === "disk" ? { "marker-start": `url(#tip-${kind})`, "marker-end": `url(#tip-${kind})` } : { "marker-end": `url(#tip-${kind})` };
    svg.append(el("path", { d: rounded(pts), class: `w-${kind}`, ...marker }));
    if (label && at) tags.push([label, at]);
  }
  for (const [label, [x, y]] of tags) {
    const g = el("g", { class: "tag" });
    const text = el("text", { x, y: y + 4, "text-anchor": "middle" });
    text.textContent = label;
    g.append(text);
    svg.append(g);
    const bb = text.getBBox();
    g.insertBefore(el("rect", { x: bb.x - 5, y: bb.y - 2, width: bb.width + 10, height: bb.height + 4, rx: 6 }), text);
  }
}

if (map && svg) {
  new ResizeObserver(() => requestAnimationFrame(draw)).observe(map);
  document.fonts?.ready.then(draw);
  draw();
}
