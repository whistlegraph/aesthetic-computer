// Pure layout and hit-testing for Menu Fighter's launcher/popover.

const inside = (box, x, y) =>
  !!box && x >= box.x && x < box.x + box.w && y >= box.y && y < box.y + box.h;

export function menuLayout(width, height) {
  const launcherSize = Math.max(18, Math.min(24, (Math.min(width, height) / 7) | 0));
  const launcher = {
    x: ((width - launcherSize) / 2) | 0,
    y: Math.max(3, height - launcherSize - 4),
    w: launcherSize,
    h: launcherSize,
  };

  const panelW = Math.min(232, Math.max(142, width - 16));
  const panelH = Math.min(126, Math.max(104, height - 16));
  const panel = {
    x: ((width - panelW) / 2) | 0,
    y: ((height - panelH) / 2) | 0,
    w: panelW,
    h: panelH,
  };
  const buttonW = panelW - 24;
  const buttonH = 18;
  const train = { x: panel.x + 12, y: panel.y + panelH - 48, w: buttonW, h: buttonH };
  const find = { x: panel.x + 12, y: panel.y + panelH - 26, w: buttonW, h: buttonH };

  return { launcher, panel, train, find };
}

export function menuHit(layout, x, y, open = false) {
  if (!layout) return null;
  if (!Number.isFinite(x) || !Number.isFinite(y)) return null;
  if (!open) return inside(layout.launcher, x, y) ? "launcher" : null;
  if (inside(layout.train, x, y)) return "train";
  if (inside(layout.find, x, y)) return "find";
  if (!inside(layout.panel, x, y)) return "outside";
  return null;
}
