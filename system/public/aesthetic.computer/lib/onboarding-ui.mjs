// Shared pixel layout. Coordinates are AC pixels, not CSS pixels.
export const palette = { ground: [25, 19, 37], panel: [47, 35, 62], text: [255, 241, 224], pink: [255, 113, 191], mint: [135, 233, 191], muted: [193, 173, 207] };
export const fonts = { heading: 'unifont', body: 'font_1' };
export const lineHeight = font => font === fonts.heading ? 18 : 13;
export const characterWidth = font => font === fonts.heading ? 8 : 6;
export function wrap(text, width, font = fonts.body) {
  const n = Math.max(1, Math.floor(width / characterWidth(font))), lines = [];
  for (const paragraph of String(text || '').split('\n')) {
    let line = '';
    for (let word of paragraph.split(/\s+/)) {
      while (word.length > n) { if (line) { lines.push(line); line = ''; } lines.push(word.slice(0, n)); word = word.slice(n); }
      if (line && line.length + word.length + 1 > n) { lines.push(line); line = ''; }
      line += (line ? ' ' : '') + word;
    }
    if (line) lines.push(line);
  }
  return lines;
}
export function signupLayout(view, width, height, offset = 0) {
  const w = Math.max(100, Math.min(252, width - 24)), x = Math.floor((width - w) / 2);
  const short = height < 270;
  let y = 38;
  const items = [], add = (kind, text, h, extra = {}) => { items.push({ kind, text, x, y, w, h, ...extra }); y += h + 8; };
  add('title', view.title, wrap(view.title, w, fonts.heading).length * 18);
  // The active field and its primary action stay together when the keyboard opens.
  if (!short && view.description) add('copy', view.description, wrap(view.description, w).length * 13);
  if (view.field) add('field', view.field.value || view.field.label, 30, { id: 'field', field: view.field });
  const primary = view.buttons.find(b => b.primary);
  if (primary) add('button', primary.label, Math.max(30, wrap(primary.label, w - 16, fonts.heading).length * 18 + 12), { id: primary.id, disabled: primary.disabled, primary: true });
  if (view.status || view.check) {
    const status = view.status || view.check;
    add('status', status, wrap(status, w).length * 13);
  }
  if (short && view.description) add('copy', view.description, wrap(view.description, w).length * 13);
  for (const b of view.buttons.filter(b => !b.primary && !b.close)) add('button', b.label, Math.max(26, wrap(b.label, w - 16).length * 13 + 12), { id: b.id, disabled: b.disabled });
  const maxScroll = Math.max(0, y + 8 - height);
  const scroll = Math.max(0, Math.min(maxScroll, offset));
  const center = maxScroll ? 0 : Math.max(0, Math.floor((height - y) / 3));
  for (const item of items) item.y += center - scroll;
  items.push({ kind: 'button', id: 'close', text: 'Back', x: 8, y: 8, w: 42, h: 22 });
  items.push({ kind: 'button', id: 'music', text: view.music ? 'Music on' : 'Music off', x: width - 78, y: 8, w: 70, h: 22 });
  return { items, maxScroll, scroll };
}
export function drawOnboarding($, view, layout) {
  const { ink, wipe, screen } = $; wipe(palette.ground);
  const text = (value, x, y, color = palette.text, font = fonts.body) => ink(color).write(value, { x, y }, undefined, undefined, false, font);
  for (const item of layout.items.filter(i => i.id !== 'close' && i.id !== 'music')) {
    const { x, y, w, h } = item;
    if (y + h < 34 || y >= screen.height) continue;
    if (item.kind === 'button' || item.kind === 'field') {
      const fill = item.primary ? palette.pink : palette.panel;
      ink(fill).box(x, y, w, h, 'fill');
      ink(item.field?.focused ? palette.mint : item.primary ? palette.pink : palette.muted).box(x, y, w, h, 'outline');
      let label = item.text;
      if (item.field) {
        const cap = Math.floor((w - 16) / 8) - (item.field.focused ? 1 : 0);
        if (label.length > cap) label = '<' + label.slice(-(cap - 1));
        if (item.field.focused) label += '_';
      }
      const font = item.field || item.primary ? fonts.heading : fonts.body;
      const lines = item.field ? [label] : wrap(label, w - 16, font);
      const top = y + Math.floor((h - lines.length * lineHeight(font)) / 2);
      lines.forEach((line, i) => text(line, x + 8, top + i * lineHeight(font), item.disabled ? palette.muted : item.primary ? palette.ground : palette.text, font));
    } else {
      const font = item.kind === 'title' ? fonts.heading : fonts.body;
      wrap(item.text, w, font).forEach((line, i) => text(line, x, y + i * lineHeight(font), item.kind === 'title' ? palette.pink : item.kind === 'status' ? palette.mint : palette.text, font));
    }
  }
  ink(palette.ground).box(0, 0, screen.width, 34, 'fill');
  for (const item of layout.items.filter(i => i.id === 'close' || i.id === 'music')) text(item.text, item.x + 4, item.y + 7, palette.muted);
  if (layout.maxScroll) ink(palette.mint).box(screen.width - 3, 38 + (screen.height - 50) * layout.scroll / layout.maxScroll, 1, 8, 'fill');
}
