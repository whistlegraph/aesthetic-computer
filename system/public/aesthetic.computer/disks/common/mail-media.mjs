// Mail-only previews stay in memory; private bytes never enter the public
// bitmap loader, persistent browser store, or piece diagnostics.
export function mailLinks(text) {
  const links = new Set();
  for (const match of (text || "").matchAll(/https?:\/\/[^\s<>"']+/gi)) {
    let raw = match[0].replace(/[.,;]+$/, "");
    for (const [open, close] of [["(", ")"], ["[", "]"]]) {
      while (raw.endsWith(close) && raw.split(close).length > raw.split(open).length) raw = raw.slice(0, -1);
    }
    try {
      const url = new URL(raw);
      if (["https:", "http:"].includes(url.protocol) && !url.username && !url.password) links.add(url.href);
    } catch {}
    if (links.size === 10) break;
  }
  return [...links];
}

export class MailMedia {
  previews = new Map();
  active = 0;
  generation = 0;

  clear() {
    this.generation++;
    this.previews.clear();
  }

  layout(api, letter, width, words) {
    const items = [
      ...(letter.media || []).map((ref) => ({
        key: ref.label, label: ref.label, image: !!ref.preview, ref,
      })),
      ...mailLinks(letter.text).filter((url) => !(letter.media || []).some((ref) => ref.url === url))
        .map((url) => ({ key: url, label: `${words.openLink} ${url}`, url })),
      ...(letter.attachments || []).map((file) => ({
        key: `${letter.id}:${file.index}`, label: `${words.download} ${file.name} (${Math.max(1, Math.ceil(file.size / 1024))} KiB)`,
        image: file.image, file, id: letter.id,
      })),
    ];
    for (const item of items) {
      item.face = api.screen.width < 320 || api.screen.height < 220 ? "MatrixChunky8" : undefined;
      const labelWidth = Math.max(32, width - (item.image ? 110 : 0));
      const labelH = api.text.box(item.label, undefined, labelWidth, 1, true, item.face).box.height;
      item.height = Math.max(item.image ? 80 : 14, labelH + 8);
    }
    return items;
  }

  async load(api, item) {
    if (this.previews.has(item.key) || this.active >= 2) return;
    const generation = this.generation;
    this.previews.set(item.key, { loading: true });
    this.active++;
    let image;
    try {
      if (item.ref) {
        image = (await api.get.picture(item.ref.preview)).img;
      } else {
        const res = await api.net.userRequest("GET", `/api/mail?id=${item.id}&attachment=${item.file.index}&preview=1`);
        if (res.status !== 200) throw new Error("Preview unavailable");
        const bytes = Uint8Array.from(atob(res.data), (c) => c.charCodeAt(0));
        const bitmap = await createImageBitmap(new Blob([bytes], { type: "image/png" }));
        try {
          const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
          const ctx = canvas.getContext("2d");
          ctx.drawImage(bitmap, 0, 0);
          image = { width: bitmap.width, height: bitmap.height, pixels: ctx.getImageData(0, 0, bitmap.width, bitmap.height).data };
        } finally { bitmap.close(); }
      }
    } catch { /* Keep the download available even if previewing fails. */ }
    finally {
      this.active--;
      if (generation === this.generation) {
        this.previews.set(item.key, { image });
      }
      api.needsPaint();
    }
  }

  paint(api, item, x, y, width, color, words) {
    if (item.image) {
      this.load(api, item);
      const cached = this.previews.get(item.key);
      if (cached?.image) {
        const image = cached.image;
        const scale = Math.min(104 / image.width, 72 / image.height);
        const w = Math.max(1, Math.floor(image.width * scale));
        const h = Math.max(1, Math.floor(image.height * scale));
        api.paste(image, x + Math.floor((104 - w) / 2), y + 4, { width: w, height: h });
      } else {
        api.ink(color).write(cached?.loading || !cached ? words.loading : words.noPreview,
          { x, y: y + 8 }, undefined, 104, true, item.face);
      }
    }
    const labelX = x + (item.image ? 110 : 0);
    api.ink(color).write(item.label, { x: labelX, y: y + 4 }, undefined, width - (labelX - x), true, item.face);
  }

  async open(api, item) {
    if (item.url) return api.net.web(item.url, true);
    if (item.ref) return api.jump(item.ref.path);
    const res = await api.net.userRequest("GET", `/api/mail?id=${item.id}&attachment=${item.file.index}&json=1`);
    if (res.status !== 200) throw new Error("Download unavailable");
    api.download(res.name, atob(res.data), { encoding: "binary", private: true });
  }
}
