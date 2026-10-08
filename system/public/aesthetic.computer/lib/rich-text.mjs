// Composable canvas prose, laid out with the same text.box API chat uses.
export const richNode = (kind, parts) => ({ richText: true, kind, parts });
export const richString = value => String(value ?? "");
export function richLink(label, destination) {
  const href = richString(destination);
  const url = new URL(href);
  if (!["https:", "http:"].includes(url.protocol) || url.username || url.password) throw new Error("Links need an http(s) URL without credentials");
  return { richText: true, kind: "link", text: richString(label), href: url.href };
}

export function richBlocks(values) {
  return values.map(value => {
    const block = value?.richText && ["heading", "paragraph"].includes(value.kind) ? value : richNode("paragraph", [value]);
    let text = "";
    const links = [];
    for (const part of block.parts) {
      const label = part?.kind === "link" ? part.text : richString(part);
      if (part?.kind === "link") links.push({ start: text.length, end: text.length + label.length, text: label, href: richLink(label, part.href).href });
      text += label;
    }
    return { kind: block.kind, text, links };
  });
}

export class RichTextFlow {
  constructor() {
    this.scroll = 0;
    this.layoutKey = null;
    this.regions = [];
    this.gesture = null;
    this.focus = -1;
  }
  paint(api, values) {
    this.api = api;
    const listener = values.find(value => value?.kind === "listen");
    if (listener) {
      const handle = listener.data;
      const episode = handle?.kidlispData ? handle.value : handle;
      if (!episode) values = [richNode("paragraph", [handle?.error || "Loading…"])];
      else {
        if (this.episode !== episode) {
          const paragraphs = episode.blocks || episode.body.split("\n\n").map(text => richNode("paragraph", [text]));
          if (richBlocks(paragraphs).map(block => block.text).join("\n\n") !== episode.body) throw new Error("Episode blocks differ from canonical prose");
          let previousEnd = 0, previousOffset = 0;
          for (const word of episode.words || []) {
            if (![word.start, word.end, word.fromMs, word.toMs].every(Number.isFinite) ||
                !Number.isInteger(word.start) || !Number.isInteger(word.end) ||
                word.start < previousOffset || word.end <= word.start || word.end > episode.body.length ||
                word.fromMs < previousEnd || word.toMs <= word.fromMs) throw new Error("Expected ordered, nonoverlapping word cues within canonical prose");
            previousEnd = word.toMs; previousOffset = word.end;
          }
          this.stop();
          this.episodeValues = [richNode("heading", [episode.title]), ...paragraphs];
          this.layoutKey = null;
          this.episode = episode;
          this.audioUrl = new URL(episode.audio.url, handle.url).href;
          if (!["http:", "https:"].includes(new URL(this.audioUrl).protocol)) throw new Error("Expected http(s) audio");
          this.streamId = `readalong-${Date.now()}-${Math.random()}`;
          this.time = 0;
          this.duration = Number.isFinite(episode.durationMs) ? episode.durationMs / 1000 : 0;
          this.follow = true;
        }
        values = this.episodeValues;
      }
    }
    const width = api.screen.width, height = api.screen.height - (this.episode ? 14 : 0);
    const scale = Math.max(1, Math.floor(width / (24 * 6)));
    const margin = 4 * scale;
    const bounds = Math.max(6, width - 2 * margin);
    // Episode records are immutable resources: lower once, rebuild on resize.
    // Generic flow expressions remain dynamic and compare their value content.
    const documentKey = listener && this.episode ? this.episode : JSON.stringify(values);
    const key = listener && this.episode ? `${width}:${scale}` : JSON.stringify([width, scale, values]);
    if (key !== this.layoutKey) {
      if (this.documentKey !== documentKey) this.scroll = 0;
      this.documentKey = documentKey;
      this.layoutKey = key;
      this.lines = [];
      this.links = [];
      let y = margin, bodyOffset = 0;
      for (const block of richBlocks(values)) {
        const font = undefined;
        const rowHeight = (block.kind === "heading" ? 11 : 14) * scale;
        // charMap records source indices across newlines, repeated words,
        // dropped wrap spaces and long words; avoid guessing from line text.
        const box = api.text.box(block.text, { x: margin, y: 0 }, bounds, scale, true, font);
        if (!box) continue;
        box.lines.forEach((text, row) => {
          const map = box.charMap[row];
          const links = [];
          for (const link of block.links) {
            let start = -1;
            for (let i = 0; i <= text.length; i++) {
              const inside = i < text.length && map[i] >= link.start && map[i] < link.end;
              if (inside && start < 0) start = i;
              if (!inside && start >= 0) {
                const x = margin + api.text.width(text.slice(0, start), font) * scale;
                const w = api.text.width(text.slice(start, i), font) * scale;
                const region = { ...link, x, y: y + row * rowHeight, w, h: 10 * scale, fragment: text.slice(start, i), id: this.links.length };
                this.links.push(region);
                links.push(region);
                start = -1;
              }
            }
          }
          this.lines.push({ text, map, bodyOffset: block.kind === "paragraph" ? bodyOffset : null, x: margin, y: y + row * rowHeight, kind: block.kind, font, links });
        });
        if (block.kind === "paragraph") bodyOffset += block.text.length + 2;
        y += box.lines.length * rowHeight + (block.kind === "heading" ? 2 : 8) * scale;
      }
      this.contentHeight = y;
      // Lower source offsets into positioned fragments in one ordered pass.
      this.wordFragments = new Map();
      const words = this.episode?.words || [];
      let wordIndex = 0;
      for (const line of this.lines) {
        if (line.bodyOffset === null) continue;
        for (let i = 0; i < line.map.length; i++) {
          if (line.map[i] < 0) continue;
          const offset = line.bodyOffset + line.map[i];
          while (wordIndex < words.length && words[wordIndex].end <= offset) wordIndex++;
          const word = words[wordIndex];
          if (!word || offset < word.start) continue;
          let fragments = this.wordFragments.get(word);
          if (!fragments) this.wordFragments.set(word, fragments = []);
          const last = fragments.at(-1);
          if (last?.line === line && last.b === i) last.b++;
          else fragments.push({ line, a: i, b: i + 1 });
        }
      }
      for (const fragments of this.wordFragments.values()) for (const fragment of fragments) {
        fragment.x = fragment.line.x + api.text.width(fragment.line.text.slice(0, fragment.a)) * scale;
        fragment.text = fragment.line.text.slice(fragment.a, fragment.b);
        fragment.w = api.text.width(fragment.text) * scale;
      }
    }
    this.height = height;
    this.maxScroll = Math.max(0, this.contentHeight - height + margin);
    const words = this.episode?.words || [];
    const timeMs = this.time * 1000;
    let low = 0, high = words.length;
    while (low < high) {
      const middle = (low + high) >>> 1;
      if (words[middle].fromMs <= timeMs) low = middle + 1;
      else high = middle;
    }
    const candidate = words[low - 1];
    const word = candidate && timeMs < candidate.toMs ? candidate : null;
    const fragments = this.wordFragments.get(word) || [];
    if (word && this.follow && this.playing) {
      const line = fragments[0]?.line;
      if (line) this.scroll = Math.max(0, line.y - height / 2 + 5 * scale);
    }
    this.scroll = Math.max(0, Math.min(this.maxScroll, this.scroll));
    this.regions = [];
    let first = 0, last = this.lines.length;
    while (first < last) {
      const middle = (first + last) >>> 1;
      if (this.lines[middle].y < this.scroll) first = middle + 1;
      else last = middle;
    }
    for (let row = first; row < this.lines.length; row++) {
      const line = this.lines[row];
      const y = line.y - this.scroll;
      if (y + 10 * scale > height) break;
      if (y < 0 || y + 10 * scale > height) continue;
      api.ink(line.kind === "heading" ? [255, 170, 210] : [235, 245, 255]);
      api.write(line.text, { x: line.x, y, size: scale }, undefined, undefined, false, line.font);
      for (const link of line.links) {
        const region = { ...link, y };
        api.ink(this.focus === link.id ? [65, 95, 120] : [15, 55, 75]);
        api.box(link.x - 1, y - 1, link.w + 2, link.h + 2);
        api.ink([140, 235, 255]);
        api.write(link.fragment, { x: link.x, y, size: scale });
        this.regions.push(region);
      }
      for (const fragment of fragments) {
        if (fragment.line !== line) continue;
        api.ink([255, 225, 90]);
        api.box(fragment.x - 1, y - 1, fragment.w + 2, 10 * scale + 2);
        api.ink([15, 20, 25]);
        api.write(fragment.text, { x: fragment.x, y, size: scale });
      }
    }
    if (this.episode) {
      this.controlsY = height;
      api.ink([25, 40, 50]); api.box(0, height, width, 14);
      api.ink([235, 245, 255]);
      api.write(this.error ? "Audio error" : this.playing ? "Pause" : "Play", { x: 4, y: height + 3 });
      api.write(this.follow ? "Follow" : "Free", { x: 44, y: height + 3 });
      api.ink([100, 150, 170]); api.box(90, height + 6, Math.max(1, width - 94), 2);
      api.ink([255, 225, 90]); api.box(90, height + 6, Math.max(1, (width - 94) * (this.time || 0) / (this.duration || 1)), 2);
      if (this.started && Date.now() - (this.lastPoll || 0) > 80) {
        this.lastPoll = Date.now(); api.send({ type: "stream:time", content: { id: this.streamId } });
      }
    }
    if (this.maxScroll > 0) {
      const thumb = Math.max(3, height * height / this.contentHeight);
      api.ink([100, 150, 170]);
      api.box(width - 2, this.scroll / this.maxScroll * (height - thumb), 1, thumb);
    }
  }
  stop() {
    if (this.started) this.api?.send({ type: "stream:stop", content: { id: this.streamId } });
    this.started = false; this.playing = false;
  }
  receive({ type, content }, api) {
    if (content?.id !== this.streamId) return;
    if (type === "stream:playing") { this.playing = true; this.error = null; }
    if (type === "stream:paused" || type === "stream:error") this.playing = false;
    if (type === "stream:error") this.error = content.error || "Audio error";
    if (type === "stream:time-data") {
      this.time = content.currentTime; this.duration = content.duration || this.duration;
      this.playing = !content.paused && !content.ended;
    }
    api.needsPaint();
  }
  hit(x, y) {
    return this.regions.find(r => x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h);
  }
  open(api, link) {
    // Keep the reader open even when browser popup policy blocks a new tab.
    api.send({ type: "web", content: { url: link.href, blank: true, preserve: true } });
  }
  act(event, api) {
    if (event.is("keyboard:down:space") && this.episode) {
      api.send({ type: this.started ? this.playing ? "stream:pause" : "stream:resume" : "stream:play", content: { id: this.streamId, url: this.audioUrl, volume: 0.8 } });
      this.started = true;
    } else if (event.is("touch")) {
      this.gesture = { control: this.episode && event.y >= this.controlsY, x: event.x, y: event.y, scroll: this.scroll, link: this.hit(event.x, event.y), dragged: false };
    } else if (event.is("draw") && this.gesture) {
      if (Math.hypot(event.x - this.gesture.x, event.y - this.gesture.y) > 2) this.gesture.dragged = true;
      if (this.gesture.dragged) { this.follow = false; }
      if (this.gesture.dragged && !this.gesture.control) this.scroll = this.gesture.scroll + this.gesture.y - event.y;
    } else if (event.is("lift")) {
      if (this.gesture?.control && !this.gesture.dragged && event.y >= this.controlsY) {
        if (event.x < 40) {
          api.send({ type: this.started ? this.playing ? "stream:pause" : "stream:resume" : "stream:play", content: { id: this.streamId, url: this.audioUrl, volume: 0.8 } });
          this.started = true;
        } else if (event.x < 88) this.follow = !this.follow;
        else if (this.started) api.send({ type: "stream:seek", content: { id: this.streamId, time: Math.max(0, Math.min(1, (event.x - 90) / (api.screen.width - 94))) * this.duration } });
      }
      const link = this.hit(event.x, event.y);
      if (this.gesture && !this.gesture.dragged && link && link.href === this.gesture.link?.href) this.open(api, link);
      this.gesture = null;
    } else if (event.is("scroll")) {
      this.follow = false;
      this.scroll += event.y;
    } else if (event.is("keyboard:down:arrowdown")) {
      this.follow = false;
      this.scroll += 14;
    } else if (event.is("keyboard:down:arrowup")) {
      this.follow = false;
      this.scroll -= 14;
    } else if (event.is("keyboard:down:tab") && this.links.length) {
      this.focus = (this.focus + 1) % this.links.length;
      const link = this.links[this.focus];
      this.scroll = Math.max(0, link.y - this.height / 2);
    } else if (event.is("keyboard:down:enter") && this.focus >= 0) {
      this.open(api, this.links[this.focus]);
    } else return false;
    this.scroll = Math.max(0, Math.min(this.maxScroll, this.scroll));
    api.needsPaint();
    return true;
  }
}
