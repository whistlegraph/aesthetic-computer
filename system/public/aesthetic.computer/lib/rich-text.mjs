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

export function validateReadAlong(episode) {
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
  return paragraphs;
}

export class RichTextFlow {
  constructor() {
    this.scroll = 0;
    this.layoutKey = null;
    this.regions = [];
    this.gesture = null;
    this.focus = -1;
    this.peekUntil = 0;
    this.pointer = null;
  }
  paint(api, values) {
    this.api = api;
    const now = performance.now();
    const elapsed = Math.min(64, now - (this.lastFrame ?? now));
    this.lastFrame = now;
    const listener = values.find(value => value?.kind === "listen");
    if (listener) {
      const handle = listener.data;
      const episode = handle?.kidlispData ? handle.value : handle;
      if (!episode) values = [richNode("paragraph", [handle?.error || "Loading…"])];
      else {
        if (this.episode !== episode) {
          const paragraphs = validateReadAlong(episode);
          this.stop();
          this.episodeValues = episode.sections ? paragraphs : [richNode("heading", [episode.title]), ...paragraphs];
          this.layoutKey = null;
          this.episode = episode;
          this.colors = null;
          this.fontCharacters = [...new Set(episode.title + episode.body)].join("");
          this.audioUrl = new URL(episode.audio.url, handle.url).href;
          if (!["http:", "https:"].includes(new URL(this.audioUrl).protocol)) throw new Error("Expected http(s) audio");
          this.streamId = `readalong-${Date.now()}-${Math.random()}`;
          this.time = 0;
          this.speed = 1;
          this.duration = Number.isFinite(episode.durationMs) ? episode.durationMs / 1000 : 0;
          this.peekUntil = 0;
          this.autoplayPending = true;
          this.play(api);
        }
        values = this.episodeValues;
      }
    }
    const width = api.screen.width, height = api.screen.height - (this.episode ? 7 : 0);
    const scale = Math.max(1, Math.floor(width / (this.episode ? 48 * 8 : 24 * 6)));
    const margin = (this.episode ? 10 : 4) * scale;
    const bounds = Math.max(8, Math.min(width - 2 * margin, this.episode ? 48 * 8 * scale : width));
    const left = Math.max(margin, Math.floor((width - bounds) / 2));
    const glyphHeight = (this.episode ? 16 : 10) * scale;
    // Episode records are immutable resources: lower once, rebuild on resize.
    // Generic flow expressions remain dynamic and compare their value content.
    const documentKey = listener && this.episode ? this.episode : JSON.stringify(values);
    // BDF widths can change when a full-width glyph arrives asynchronously.
    const fontWidth = this.episode ? api.text.width(this.fontCharacters, "unifont") : 0;
    const key = listener && this.episode ? `${width}:${scale}:${fontWidth}` : JSON.stringify([width, scale, values]);
    if (key !== this.layoutKey) {
      if (this.documentKey !== documentKey) this.scroll = 0;
      this.documentKey = documentKey;
      this.layoutKey = key;
      this.lines = [];
      this.links = [];
      let y = margin, bodyOffset = 0;
      for (const block of richBlocks(values)) {
        const font = this.episode ? "unifont" : undefined;
        const rowHeight = (this.episode ? 23 : block.kind === "heading" ? 11 : 14) * scale;
        // charMap records source indices across newlines, repeated words,
        // dropped wrap spaces and long words; avoid guessing from line text.
        const box = api.text.box(block.text, { x: left, y: 0 }, bounds, scale, true, font);
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
                const x = left + api.text.width(text.slice(0, start), font) * scale;
                const w = api.text.width(text.slice(start, i), font) * scale;
                const region = { ...link, x, y: y + row * rowHeight, w, h: glyphHeight, fragment: text.slice(start, i), id: this.links.length };
                this.links.push(region);
                links.push(region);
                start = -1;
              }
            }
          }
          // Keep each code point's UTF-16 index for canonical word membership.
          const characters = [];
          let index = 0, x = left;
          for (const char of text) {
            const w = api.text.width(char, font) * scale;
            characters.push({ text: char, index, x, w, phase: (this.lines.length * 79 + index * 31) * 0.37 });
            index += char.length; x += w;
          }
          this.lines.push({ text, map, characters, rowHeight, bodyOffset: block.kind === "paragraph" ? bodyOffset : null, x: left, y: y + row * rowHeight, kind: block.kind, font, links });
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
        fragment.x = fragment.line.x + api.text.width(fragment.line.text.slice(0, fragment.a), fragment.line.font) * scale;
        fragment.text = fragment.line.text.slice(fragment.a, fragment.b);
        fragment.w = api.text.width(fragment.text, fragment.line.font) * scale;
      }
      // Give words on the same line distinct, ordered positions, so a small
      // scroll can scrub within a sentence as well as between paragraphs.
      this.wordPositions = [...this.wordFragments].map(([word, fragments]) => {
        const first = fragments[0];
        return { time: word.fromMs / 1000,
          position: first.line.y + (first.x - left) / bounds * first.line.rowHeight };
      });
      const finalWord = words.at(-1), finalFragment = this.wordFragments.get(finalWord)?.at(-1);
      if (finalFragment) this.wordPositions.push({ time: (finalWord.toMs - 1) / 1000,
        position: finalFragment.line.y + (finalFragment.x + finalFragment.w - left) / bounds * finalFragment.line.rowHeight });
      if (this.wordPositions[0]?.time > 0) this.wordPositions.unshift({ time: 0, position: 0 });
      this.scrubUntil = 0;
    }
    this.height = height;
    this.maxScroll = Math.max(0, this.contentHeight - height + margin);
    const words = this.episode?.words || [];
    // Interpolate only between recent media-clock samples; never drift through
    // buffering or a background-tab suspension.
    const timeMs = this.mediaTime(now) * 1000;
    let low = 0, high = words.length;
    while (low < high) {
      const middle = (low + high) >>> 1;
      if (words[middle].fromMs <= timeMs) low = middle + 1;
      else high = middle;
    }
    const candidate = words[low - 1];
    const word = candidate && timeMs < candidate.toMs ? candidate : null;
    if (this.episode) {
      const theme = this.episode.themes?.find(theme => (candidate?.start || 0) >= theme.start && (candidate?.start || 0) < theme.end) || this.episode.themes?.[0] ||
        { background: [246, 235, 196], foreground: [48, 43, 63], highlight: [151, 39, 77], link: [37, 76, 124] };
      this.theme = theme;
      const blend = 1 - Math.exp(-elapsed / 450);
      if (!this.colors) this.colors = Object.fromEntries(['background', 'foreground', 'highlight', 'link'].map(key => [key, [...theme[key]]]));
      for (const key of ['background', 'foreground', 'highlight', 'link']) {
        this.colors[key] = this.colors[key].map((v, i) => v + (theme[key][i] - v) * blend);
      }
      this.inkColors = Object.fromEntries(Object.entries(this.colors).map(([key, value]) => [key, value.map(Math.round)]));
      api.wipe(...this.inkColors.background);
    }
    const fragments = this.wordFragments.get(word) || [];
    const followLine = (this.wordFragments.get(candidate) || [])[0]?.line;
    if (this.episode && now >= this.peekUntil && !this.gesture) {
      const target = Math.max(0, Math.min(this.maxScroll, followLine ? followLine.y - height * 0.43 + glyphHeight / 2 : 0));
      this.scroll += (target - this.scroll) * (1 - Math.exp(-elapsed / 140));
      if (Math.abs(target - this.scroll) < 0.1) this.scroll = target;
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
      if (y + glyphHeight > height) break;
      if (y < 0 || y + glyphHeight > height) continue;
      if (this.episode) {
        for (const link of line.links) this.regions.push({ ...link, y });
        for (const char of line.characters) {
          if (/\s/u.test(char.text)) continue;
          const offset = line.bodyOffset === null ? -1 : line.bodyOffset + line.map[char.index];
          const active = word && offset >= word.start && offset < word.end;
          const link = line.links.find(link => char.x >= link.x && char.x < link.x + link.w);
          const distance = this.pointer ? Math.hypot(char.x + char.w / 2 - this.pointer.x, y + glyphHeight / 2 - this.pointer.y) : Infinity;
          const hover = Math.max(0, 1 - distance / (28 * scale)) * Math.max(0, 1 - (now - (this.pointer?.at || 0)) / 900);
          const amount = (active ? 0.85 : 0) + hover * 2.4;
          const dx = Math.round(Math.sin(now / 95 + char.phase) * amount * scale);
          const dy = Math.round(Math.cos(now / 120 + char.phase * 1.3) * amount * scale);
          api.ink(active ? this.inkColors.highlight : link ? this.inkColors.link : this.inkColors.foreground);
          api.write(char.text, { x: char.x + dx, y: Math.round(y) + dy, size: scale }, undefined, undefined, false, line.font);
          if (link && this.focus === link.id) api.box(char.x, Math.round(y) + glyphHeight, char.w, 1);
        }
        continue;
      }
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
      this.seekX = 4; this.seekWidth = Math.max(1, width - 8);
      api.ink(this.inkColors.background.map((v, i) => Math.round(v * 0.7 + this.inkColors.foreground[i] * 0.3))); api.box(this.seekX, height + 3, this.seekWidth, 1);
      api.ink(this.speed < 0 ? this.inkColors.link : this.inkColors.highlight);
      api.box(this.seekX, height + 3, Math.max(1, this.seekWidth * Math.min(1, (this.time || 0) / (this.duration || 1))), 1);
      if (this.started && now - (this.lastPoll ?? -Infinity) > 40) {
        this.lastPoll = now; api.send({ type: "stream:time", content: { id: this.streamId } });
      }
    }
    if (!this.episode && this.maxScroll > 0) {
      const thumb = Math.max(3, height * height / this.contentHeight);
      api.ink([100, 150, 170]);
      api.box(width - 2, this.scroll / this.maxScroll * (height - thumb), 1, thumb);
    }
  }
  stop() {
    if (this.started) this.api?.send({ type: "stream:stop", content: { id: this.streamId } });
    this.started = false; this.playing = false;
    this.sampleAt = null; this.buffering = false; this.error = null;
    this.samplerReady = false;
  }
  mediaTime(now = performance.now()) {
    const advance = this.playing && !this.buffering && this.sampleAt !== null
      ? Math.max(0, Math.min(120, now - this.sampleAt)) / 1000 : 0;
    const time = (this.time || 0) + advance * (this.speed ?? 1);
    return this.duration > 0 ? (time % this.duration + this.duration) % this.duration : Math.max(0, time);
  }
  play(api) {
    api.send({ type: this.started && !this.error ? "stream:resume" : "stream:play", content: { id: this.streamId, url: this.audioUrl, volume: 0.8, loop: true, scrubbable: true } });
    this.started = true;
  }
  scratch(delta, api, speed = Math.max(-4, Math.min(4, delta * 0.06))) {
    if (!this.episode || !Number.isFinite(delta) || delta === 0) return;
    // Like Stample, pointer motion controls the signed speed of the sample.
    // The BIOS restores normal speed after the gesture, even if paint stops.
    api.send({ type: "stream:speed", content: { id: this.streamId,
      speed, releaseAfter: 140 } });
  }
  timelineAt(value, from, to) {
    const points = this.wordPositions || [];
    if (!points.length) return 0;
    if (value <= points[0][from]) return points[0][to];
    let low = 0, high = points.length;
    while (low < high) {
      const middle = (low + high) >>> 1;
      if (points[middle][from] <= value) low = middle + 1;
      else high = middle;
    }
    const a = points[low - 1], b = points[low];
    if (!b) return a[to];
    const fraction = (value - a[from]) / (b[from] - a[from]);
    return a[to] + fraction * (b[to] - a[to]);
  }
  scrollAudio(delta, api) {
    if (!Number.isFinite(delta) || delta === 0) return;
    this.peek();
    if (!this.episode || !this.wordPositions?.length) {
      this.scroll += delta;
      this.scratch(delta, api);
      return;
    }
    const now = performance.now();
    if (!(now < this.scrubUntil)) {
      this.scrubPosition = this.timelineAt(this.mediaTime(now), "time", "position");
      this.scrubAnchor = Math.max(32, Math.min(this.height - 16, this.scrubPosition - this.scroll));
    }
    this.scrubUntil = now + 200;
    this.scrubPosition = Math.max(this.wordPositions[0].position,
      Math.min(this.wordPositions.at(-1).position, this.scrubPosition + delta));
    this.scroll = this.scrubPosition - this.scrubAnchor;
    this.time = this.timelineAt(this.scrubPosition, "position", "time");
    this.sampleAt = null;
    api.send({ type: "stream:seek", content: { id: this.streamId, time: this.time } });
    // Scratching against the beginning must not wrap to the closing words.
    this.scratch(delta, api, this.time === 0 && delta < 0 ? 0 : undefined);
  }
  peek() {
    this.peekUntil = performance.now() + 1600;
  }
  receive({ type, content }, api) {
    if (content?.id !== this.streamId) return;
    if (type === "stream:playing") { this.playing = true; this.buffering = false; this.error = null; this.autoplayPending = false; }
    if (type === "stream:paused" || type === "stream:error") this.playing = false;
    if (type === "stream:error") this.error = content.error || "Audio error";
    if (type === "stream:speed-data" && Number.isFinite(content.speed)) {
      this.time = this.mediaTime(); this.sampleAt = performance.now();
      this.speed = content.speed;
    }
    if (type === "stream:sampler-ready") this.samplerReady = true;
    if (type === "stream:state") {
      this.buffering = ["connecting", "buffering", "stalled"].includes(content.state);
      if (this.buffering) this.sampleAt = null;
    }
    if (type === "stream:time-data") {
      this.time = content.currentTime; this.duration = content.duration || this.duration;
      if (Number.isFinite(content.speed)) this.speed = content.speed;
      this.sampleAt = performance.now();
      this.playing = !content.paused && !content.ended;
      // Older packed runtimes do not yet set HTMLAudioElement.loop.
      if (content.ended) {
        this.time = 0; this.peekUntil = 0;
        api.send({ type: "stream:seek", content: { id: this.streamId, time: 0 } });
        this.play(api);
      }
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
      if (!this.playing) this.play(api);
    } else if (event.is("move") && this.episode) {
      this.pointer = { x: event.x, y: event.y, at: performance.now() };
    } else if (event.is("touch")) {
      if (this.episode && !this.playing) this.play(api);
      this.pointer = { x: event.x, y: event.y, at: performance.now() };
      this.gesture = { control: this.episode && event.y >= this.controlsY, x: event.x, y: event.y, scroll: this.scroll, link: this.hit(event.x, event.y), dragged: false };
    } else if (event.is("draw") && this.gesture) {
      this.pointer = { x: event.x, y: event.y, at: performance.now() };
      if (Math.hypot(event.x - this.gesture.x, event.y - this.gesture.y) > 2) this.gesture.dragged = true;
      if (this.gesture.dragged) this.peek();
      if (this.gesture.dragged && !this.gesture.control) {
        const delta = (this.gesture.lastY ?? this.gesture.y) - event.y;
        this.scrollAudio(delta, api);
      }
      this.gesture.lastY = event.y;
    } else if (event.is("lift")) {
      if (this.gesture?.control && !this.gesture.dragged && event.y >= this.controlsY) {
        if (this.started) {
          this.time = Math.max(0, Math.min(1, (event.x - this.seekX) / this.seekWidth)) * this.duration;
          this.sampleAt = null; this.peekUntil = 0;
          api.send({ type: "stream:seek", content: { id: this.streamId, time: this.time } });
        }
      }
      const link = this.hit(event.x, event.y);
      if (this.gesture && !this.gesture.dragged && link && link.href === this.gesture.link?.href) this.open(api, link);
      if (this.gesture?.dragged) this.peek();
      this.gesture = null;
    } else if (event.is("scroll")) {
      this.scrollAudio(event.y, api);
    } else if (event.is("keyboard:down:arrowdown")) {
      this.scrollAudio(14, api);
    } else if (event.is("keyboard:down:arrowup")) {
      this.scrollAudio(-14, api);
    } else if (event.is("keyboard:down:tab") && this.links.length) {
      this.focus = (this.focus + 1) % this.links.length;
      const link = this.links[this.focus];
      this.scroll = Math.max(0, link.y - this.height / 2);
      this.peek();
    } else if (event.is("keyboard:down:enter") && this.focus >= 0) {
      this.open(api, this.links[this.focus]);
    } else return false;
    this.scroll = Math.max(0, Math.min(this.maxScroll, this.scroll));
    api.needsPaint();
    return true;
  }
}
