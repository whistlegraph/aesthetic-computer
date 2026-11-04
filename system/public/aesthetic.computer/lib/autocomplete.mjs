// Autocomplete, 2026.01.10
// A reusable autocomplete system for text inputs.
// Supports multiple triggers (@handles, $codes, commands, etc.)

/* #region 📚 README
  Usage:
    import { Autocomplete } from "../lib/autocomplete.mjs";
    
    const ac = new Autocomplete({
      triggers: {
        "@": {
          fetch: async (query) => {
            const res = await fetch(`/api/handles?search=${query}&limit=8`);
            const data = await res.json();
            return data.handles.map(h => ({ text: h, display: `@${h}` }));
          },
          minChars: 1,
          color: [255, 100, 180], // pink for handles
        },
        "$": {
          fetch: async (query) => [...], // KidLisp codes
          color: [100, 220, 255],
        },
      },
      maxResults: 8,
    });

    // In paint():
    ac.paint({ ink, write, box, screen });

    // In act():
    const handled = ac.act(event, inputText, cursorPos);
    if (handled) return; // autocomplete consumed the event

    // Get selected completion:
    if (ac.selected) {
      // Replace text with ac.getCompletedText(inputText, cursorPos)
    }
#endregion */

const { floor, min, max } = Math;

class Autocomplete {
  // Configuration
  triggers = {};
  maxResults = 8;
  debounceMs = 150;

  // State
  visible = false;
  items = [];
  selectedIndex = 0;
  activeTrigger = null;
  triggerPos = -1; // Position of trigger char in text
  query = "";
  loading = false;
  streaming = false; // rows are showing but more sources are still answering
  onRows = null; // (freshRows, allRows) => void — e.g. sonify each arrival

  // True once the user has explicitly arrowed into the list. Used by the
  // prompt so universal (no-trigger) search does NOT hijack Enter until the
  // user has navigated — typing a command + Enter still runs it.
  navigated = false;

  // Mouse/hover state
  hoveredIndex = -1; // Index of item being hovered (-1 = none)
  #dropdownBounds = null; // { x, y, width, height, itemHeight, padding }

  // Internal
  #fetchTimeout = null;
  #lastQuery = "";
  #pendingQuery = ""; // Track query we're currently fetching/waiting for
  #cache = new Map(); // Cache results by trigger+query

  // Universal (no-trigger) search: when no trigger char matches at the
  // cursor but there is a bare query, fall back to this. `universal` is a
  // normal trigger config ({ fetch, minChars, ... }) whose fetch receives
  // the whole trimmed input. Items may carry their own `color` (per-row
  // namespace tag) and `text` is inserted wholesale on completion.
  universal = null;

  constructor(options = {}) {
    this.triggers = options.triggers || {};
    this.maxResults = options.maxResults || 8;
    this.debounceMs = options.debounceMs || 100; // Faster response
    this.universal = options.universal || null;
    this.onRows = options.onRows || null;
  }

  // Set/replace the universal search config after construction.
  setUniversal(config) {
    this.universal = config || null;
  }

  // Add or update a trigger
  addTrigger(char, config) {
    this.triggers[char] = {
      fetch: config.fetch,
      minChars: config.minChars ?? 1,
      color: config.color || [255, 255, 255],
      prefix: config.prefix ?? char, // What to show before each result
      cache: config.cache ?? true,
      ...config,
    };
  }

  // Update autocomplete state based on text and cursor
  update(text, cursorPos) {
    if (!text || cursorPos < 0) {
      this.hide();
      return;
    }

    // Look backwards from cursor for a trigger character
    let foundTrigger = null;
    let foundPos = -1;

    for (let i = cursorPos - 1; i >= 0; i--) {
      const char = text[i];

      // Stop at whitespace (trigger must be start of "word")
      if (/\s/.test(char)) break;

      // Check if this char is a trigger
      if (this.triggers[char]) {
        // Make sure trigger is at word boundary (start of text or after space)
        if (i === 0 || /\s/.test(text[i - 1])) {
          foundTrigger = char;
          foundPos = i;
          break;
        }
      }
    }

    if (!foundTrigger) {
      // No sigil/@ trigger at the cursor — fall back to universal search
      // over the whole input, if configured.
      if (this.universal) {
        const q = text.trim();
        const minChars = this.universal.minChars ?? 1;
        if (q.length < minChars) {
          this.hide();
          return;
        }
        this.activeTrigger = ""; // sentinel: universal mode
        this.triggerPos = 0;
        this.query = q.toLowerCase();
        this.visible = true;
        const cacheKey = `:${this.query}`;
        // Skip if this query is in flight OR already resolved — otherwise the
        // per-frame update() would re-fetch the same query forever (cache:false),
        // leaving the dropdown stuck on "Loading...".
        if (cacheKey === this.#pendingQuery || cacheKey === this.#lastQuery)
          return;
        this.#fetchResults("", this.query, this.universal);
        return;
      }
      this.hide();
      return;
    }

    // Extract query (text between trigger and cursor)
    const query = text.slice(foundPos + 1, cursorPos).toLowerCase();
    const config = this.triggers[foundTrigger];

    // Check minimum characters
    if (query.length < config.minChars) {
      this.hide();
      return;
    }

    this.activeTrigger = foundTrigger;
    this.triggerPos = foundPos;
    this.query = query;
    this.visible = true;

    // Only fetch if query changed
    const cacheKey = `${foundTrigger}:${query}`;
    if (cacheKey === this.#pendingQuery || cacheKey === this.#lastQuery) {
      return; // Already fetching, or already resolved, this query.
    }

    // Fetch results (debounced)
    this.#fetchResults(foundTrigger, query, config);
  }

  async #fetchResults(trigger, query, config) {
    const cacheKey = `${trigger}:${query}`;

    // New query → user hasn't navigated this result set yet.
    this.navigated = false;

    // Track that we're working on this query
    this.#pendingQuery = cacheKey;

    // Check cache first
    if (config.cache && this.#cache.has(cacheKey)) {
      this.items = this.#cache.get(cacheKey);
      this.selectedIndex = 0;
      this.loading = false;
      this.#lastQuery = cacheKey; // Resolved — don't re-fetch until it changes.
      return;
    }

    // Debounce
    if (this.#fetchTimeout) {
      clearTimeout(this.#fetchTimeout);
    }

    // Keep the rows already showing while the next query streams in, so
    // typing reads as a list refining, not a blank "Loading..." flash.
    this.loading = this.items.length === 0;
    this.streaming = true;

    // A fetch may hand back rows as each source lands: `partial(rows)`.
    // Only the newest query may paint; `onRows` hears each new row (bleeps).
    const partial = (rows) => {
      if (this.#pendingQuery !== cacheKey || !Array.isArray(rows)) return;
      this.#show(rows, config);
    };

    this.#fetchTimeout = setTimeout(async () => {
      try {
        const results = await config.fetch(query, { partial });
        if (this.#pendingQuery !== cacheKey) return; // a newer query owns it
        this.#show(results, config);

        // Cache results
        if (config.cache) {
          this.#cache.set(cacheKey, this.items);
        }
      } catch (err) {
        console.warn("Autocomplete fetch error:", err);
        if (this.#pendingQuery === cacheKey) this.items = [];
      } finally {
        if (this.#pendingQuery === cacheKey) {
          this.loading = false;
          this.streaming = false;
          this.#pendingQuery = ""; // No longer in flight.
          this.#lastQuery = cacheKey; // Resolved — don't re-fetch until it changes.
        }
      }
    }, this.debounceMs);
  }

  // Swap in a (partial or final) result list, keeping the highlighted row on
  // the same entry when it survives, and tell `onRows` which rows are new.
  #show(rows, config) {
    const next = rows.slice(0, this.maxResults);
    const key = (it) => it.display || it.text;
    const before = new Set(this.items.map(key));
    const held = this.navigated ? key(this.items[this.selectedIndex] || {}) : null;
    this.items = next;
    this.loading = false;
    const keep = held ? next.findIndex((it) => key(it) === held) : -1;
    this.selectedIndex = keep >= 0 ? keep : 0;
    if (keep < 0) this.navigated = false;
    const fresh = next.filter((it) => !before.has(key(it)));
    if (fresh.length) (config.onRows || this.onRows)?.(fresh, next);
  }

  hide() {
    this.visible = false;
    this.items = [];
    this.selectedIndex = 0;
    this.navigated = false;
    this.activeTrigger = null;
    this.triggerPos = -1;
    this.query = "";
    this.loading = false;
    this.streaming = false;
    this.#pendingQuery = ""; // Clear pending query
    this.#lastQuery = ""; // Allow a fresh fetch when reopened.
    if (this.#fetchTimeout) {
      clearTimeout(this.#fetchTimeout);
      this.#fetchTimeout = null;
    }
  }

  // Handle keyboard events - returns true if event was consumed
  act(event) {
    if (!this.visible || this.items.length === 0) return false;

    if (event.is("keyboard:down:arrowup")) {
      this.selectedIndex = this.selectedIndex > 0
        ? this.selectedIndex - 1
        : this.items.length - 1;
      this.navigated = true;
      return true;
    }

    if (event.is("keyboard:down:arrowdown")) {
      this.selectedIndex = (this.selectedIndex + 1) % this.items.length;
      this.navigated = true;
      return true;
    }

    if (event.is("keyboard:down:tab") || event.is("keyboard:down:enter")) {
      // Return the selected item - caller handles text replacement
      return true; // Signal that selection was made
    }

    if (event.is("keyboard:down:escape")) {
      this.hide();
      return true;
    }

    return false;
  }

  // Handle pointer/mouse events - returns { consumed: bool, clicked: item|null }
  // Call this in act() for touch/pointer events
  handlePointer(event) {
    // Reset hover when not visible
    if (!this.visible || this.items.length === 0 || !this.#dropdownBounds) {
      this.hoveredIndex = -1;
      return { consumed: false, clicked: null };
    }

    const bounds = this.#dropdownBounds;
    const pointer = event.pointer || event;
    const px = pointer.x;
    const py = pointer.y;

    // Check if pointer is within dropdown bounds
    const inBounds = px >= bounds.x && px <= bounds.x + bounds.width &&
                     py >= bounds.y && py <= bounds.y + bounds.height;

    if (!inBounds) {
      this.hoveredIndex = -1;
      return { consumed: false, clicked: null };
    }

    // Calculate which item is being hovered
    const relativeY = py - bounds.y - bounds.padding;
    const itemIndex = floor(relativeY / bounds.itemHeight);
    
    if (itemIndex >= 0 && itemIndex < this.items.length) {
      this.hoveredIndex = itemIndex;

      // Handle click - select and return the clicked item
      if (event.is("touch") || event.is("lift")) {
        this.selectedIndex = itemIndex;
        return { consumed: true, clicked: this.items[itemIndex] };
      }

      // Just hovering - consume to prevent other interactions
      return { consumed: true, clicked: null };
    }

    this.hoveredIndex = -1;
    return { consumed: false, clicked: null };
  }

  // Get the currently selected item
  get selected() {
    if (!this.visible || this.items.length === 0) return null;
    return this.items[this.selectedIndex];
  }

  // Get text with completion applied
  getCompletedText(originalText, cursorPos) {
    const item = this.selected;
    if (!item || this.triggerPos < 0) return originalText;

    // Universal mode: the item carries the full enterable (e.g. "$cow",
    // "@h/abc", "line") — replace the whole input with it.
    if (this.activeTrigger === "") return item.text;

    const before = originalText.slice(0, this.triggerPos);
    const after = originalText.slice(cursorPos);
    const completion = item.text;

    // Include trigger in completion
    const trigger = this.activeTrigger;
    return `${before}${trigger}${completion}${after}`;
  }

  // Get new cursor position after completion
  getCompletedCursorPos(originalText) {
    const item = this.selected;
    if (!item || this.triggerPos < 0) return originalText.length;

    if (this.activeTrigger === "") return item.text.length; // universal

    const before = originalText.slice(0, this.triggerPos);
    const trigger = this.activeTrigger;
    const completion = item.text;

    return before.length + trigger.length + completion.length;
  }

  // Paint the dropdown
  paint({ ink, write, box, screen }, position = {}) {
    if (!this.visible || (this.items.length === 0 && !this.loading)) {
      this.#dropdownBounds = null;
      return;
    }

    const config = this.triggers[this.activeTrigger] || {};
    const inline = position.inline === true; // Render under the typed line, no box.
    const itemHeight = inline ? 11 : 14;
    const padding = inline ? 0 : 4;
    const maxWidth = inline
      ? min(screen.width - (position.x ?? 10) - 4, 240)
      : 120;
    const font = inline ? "MatrixChunky8" : undefined;

    // While loading the universal (bare-word) search, surface live per-source
    // fetch state instead of a blank "Loading..." — this is the debug window
    // into what the backend is actually doing.
    let loadingLines = null;
    if (this.loading) {
      const diag =
        this.activeTrigger === "" ? this.universal?.getDiag?.() : null;
      if (diag?.query) {
        const s = diag.sources;
        const tag = (src) =>
          src.state === "ok"
            ? `${src.n}`
            : src.state === "timeout"
              ? "t/o"
              : src.state;
        loadingLines = [
          `"${diag.query}" ${diag.done ? "done" : "..."}`,
          `cmd ${diag.cmds}  wg ${diag.whistlegraphs ?? 0}  $ ${diag.sigil}`,
          `@ ${tag(s.handles)}  pc ${tag(s.pieces)}`,
          `md ${tag(s.mood)}  ch ${tag(s.chat)}`,
        ];
      } else {
        // Nothing to show yet: the query itself, with a dot that breathes.
        const dots = ".".repeat(1 + (Math.floor(performance.now() / 250) % 3));
        loadingLines = [`${config.prefix || ""}${this.query}${dots}`];
      }
    }

    // Calculate dropdown dimensions. While rows stream in, one dim row of
    // dots under them says more is coming.
    const tail = !this.loading && this.streaming ? 1 : 0;
    const itemCount = this.loading
      ? loadingLines.length
      : this.items.length + tail;
    const dropdownHeight = itemCount * itemHeight + padding * 2;
    
    // Position dropdown BELOW cursor
    const x = position.x ?? 10;
    const y = position.y ?? 80;

    // Inline anchors right at the line; boxed mode clamps + flips to fit.
    const dropX = inline ? x : min(x, screen.width - maxWidth - 4);
    const dropY = !inline && y + dropdownHeight > screen.height - 4
      ? max(4, y - dropdownHeight - 16) // flip above cursor
      : y; // below cursor

    // Store bounds for hover/click detection
    this.#dropdownBounds = { x: dropX, y: dropY, width: maxWidth, height: dropdownHeight, itemHeight, padding };

    if (!inline) {
      ink(20, 20, 30, 240).box(dropX, dropY, maxWidth, dropdownHeight); // Background
      ink(80, 80, 100).box(dropX, dropY, maxWidth, dropdownHeight, "outline"); // Border
    }

    if (this.loading) {
      loadingLines.forEach((ln, i) => {
        const c = i === 0 ? [150, 200, 255] : [120, 140, 160];
        ink(...c).write(
          ln,
          { x: dropX + padding, y: dropY + padding + 2 + i * itemHeight },
          undefined,
          undefined,
          false,
          "MatrixChunky8",
        );
      });
      return;
    }

    // Items
    this.items.forEach((item, i) => {
      const itemY = dropY + padding + i * itemHeight;
      const isSelected = i === this.selectedIndex;
      const isHovered = i === this.hoveredIndex && !isSelected;
      // Per-item `color` (namespace tag) wins over the trigger color so
      // universal results stay type-coded row by row.
      const baseColor = item.color || config.color || [180, 180, 180];
      const color = isSelected
        ? [255, 255, 255]
        : isHovered
          ? [220, 220, 255]
          : baseColor;
      const displayText = item.display || `${config.prefix || ""}${item.text}`;

      if (inline) {
        // Subtle row highlight so each row reads as a tappable button — no
        // enclosing box, just a faint bar behind the selected/hovered row.
        if (isSelected) {
          ink(255, 255, 255, 38).box(dropX - 2, itemY, maxWidth, itemHeight);
        } else if (isHovered) {
          ink(255, 255, 255, 20).box(dropX - 2, itemY, maxWidth, itemHeight);
        }
        // A typed icon leads each row (2026-11-03): a 5x5 pixel mark for the
        // row's kind in its own colour, then the text, then the age of a
        // message or mood at the right edge.
        const icon = KIND_ICONS[item.kind] || KIND_ICONS.command;
        const iconColor = isSelected ? [255, 255, 255] : baseColor;
        icon.forEach((row, r) => {
          for (let col = 0; col < 5; col++)
            if (row[col] === "#") ink(...iconColor).box(dropX + col, itemY + 2 + r, 1, 1);
        });
        const age = item.when ? ago(item.when) : "";
        const textX = dropX + 8;
        const room = Math.max(0, Math.floor((maxWidth - 8 - (age ? age.length * 4 + 6 : 0)) / 4));
        ink(...color).write(
          displayText.slice(0, room),
          { x: textX, y: itemY + 1 },
          undefined,
          undefined,
          false,
          font,
        );
        if (age) {
          ink(130, 130, 150).write(
            age,
            { x: dropX + maxWidth - 2 - age.length * 4, y: itemY + 1 },
            undefined,
            undefined,
            false,
            font,
          );
        }
      } else {
        if (isSelected) {
          ink(60, 60, 100).box(dropX + 2, itemY - 1, maxWidth - 4, itemHeight);
        } else if (isHovered) {
          ink(45, 45, 70).box(dropX + 2, itemY - 1, maxWidth - 4, itemHeight);
        }
        ink(...color).write(displayText, { x: dropX + padding + 2, y: itemY + 2 });
      }
    });

    // More sources still answering: a dim row of drifting dots.
    if (tail) {
      const tailY = dropY + padding + this.items.length * itemHeight;
      const step = Math.floor(performance.now() / 180) % 4;
      const dots = ["·  ", " · ", "  ·", " · "][step];
      ink(110, 120, 140).write(
        (inline ? "  " : "") + dots,
        { x: inline ? dropX : dropX + padding + 2, y: tailY + (inline ? 1 : 2) },
        undefined, undefined, false, font,
      );
    }

    // Query highlight at bottom (optional, boxed mode only)
    if (!inline && this.query && this.items.length > 0) {
      const hintY = dropY + dropdownHeight + 2;
      if (hintY < screen.height - 10) {
        ink(80).write(`↑↓/click · tab/enter`, { x: dropX, y: hintY });
      }
    }
  }

  // Clear cache (useful when data changes)
  clearCache(trigger = null) {
    if (trigger) {
      // Clear only entries for this trigger
      for (const key of this.#cache.keys()) {
        if (key.startsWith(`${trigger}:`)) {
          this.#cache.delete(key);
        }
      }
    } else {
      this.#cache.clear();
    }
  }
}

// Typed icons, 5x5, one per row kind.
const KIND_ICONS = {
  command: ["#....", ".#...", "..#..", ".#...", "#...."],
  piece: ["#####", "#...#", "#...#", "#...#", "#####"],
  handle: [".###.", "#..##", "#.#.#", "#..##", ".###."],
  kidlisp: [".###.", "#.#..", ".###.", "..#.#", ".###."],
  painting: [".#.#.", "#####", ".#.#.", "#####", ".#.#."],
  tape: ["..#..", "..#..", "..#..", ".....", "..#.."],
  whistlegraph: [".....", ".##.#", "#..#.", ".....", "....."],
  mood: [".#.#.", "#####", "#####", ".###.", "..#.."],
  chat: ["#####", "#...#", "#####", ".#...", "#...."],
};

// A message's age, short: 12s, 5m, 3h, 2d, 6w.
function ago(when) {
  const t = new Date(when).getTime();
  if (!Number.isFinite(t)) return "";
  const s = Math.max(0, (Date.now() - t) / 1000);
  if (s < 60) return `${Math.floor(s)}s`;
  if (s < 3600) return `${Math.floor(s / 60)}m`;
  if (s < 86400) return `${Math.floor(s / 3600)}h`;
  if (s < 86400 * 7) return `${Math.floor(s / 86400)}d`;
  return `${Math.floor(s / 86400 / 7)}w`;
}

// Pre-configured handle autocomplete helper.
// Pass `extraTriggers` (a { char: triggerConfig } map) to add sigil/command
// triggers alongside `@` without disturbing callers that only want handles
// (e.g. chat.mjs). Each extra trigger is a normal Autocomplete trigger config
// ({ fetch, minChars, color, prefix, cache, ... }).
function createHandleAutocomplete(options = {}) {
  const { extraTriggers = {}, maxResults, ...rest } = options;
  return new Autocomplete({
    maxResults: maxResults || 8,
    triggers: {
      "@": {
        fetch: async (query) => {
          console.log("🔍 Handle autocomplete fetch:", query);
          try {
            const url = `/api/handles?search=${encodeURIComponent(query)}&limit=20`;
            const res = await fetch(url);
            if (!res.ok) return [];
            const data = await res.json();
            console.log("🔍 API response:", data);
            // API returns { handles: ["string", ...] } or array of objects
            const handles = Array.isArray(data) ? data : (data.handles || []);
            const queryLower = query.toLowerCase();
            const results = handles
              .map(h => ({
                text: typeof h === 'string' ? h : h.handle,
                display: `@${typeof h === 'string' ? h : h.handle}`,
                kind: "handle",
              }))
              .filter(h => h.text) // Filter out undefined
              .sort((a, b) => {
                // Sort: exact prefix match first, then alphabetical
                const aLower = a.text.toLowerCase();
                const bLower = b.text.toLowerCase();
                const aStartsWith = aLower.startsWith(queryLower);
                const bStartsWith = bLower.startsWith(queryLower);
                if (aStartsWith && !bStartsWith) return -1;
                if (!aStartsWith && bStartsWith) return 1;
                return aLower.localeCompare(bLower);
              });
            console.log("🔍 Sorted results:", results);
            return results;
          } catch (err) {
            console.error("🔍 Fetch error:", err);
            return [];
          }
        },
        minChars: 1,
        color: [255, 100, 180], // pink
        prefix: "@",
        cache: true,
      },
      ...extraTriggers,
    },
    ...rest,
  });
}

export { Autocomplete, createHandleAutocomplete };
