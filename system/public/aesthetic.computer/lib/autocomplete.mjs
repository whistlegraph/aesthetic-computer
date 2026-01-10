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

  // Mouse/hover state
  hoveredIndex = -1; // Index of item being hovered (-1 = none)
  #dropdownBounds = null; // { x, y, width, height, itemHeight, padding }

  // Internal
  #fetchTimeout = null;
  #lastQuery = "";
  #pendingQuery = ""; // Track query we're currently fetching/waiting for
  #cache = new Map(); // Cache results by trigger+query

  constructor(options = {}) {
    this.triggers = options.triggers || {};
    this.maxResults = options.maxResults || 8;
    this.debounceMs = options.debounceMs || 100; // Faster response
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
    if (cacheKey === this.#pendingQuery) {
      return; // Already fetching this query
    }

    // Fetch results (debounced)
    this.#fetchResults(foundTrigger, query, config);
  }

  async #fetchResults(trigger, query, config) {
    const cacheKey = `${trigger}:${query}`;

    // Track that we're working on this query
    this.#pendingQuery = cacheKey;

    // Check cache first
    if (config.cache && this.#cache.has(cacheKey)) {
      this.items = this.#cache.get(cacheKey);
      this.selectedIndex = 0;
      this.loading = false;
      return;
    }

    // Debounce
    if (this.#fetchTimeout) {
      clearTimeout(this.#fetchTimeout);
    }

    this.loading = true;

    this.#fetchTimeout = setTimeout(async () => {
      console.log("🔍 Handle autocomplete fetch:", query);
      try {
        const results = await config.fetch(query);
        console.log("🔍 Fetch results:", results);
        this.items = results.slice(0, this.maxResults);
        this.selectedIndex = 0;

        // Cache results
        if (config.cache) {
          this.#cache.set(cacheKey, this.items);
        }
      } catch (err) {
        console.warn("Autocomplete fetch error:", err);
        this.items = [];
      } finally {
        this.loading = false;
        this.#pendingQuery = ""; // Clear pending query
      }
    }, this.debounceMs);
  }

  hide() {
    this.visible = false;
    this.items = [];
    this.selectedIndex = 0;
    this.activeTrigger = null;
    this.triggerPos = -1;
    this.query = "";
    this.loading = false;
    this.#pendingQuery = ""; // Clear pending query
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
      return true;
    }

    if (event.is("keyboard:down:arrowdown")) {
      this.selectedIndex = (this.selectedIndex + 1) % this.items.length;
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
    const itemHeight = 14;
    const padding = 4;
    const maxWidth = 120;

    // Calculate dropdown dimensions
    const itemCount = this.loading ? 1 : this.items.length;
    const dropdownHeight = itemCount * itemHeight + padding * 2;
    
    // Position dropdown BELOW cursor
    const x = position.x ?? 10;
    const y = position.y ?? 80;

    // Clamp to screen bounds
    const dropX = min(x, screen.width - maxWidth - 4);
    // Only flip above if truly no room below
    const dropY = (y + dropdownHeight > screen.height - 4) 
      ? max(4, y - dropdownHeight - 16) // flip above cursor
      : y; // below cursor

    // Store bounds for hover/click detection
    this.#dropdownBounds = { x: dropX, y: dropY, width: maxWidth, height: dropdownHeight, itemHeight, padding };

    // Background
    ink(20, 20, 30, 240).box(dropX, dropY, maxWidth, dropdownHeight);

    // Border
    ink(80, 80, 100).box(dropX, dropY, maxWidth, dropdownHeight, "outline");

    if (this.loading) {
      ink(120).write("Loading...", { x: dropX + padding, y: dropY + padding + 2 });
      return;
    }

    // Items
    this.items.forEach((item, i) => {
      const itemY = dropY + padding + i * itemHeight;
      const isSelected = i === this.selectedIndex;
      const isHovered = i === this.hoveredIndex && !isSelected;

      // Selection highlight (keyboard selection takes priority)
      if (isSelected) {
        ink(60, 60, 100).box(dropX + 2, itemY - 1, maxWidth - 4, itemHeight);
      } else if (isHovered) {
        // Hover highlight (different color from selection)
        ink(45, 45, 70).box(dropX + 2, itemY - 1, maxWidth - 4, itemHeight);
      }

      // Item text
      const color = isSelected ? [255, 255, 255] : (isHovered ? [220, 220, 255] : (config.color || [180, 180, 180]));
      const displayText = item.display || `${config.prefix || ""}${item.text}`;
      ink(...color).write(displayText, { x: dropX + padding + 2, y: itemY + 2 });
    });

    // Query highlight at bottom (optional)
    if (this.query && this.items.length > 0) {
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

// Pre-configured handle autocomplete helper
function createHandleAutocomplete(options = {}) {
  return new Autocomplete({
    maxResults: options.maxResults || 8,
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
    },
    ...options,
  });
}

export { Autocomplete, createHandleAutocomplete };
