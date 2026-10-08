// safari-page.mjs — the in-page half of puppet's Safari driver. Safari has no
// CDP and Playwright cannot attach to the real (logged-in) Safari, so the
// semantic verbs (snapshot / locate / fill / wait) run as plain DOM code that
// AppleScript's `do JavaScript` injects into the tab. Semantics mirror
// puppet-semantic.mjs: exactly one locator kind, exact names, strict matching
// (more than one match is an error), role locators skip hidden elements.
//
// Every entry point returns a JSON string, because that is the one value type
// that survives the Safari → AppleScript → osascript → node trip unchanged.

// The library is written as a real function so it stays lintable; we ship
// its source text into the page.
function pageLibrary() {
  const norm = s => String(s || "").replace(/\s+/g, " ").trim();
  const IMPLICIT = {
    A: el => el.hasAttribute("href") ? "link" : null,
    AREA: el => el.hasAttribute("href") ? "link" : null,
    BUTTON: () => "button", SUMMARY: () => "button",
    TEXTAREA: () => "textbox", SELECT: el => el.multiple || el.size > 1 ? "listbox" : "combobox",
    OPTION: () => "option", IMG: el => el.getAttribute("alt") === "" ? "presentation" : "img",
    H1: () => "heading", H2: () => "heading", H3: () => "heading", H4: () => "heading", H5: () => "heading", H6: () => "heading",
    NAV: () => "navigation", MAIN: () => "main", ASIDE: () => "complementary", FORM: () => "form",
    HEADER: el => el.closest("article,aside,main,nav,section") ? null : "banner",
    FOOTER: el => el.closest("article,aside,main,nav,section") ? null : "contentinfo",
    DIALOG: () => "dialog", UL: () => "list", OL: () => "list", LI: () => "listitem",
    TABLE: () => "table", TR: () => "row", TD: () => "cell", TH: () => "columnheader",
    PROGRESS: () => "progressbar", HR: () => "separator", ARTICLE: () => "article",
    INPUT: el => {
      const t = (el.getAttribute("type") || "text").toLowerCase();
      if (["button", "submit", "reset", "image"].includes(t)) return "button";
      if (t === "checkbox") return el.getAttribute("role") === "switch" ? "switch" : "checkbox";
      if (t === "radio") return "radio";
      if (t === "range") return "slider";
      if (t === "number") return "spinbutton";
      if (t === "search") return el.hasAttribute("list") ? "combobox" : "searchbox";
      if (t === "hidden" || t === "file" || t === "color") return null;
      return el.hasAttribute("list") ? "combobox" : "textbox";
    },
  };
  const role = el => {
    const explicit = norm(el.getAttribute && el.getAttribute("role")).split(" ")[0];
    if (explicit) return explicit;
    if (el.isContentEditable && (!el.parentElement || !el.parentElement.isContentEditable)) return "textbox";
    const f = IMPLICIT[el.tagName];
    return f ? f(el) : null;
  };
  const root = () => document;
  // Walk light DOM plus open shadow roots, in document order.
  function* walk(node) {
    for (let c = node.firstElementChild; c; c = c.nextElementSibling) {
      yield c;
      if (c.shadowRoot) yield* walk(c.shadowRoot);
      yield* walk(c);
    }
  }
  const all = () => Array.from(walk(document));
  const byId = id => document.getElementById(id);
  const visible = el => {
    if (!el.isConnected) return false;
    if (typeof el.checkVisibility === "function" && !el.checkVisibility({ checkOpacity: false, checkVisibilityCSS: true })) return false;
    const r = el.getBoundingClientRect();
    if (r.width <= 0 || r.height <= 0) {
      // display:contents wrappers have no box but their children do.
      return getComputedStyle(el).display === "contents" && Array.from(el.children).some(visible);
    }
    return getComputedStyle(el).visibility !== "hidden";
  };
  const ariaHidden = el => !!el.closest("[aria-hidden='true'],[inert]");
  function textOf(node, depth = 0) {
    if (depth > 30) return "";
    if (node.nodeType === 3) return node.nodeValue;
    if (node.nodeType !== 1) return "";
    const el = node;
    if (["SCRIPT", "STYLE", "NOSCRIPT", "TEMPLATE"].includes(el.tagName)) return "";
    if (depth > 0 && el.getAttribute("aria-hidden") === "true") return "";
    if (depth > 0 && el.getAttribute("aria-label")) return " " + el.getAttribute("aria-label") + " ";
    if (el.tagName === "IMG") return " " + (el.getAttribute("alt") || "") + " ";
    if (el.tagName === "INPUT" && ["button", "submit", "reset"].includes(el.type)) return " " + el.value + " ";
    const kids = el.shadowRoot ? el.shadowRoot.childNodes : el.childNodes;
    let out = "";
    for (const k of kids) out += textOf(k, depth + 1);
    if (el.tagName === "SLOT") for (const k of el.assignedNodes()) out += textOf(k, depth + 1);
    const block = depth > 0 && /^(block|flex|grid|list-item|table)/.test(getComputedStyle(el).display);
    return block ? " " + out + " " : out;
  }
  const labelsFor = el => {
    const out = [];
    if (el.labels) for (const l of el.labels) out.push(norm(textOf(l)));
    return out.filter(Boolean);
  };
  const FROM_CONTENT = new Set(["button", "link", "heading", "cell", "columnheader", "rowheader", "option",
    "tab", "menuitem", "menuitemcheckbox", "menuitemradio", "treeitem", "checkbox", "radio", "switch", "tooltip", "listitem", "row"]);
  function name(el) {
    const lb = el.getAttribute("aria-labelledby");
    if (lb) {
      const t = norm(lb.split(/\s+/).map(id => { const n = byId(id); return n ? textOf(n) : ""; }).join(" "));
      if (t) return t;
    }
    const al = norm(el.getAttribute("aria-label"));
    if (al) return al;
    if (["INPUT", "TEXTAREA", "SELECT"].includes(el.tagName)) {
      const ls = labelsFor(el);
      if (ls.length) return norm(ls.join(" "));
      if (el.tagName === "INPUT" && ["button", "submit", "reset"].includes(el.type)) return norm(el.value || (el.type === "submit" ? "Submit" : ""));
      if (el.tagName === "INPUT" && el.type === "image") return norm(el.alt);
    }
    if (el.tagName === "IMG") return norm(el.getAttribute("alt") || el.title);
    const r = role(el);
    if (r && FROM_CONTENT.has(r)) { const t = norm(textOf(el)); if (t) return t; }
    return norm(el.getAttribute("title") || el.getAttribute("placeholder"));
  }
  const disabled = el => !!(el.disabled || el.closest("[aria-disabled='true']") || el.closest("fieldset:disabled"));

  function candidates(loc) {
    const kinds = ["role", "label", "text", "testId", "css"].filter(k => typeof loc[k] === "string" && loc[k]);
    if (kinds.length !== 1) throw new Error("Choose exactly one locator: role, label, text, testId, or css");
    if (loc.role) {
      if (typeof loc.name !== "string" || !loc.name) throw new Error("Role locators require an exact accessible name");
      return all().filter(el => role(el) === loc.role && !ariaHidden(el) && visible(el) && name(el) === norm(loc.name));
    }
    if (loc.label) {
      const want = norm(loc.label);
      return all().filter(el => {
        if (!(["INPUT", "TEXTAREA", "SELECT"].includes(el.tagName) || el.isContentEditable || el.getAttribute("aria-label") || el.getAttribute("aria-labelledby"))) return false;
        if (norm(el.getAttribute("aria-label")) === want) return true;
        const lb = el.getAttribute("aria-labelledby");
        if (lb && norm(lb.split(/\s+/).map(id => { const n = byId(id); return n ? textOf(n) : ""; }).join(" ")) === want) return true;
        return labelsFor(el).includes(want);
      });
    }
    if (loc.text) {
      const want = norm(loc.text);
      const hits = all().filter(el => !["SCRIPT", "STYLE", "HEAD", "TITLE", "HTML", "BODY"].includes(el.tagName) && norm(textOf(el)) === want);
      // Keep the innermost matches, as Playwright does.
      return hits.filter(el => !hits.some(o => o !== el && el.contains(o)));
    }
    if (loc.testId) return all().filter(el => el.getAttribute("data-testid") === loc.testId);
    return Array.from(document.querySelectorAll(loc.css));
  }
  function resolve(loc) {
    const found = candidates(loc);
    if (found.length !== 1) return { count: found.length };
    return { count: 1, el: found[0] };
  }
  function stateOf(loc) {
    const found = candidates(loc);
    return { attached: found.length > 0, visible: found.some(visible), count: found.length };
  }
  function satisfied(loc, state) {
    const s = stateOf(loc);
    if (state === "attached") return s.attached;
    if (state === "detached") return !s.attached;
    if (state === "hidden") return !s.visible;
    return s.visible;
  }
  // Actionability: unique, visible, enabled, scrolled into view, not covered.
  function actionable(loc, { editable = false } = {}) {
    const { count, el } = resolve(loc);
    if (count === 0) return { ready: false, reason: "no element matches the locator" };
    if (count > 1) return { ready: false, strict: true, reason: `strict mode violation: locator matches ${count} elements` };
    if (!visible(el)) return { ready: false, reason: "element is not visible" };
    if (disabled(el)) return { ready: false, reason: "element is disabled" };
    if (editable && (el.readOnly || !(el.isContentEditable || ["INPUT", "TEXTAREA"].includes(el.tagName))))
      return { ready: false, reason: el.tagName === "SELECT" ? "fill cannot target a <select>" : "element is not editable" };
    let r = el.getBoundingClientRect();
    if (r.top < 0 || r.left < 0 || r.bottom > innerHeight || r.right > innerWidth) {
      el.scrollIntoView({ block: "center", inline: "center", behavior: "instant" });
      r = el.getBoundingClientRect();
    }
    const x = r.left + r.width / 2, y = r.top + r.height / 2;
    if (!editable) {
      let hit = document.elementFromPoint(x, y);
      while (hit && hit.shadowRoot) { const deeper = hit.shadowRoot.elementFromPoint(x, y); if (!deeper || deeper === hit) break; hit = deeper; }
      const inside = n => { for (let c = n; c; c = c.parentNode || c.host) if (c === el) return true; return false; };
      if (!hit || !inside(hit)) return { ready: false, reason: `element is covered by <${hit ? hit.tagName.toLowerCase() : "nothing"}>` };
    }
    return { ready: true, x, y, rect: [r.left, r.top, r.width, r.height] };
  }
  function fill(loc, value) {
    const a = actionable(loc, { editable: true });
    if (!a.ready) return a;
    const { el } = resolve(loc);
    el.focus();
    if (el.isContentEditable) {
      const sel = getSelection(), range = document.createRange();
      range.selectNodeContents(el); sel.removeAllRanges(); sel.addRange(range);
      document.execCommand(value ? "insertText" : "delete", false, value);
    } else {
      // The prototype setter is what React-style controlled inputs observe.
      const proto = el.tagName === "TEXTAREA" ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
      Object.getOwnPropertyDescriptor(proto, "value").set.call(el, value);
      el.dispatchEvent(new InputEvent("input", { bubbles: true, composed: true, inputType: "insertText", data: value }));
      el.dispatchEvent(new Event("change", { bubbles: true }));
    }
    return { ready: true, filled: true };
  }

  // Playwright-ariaSnapshot-flavoured outline: roles, exact names, states.
  const LEAF = new Set(["button", "link", "textbox", "searchbox", "checkbox", "radio", "switch", "combobox", "slider",
    "spinbutton", "heading", "img", "option", "tab", "menuitem", "menuitemcheckbox", "menuitemradio", "progressbar", "separator"]);
  const SKIP = new Set(["presentation", "none", "generic"]);
  function snapshot(limit = 24000) {
    const lines = [];
    let size = 0, truncated = false;
    const push = (depth, line) => {
      if (truncated) return;
      const s = "  ".repeat(depth) + "- " + line;
      size += s.length + 1;
      if (size > limit) { truncated = true; return; }
      lines.push(s);
    };
    const attrs = el => {
      const a = [];
      const r = role(el);
      if (r === "heading") a.push(`level=${/^H[1-6]$/.test(el.tagName) ? el.tagName[1] : el.getAttribute("aria-level") || 2}`);
      if (el.checked || el.getAttribute("aria-checked") === "true") a.push("checked");
      if (el.getAttribute("aria-expanded")) a.push(`expanded=${el.getAttribute("aria-expanded")}`);
      if (el.selected || el.getAttribute("aria-selected") === "true") a.push("selected");
      if (el.getAttribute("aria-pressed") === "true") a.push("pressed");
      if (disabled(el)) a.push("disabled");
      return a.map(x => ` [${x}]`).join("");
    };
    const valueOf = el => {
      if (el.tagName === "INPUT" && el.type === "password") return el.value ? ': "••••"' : "";
      if (["INPUT", "TEXTAREA"].includes(el.tagName) && el.value) return `: ${JSON.stringify(el.value.slice(0, 120))}`;
      if (el.tagName === "SELECT") return `: ${JSON.stringify(norm(el.selectedOptions[0] && el.selectedOptions[0].textContent))}`;
      return "";
    };
    function visit(node, depth) {
      const kids = node.shadowRoot ? [...node.shadowRoot.childNodes] : [...node.childNodes];
      if (node.tagName === "SLOT") kids.push(...node.assignedNodes());
      for (const k of kids) {
        if (truncated) return;
        if (k.nodeType === 3) {
          const t = norm(k.nodeValue);
          if (t) push(depth, `text: ${JSON.stringify(t.slice(0, 160))}`);
          continue;
        }
        if (k.nodeType !== 1 || ["SCRIPT", "STYLE", "NOSCRIPT", "TEMPLATE", "HEAD"].includes(k.tagName)) continue;
        if (k.getAttribute("aria-hidden") === "true" || !visible(k)) continue;
        const r = role(k);
        if (!r || SKIP.has(r)) { visit(k, depth); continue; }
        const n = name(k);
        const head = `${r}${n ? " " + JSON.stringify(n.slice(0, 160)) : ""}${attrs(k)}${valueOf(k)}`;
        if (LEAF.has(r)) { push(depth, head); continue; }
        push(depth, head + ":");
        visit(k, depth + 1);
      }
    }
    visit(document.body || document.documentElement, 0);
    return { tree: lines.join("\n"), truncated };
  }
  const geometry = () => ({ sx: screenX, sy: screenY, ow: outerWidth, oh: outerHeight, iw: innerWidth, ih: innerHeight, dpr: devicePixelRatio, url: location.href, title: document.title });
  function cursor(x, y) {
    let c = document.getElementById("__puppet_cursor");
    if (!c) {
      c = document.createElement("div");
      c.id = "__puppet_cursor";
      c.style.cssText = "position:fixed;z-index:2147483647;width:14px;height:14px;margin:-7px 0 0 -7px;border-radius:50%;background:rgba(255,40,120,.75);box-shadow:0 0 0 2px #fff;pointer-events:none;transition:left .12s,top .12s";
      document.documentElement.appendChild(c);
    }
    c.style.left = x + "px"; c.style.top = y + "px";
    return true;
  }
  return { snapshot, actionable, fill, satisfied, stateOf, geometry, cursor };
}

// Build one injectable script: define the library, call `call`, wrap the
// answer (or the thrown error) as JSON.
export function pageCall(call) {
  return `(function(){try{var P=(${pageLibrary.toString()})();return JSON.stringify({ok:(${call})});}catch(e){return JSON.stringify({error:String(e&&e.message||e)});}})()`;
}

// Arbitrary user JS, puppet_eval-style: the expression's value comes back as
// JSON; a returned promise is parked on window and polled by the caller.
export function evalCall(src) {
  return `(function(){var ser=function(v){if(v===undefined)return JSON.stringify({undef:true});if(typeof Node!=="undefined"&&v instanceof Node)return JSON.stringify({value:(v.outerHTML||v.textContent||"").slice(0,4000)});try{return JSON.stringify({value:v});}catch(e){return JSON.stringify({value:String(v)});}};
try{var v=(
${src}
);if(v&&typeof v.then==="function"){var k="p"+Date.now().toString(36)+Math.random().toString(36).slice(2);window.__puppetPending=window.__puppetPending||{};window.__puppetPending[k]={pending:true};v.then(function(r){window.__puppetPending[k]={done:ser(r)};},function(e){window.__puppetPending[k]={done:JSON.stringify({error:String(e&&e.message||e)})};});return JSON.stringify({pending:k});}return ser(v);}catch(e){return JSON.stringify({error:String(e&&e.message||e)});}})()`;
}

export function pendingCall(key) {
  const k = JSON.stringify(key);
  return `(function(){var s=window.__puppetPending&&window.__puppetPending[${k}];if(!s)return JSON.stringify({error:"pending result lost (page navigated?)"});if(s.pending)return JSON.stringify({pending:${k}});delete window.__puppetPending[${k}];return s.done;})()`;
}
