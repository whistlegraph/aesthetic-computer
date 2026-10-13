// A shared DOM profile: <ac-profile handle="@name"> or openProfile("@name").
const AC = "https://aesthetic.computer";
const labels = { painting: "Paintings", piece: "Pieces", kidlisp: "KidLisp programs", tape: "Tapes", whistlegraph: "Whistlegraphs", mood: "Moods" };
const cache = new Map();
const pending = new Map();
const normalize = (value) => {
  const handle = String(value || "").replace(/^@/, "").toLowerCase();
  return /^[a-z0-9][a-z0-9_.-]{0,63}$/.test(handle) ? "@" + handle : null;
};
const element = (tag, className, text) => {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text != null) node.textContent = text;
  return node;
};

async function load(handle, membership = false) {
  const query = new URLSearchParams({ handle });
  if (membership) query.set("membership", "1");
  const key = query.toString(), cached = cache.get(key);
  if (cached && Date.now() - cached.at < 15000) return cached.data;
  if (pending.has(key)) return pending.get(key);
  const request = (async () => {
    const response = await fetch(new URL("/api/profile-card?" + key, import.meta.url), { signal: AbortSignal.timeout(12000), credentials: "omit" });
    if (!response.ok) throw new Error(response.status === 404 ? "Profile not found." : "Couldn’t load this profile.");
    const data = await response.json();
    cache.delete(key);
    cache.set(key, { at: Date.now(), data });
    if (cache.size > 40) cache.delete(cache.keys().next().value);
    return data;
  })();
  pending.set(key, request);
  try { return await request; } finally { pending.delete(key); }
}

export class ACProfile extends HTMLElement {
  static observedAttributes = ["handle"];
  constructor() { super(); this.attachShadow({ mode: "open" }); this.version = 0; }
  connectedCallback() { this.render(); }
  disconnectedCallback() { this.version++; this.closePresence(); }
  attributeChangedCallback() { if (this.isConnected) this.render(); }
  closePresence() {
    clearTimeout(this.presenceTimer);
    const socket = this.socket; this.socket = null;
    socket?.close();
  }
  render() {
    this.version++;
    this.closePresence();
    this.handle = normalize(this.getAttribute("handle"));
    const stylesheet = element("link");
    stylesheet.rel = "stylesheet";
    stylesheet.href = new URL("./profile-card.css", import.meta.url).href;
    const identity = element("div", "identity"), header = element("header");
    this.heading = element("h1", "", this.handle || "Profile");
    this.mood = element("dd", "mood");
    const moodField = element("dl", "mood-field");
    moodField.append(element("dt", "", "Mood"), this.mood);
    this.wallpaper = element("div", "wallpaper");
    this.wallpaper.setAttribute("aria-hidden", "true");
    this.profileLink = element("a", "handle-link");
    this.profileLink.href = AC + "/" + this.handle;
    this.profileLink.append(this.heading);
    header.append(element("p", "field-label", "Handle"), this.profileLink, moodField);
    identity.append(header);
    this.status = element("p", "status", "Loading…");
    this.status.setAttribute("role", "status");
    this.stats = element("dl", "stats");
    this.stats.setAttribute("aria-label", "Profile statistics");
    this.retry = element("button", "", "Try again");
    this.retry.hidden = true;
    this.retry.onclick = () => this.refresh();
    this.shadowRoot.replaceChildren(stylesheet, this.wallpaper, identity, this.status, this.stats, this.retry);
    if (this.handle) this.refresh();
    else this.status.textContent = "No profile selected.";
  }
  addStat(label, value) {
    const row = element("div", "stat"), output = element("dd", "", value);
    row.append(element("dt", "", label), output); this.stats.append(row);
    return output;
  }
  async refresh() {
    const version = ++this.version;
    this.retry.hidden = true;
    this.status.textContent = "Loading…";
    this.stats.setAttribute("aria-busy", "true");
    try {
      const data = await load(this.handle);
      if (version !== this.version || !this.isConnected) return;
      this.heading.textContent = data.handle;
      if (Array.isArray(data.colors)) {
        this.heading.replaceChildren(...[...data.handle].map((char, i) => {
          const span = element("span", "", char), rgb = data.colors[i];
          if (rgb && [rgb.r, rgb.g, rgb.b].every(Number.isFinite)) {
            const channels = [rgb.r, rgb.g, rgb.b].map((v) => Math.max(0, Math.min(255, v)));
            const brightness = channels[0] * .2126 + channels[1] * .7152 + channels[2] * .0722;
            span.style.color = `rgb(${channels.join(",")})`;
            if (brightness > 180) span.style.webkitTextStroke = ".6px #385348";
          }
          return span;
        }));
      }
      this.mood.textContent = data.mood?.text || "Not set";
      this.wallpaper.style.backgroundImage = data.latestPainting?.file?.url
        ? `url(${JSON.stringify(new URL(data.latestPainting.file.url, AC).href)})` : "none";
      this.stats.replaceChildren();

      for (const [kind, label] of Object.entries(labels)) {
        this.addStat(label, Number.isSafeInteger(data.counts?.[kind]) ? data.counts[kind].toLocaleString() : "Unavailable");
      }
      const registered = this.addStat("Registered", "Loading…");
      const logins = this.addStat("Logins", "Loading…");
      registered.parentElement.classList.add("membership-row");
      logins.parentElement.classList.add("membership-row");
      // Provider latency must never hold up the card or media counts.
      this.loadMembership(version, registered, logins);
      this.presence = this.addStat("Status", "Checking…");
      this.presence.parentElement.classList.add("presence-row");
      const time = this.addStat("Time online", data.onlineTimeMs == null ? "Not recorded" : `${Math.floor(data.onlineTimeMs / 60000).toLocaleString()} min`);
      time.parentElement.classList.add("time-row");
      this.status.textContent = "";
      this.watchPresence(version);
    } catch (error) {
      if (version !== this.version || !this.isConnected) return;
      this.status.textContent = error.name === "TimeoutError" ? "This is taking a while. Try again." : error.message;
      this.retry.hidden = false;
    } finally {
      if (version === this.version) this.stats.setAttribute("aria-busy", "false");
    }
  }
  async loadMembership(version, registered, logins) {
    try {
      const data = await load(this.handle, true);
      if (version !== this.version || !this.isConnected) return;
      const date = data.registeredAt && new Date(data.registeredAt);
      registered.textContent = date && Number.isFinite(+date)
        ? new Intl.DateTimeFormat("en", { month: "short", day: "numeric", year: "numeric", timeZone: "UTC" }).format(date)
        : "Unavailable";
      logins.textContent = Number.isSafeInteger(data.loginCount) ? data.loginCount.toLocaleString() : "Unavailable";
    } catch {
      if (version !== this.version || !this.isConnected) return;
      registered.textContent = logins.textContent = "Unavailable";
    }
  }
  watchPresence(version) {
    this.closePresence();
    const unavailable = () => { if (this.isConnected && this.version === version) {
      this.presence.textContent = "Unavailable"; delete this.presence.dataset.online;
    } };
    try {
      const socket = new WebSocket("wss://session-server.aesthetic.computer/profile-stream?handle=" + encodeURIComponent(this.handle));
      this.socket = socket;
      this.presenceTimer = setTimeout(unavailable, 5000);
      socket.onmessage = (event) => {
        if (this.socket !== socket || version !== this.version) return;
        try {
          const message = JSON.parse(event.data);
          if (!["profile:snapshot", "presence:update"].includes(message.type) || typeof message.data?.presence?.online !== "boolean") return;
          clearTimeout(this.presenceTimer);
          this.presence.textContent = message.data.presence.online ? "Online" : "Offline";
          this.presence.dataset.online = String(message.data.presence.online);
        } catch { /* Ignore unrelated stream events. */ }
      };
      socket.onclose = socket.onerror = () => { if (this.socket === socket) unavailable(); };
    } catch { unavailable(); }
  }
}
if (!customElements.get("ac-profile")) customElements.define("ac-profile", ACProfile);

let activeDialog;
export function openProfile(value, { opener = document.activeElement } = {}) {
  const handle = normalize(value);
  if (!handle) return;
  if (activeDialog?.open) {
    activeDialog.setAttribute("aria-label", `${handle} profile`);
    activeDialog.querySelector("ac-profile").setAttribute("handle", handle);
    return activeDialog;
  }
  const dialog = element("dialog");
  dialog.setAttribute("aria-label", `${handle} profile`);
  dialog.dataset.acProfile = "";
  const style = element("style");
  style.textContent = `dialog[data-ac-profile]{box-sizing:border-box;width:min(440px,calc(100% - 32px));max-height:calc(100dvh - 24px);padding:0;margin:auto;border:0;background:transparent;overflow:visible;perspective:1100px}dialog[data-ac-profile]::backdrop{background:#080d14aa;backdrop-filter:blur(3px)}dialog[data-ac-profile] ac-profile{max-height:calc(100dvh - 40px);overflow:auto;overscroll-behavior:contain;transform:rotateX(var(--card-rx,2deg)) rotateY(var(--card-ry,-3deg));animation:ac-card-rise 330ms cubic-bezier(.2,.9,.3,1.15);will-change:transform;transition:transform 150ms ease-out}dialog[data-ac-profile] .profile-dismiss{position:absolute;width:1px;height:1px;overflow:hidden;clip-path:inset(50%)}dialog[data-ac-profile] .profile-dismiss:focus-visible{clip-path:none;bottom:8px;right:8px;width:auto;height:auto;z-index:2;padding:6px 10px;border:1px solid #333;border-radius:5px;background:#fff;color:#111}@keyframes ac-card-rise{from{opacity:0;transform:translateY(55px) rotateX(24deg) rotateY(-12deg) scale(.88)}to{opacity:1;transform:rotateX(2deg) rotateY(-3deg)}}@media(prefers-reduced-motion:reduce){dialog[data-ac-profile] ac-profile{animation:none;transition:none;transform:none}}@media(max-height:300px){dialog[data-ac-profile]{overflow:auto}}`;
  const close = element("button", "profile-dismiss", "Put card away");
  close.type = "button";
  close.onclick = () => dialog.close();
  const profile = element("ac-profile");
  profile.setAttribute("handle", handle);
  dialog.append(style, close, profile);
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  dialog.addEventListener("pointermove", (event) => {
    if (motion.matches || event.pointerType !== "mouse") return;
    const rect = dialog.getBoundingClientRect();
    profile.style.setProperty("--card-rx", `${(0.5 - (event.clientY - rect.top) / rect.height) * 7}deg`);
    profile.style.setProperty("--card-ry", `${((event.clientX - rect.left) / rect.width - 0.5) * 9}deg`);
  });
  dialog.addEventListener("pointerleave", () => {
    profile.style.removeProperty("--card-rx"); profile.style.removeProperty("--card-ry");
  });
  const overflow = document.documentElement.style.overflow;
  const blockKeys = (event) => {
    if (!dialog.open) return;
    if (event.key === "Escape") { event.preventDefault(); dialog.close(); }
    if (["ArrowDown", "ArrowUp", "PageDown", "PageUp", "Home", "End", " "].includes(event.key)) {
      event.preventDefault();
      if (event.type === "keydown") {
        const amount = ["ArrowUp", "PageUp", "Home"].includes(event.key) ? -1 : 1;
        profile.scrollBy({ top: amount * (event.key.startsWith("Arrow") ? 40 : profile.clientHeight), behavior: "instant" });
      }
    }
    if (event.type === "keydown" && event.key === "Tab") {
      const controls = [close, ...profile.shadowRoot.querySelectorAll("a, button")]
        .filter((node) => !node.disabled && node.getClientRects().length);
      const focused = profile.shadowRoot.activeElement || document.activeElement;
      if (event.shiftKey && focused === controls[0]) {
        event.preventDefault(); controls.at(-1).focus();
      } else if (!event.shiftKey && focused === controls.at(-1)) {
        event.preventDefault(); controls[0].focus();
      }
    }
    // Keep feed and canvas shortcuts from acting behind the modal.
    event.stopImmediatePropagation();
  };
  window.addEventListener("keydown", blockKeys, true);
  window.addEventListener("keyup", blockKeys, true);
  let backdropDown = false;
  dialog.addEventListener("pointerdown", (event) => { backdropDown = event.target === dialog; });
  dialog.addEventListener("click", (event) => {
    if (backdropDown && event.target === dialog) {
      const rect = dialog.getBoundingClientRect();
      if (event.clientX < rect.left || event.clientX > rect.right || event.clientY < rect.top || event.clientY > rect.bottom) dialog.close();
    }
  });
  dialog.addEventListener("close", () => {
    window.removeEventListener("keydown", blockKeys, true);
    window.removeEventListener("keyup", blockKeys, true);
    document.documentElement.style.overflow = overflow;
    dialog.remove();
    if (activeDialog === dialog) activeDialog = null;
    if (opener?.isConnected) opener.focus({ preventScroll: true });
    window.dispatchEvent(new CustomEvent("ac-profile-close"));
  }, { once: true });
  document.body.append(dialog);
  activeDialog = dialog;
  dialog.showModal();
  profile.shadowRoot.querySelector("a")?.focus({ preventScroll: true });
  document.documentElement.style.overflow = "hidden";
  window.dispatchEvent(new CustomEvent("ac-profile-open"));
  return dialog;
}

// Opt in by marking handle links; normal links and modified clicks still work.
export function installProfileLinks(root = document) {
  const handler = (event) => {
    const link = event.target.closest?.("a[data-ac-profile-handle]");
    if (!link || event.defaultPrevented || event.button || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    const handle = normalize(link.dataset.acProfileHandle);
    if (!handle) return;
    event.preventDefault();
    event.stopPropagation();
    openProfile(handle, { opener: link });
  };
  root.addEventListener("click", handler);
  return () => root.removeEventListener("click", handler);
}
