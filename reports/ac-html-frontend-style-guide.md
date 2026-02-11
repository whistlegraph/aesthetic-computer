# AC HTML Frontend Style Guide

How we build dashboard and utility pages on aesthetic.computer.

---

## Philosophy

No frameworks. No build step. No npm for the frontend. One HTML file, readable top to bottom. If you can't understand the whole page in one sitting, it's too complex.

AC services are named and self-contained: **oven** bakes, **judge** moderates, **silo** stores. Each gets its own subdomain and its own single `index.html`. The frontend is just the face of the service -- the server does the real work.

---

## File Structure

One file per dashboard:

```
service-name/
├── server.mjs          (or api-server.mjs)
├── index.html          (the entire frontend)
├── package.json
├── .env
└── deploy.fish
```

Everything lives in `index.html` -- styles, markup, script. No separate `.css` or `.js` files unless the page genuinely exceeds ~1000 lines of CSS (kidlisp.com is the one exception, using modular CSS imports).

---

## HTML Skeleton

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>service-name · Aesthetic Computer</title>
    <link rel="icon" href="https://aesthetic.computer/icon/128x128/prompt.png"
          type="image/png" />
    <style>
        /* all styles here */
    </style>
</head>
<body>
    <!-- all markup here -->
    <script>
        // all logic here
    </script>
</body>
</html>
```

No `<link rel="stylesheet">` to external CSS (except fonts). No `<script src="">` to external JS (except auth0 or a specific library when truly needed). Keep dependencies minimal and loaded from CDN when unavoidable.

---

## CSS Conventions

### Reset

```css
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

::-webkit-scrollbar {
    display: none;
}
```

### Fonts

Monospace is the default. Use AC's own typefaces when available:

```css
body {
    font-family: 'Berkeley Mono Variable', 'Menlo', monospace;
    font-size: 14px;
    line-height: 1.5;
    -webkit-text-size-adjust: none;
}
```

Available AC fonts (loaded from `aesthetic.computer/type/webfonts/`):
- **Berkeley Mono Variable** -- primary monospace
- **YWFT Processing** -- display/heading
- **Comic Relief** -- playful contexts

### Color & Theming

Use CSS custom properties. Define both modes on `:root` and `body.light-mode`:

```css
/* Dark (default) */
:root {
    --bg: #1a1a2e;
    --text: #e8e8e8;
    --dim: #888;
    --pink: #ff6b9d;
    --cyan: #4ecdc4;
    --green: #6bcb77;
    --gold: #ffd93d;
    --box-bg: rgba(255,255,255,0.03);
    --box-border: rgba(255,255,255,0.1);
}

/* Light */
body.light-mode {
    --bg: #f5f5f5;
    --text: #1a1a2e;
    --dim: #666;
    --pink: rgb(205, 92, 155);
    --cyan: #0891b2;
    --green: #059669;
    --box-bg: rgba(0,0,0,0.03);
    --box-border: rgba(0,0,0,0.12);
}
```

The pink (`--pink`) is the AC accent color. Use it for:
- Interactive element hover states
- Active/selected states
- Stat values and highlights
- The `h1` service name

For simpler dashboards (like judge), a light default with just `#f5f5f5` background is fine. Match the service's personality.

### Status Colors

Consistent across all dashboards:

```css
/* Status indicators */
.connected  { color: #4ade80; }  /* green */
.blocked    { color: #ef4444; }  /* red */
.warning    { color: #fbbf24; }  /* amber */
.info       { color: var(--pink); }
```

### Layout

CSS Grid for stat boxes and card grids. Flexbox for toolbars and inline layouts. No layout framework.

```css
.stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
    gap: 0.8em;
}
```

Responsive: single column on mobile via `@media (max-width: 600px)`.

### Animations

Keep them subtle. The standard AC set:

```css
@keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.6; }
}

@keyframes slideIn {
    from { opacity: 0; transform: translateX(-10px); }
    to { opacity: 1; transform: translateX(0); }
}

@keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
}
```

Use `pulse` for live status dots. `slideIn` for new list items. `fadeIn` for panels appearing.

### Cursor

AC uses custom cursors:

```css
body {
    cursor: url('https://aesthetic.computer/aesthetic.computer/cursors/precise.svg')
            12 12, auto;
}
```

---

## JavaScript Conventions

### No Framework

Vanilla JS only. DOM manipulation via:

```javascript
document.getElementById('thing')
document.querySelector('.thing')
document.querySelectorAll('.things')
element.addEventListener('click', handler)
```

### State

Top-level `let` variables. No state management library. Keep it simple:

```javascript
let stats = { total: 0, allowed: 0, blocked: 0 };
let ws = null;
let reconnectTimer = null;
```

### DOM Updates

Template literals + `innerHTML` for rendering dynamic content. Always escape user-provided text:

```javascript
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

element.innerHTML = `
    <div class="item">
        <span>${escapeHtml(userText)}</span>
        <span class="meta">${new Date().toLocaleTimeString()}</span>
    </div>
`;
```

For lists that grow, use `insertBefore` + trim old entries:

```javascript
container.insertBefore(newItem, container.firstChild);
while (container.children.length > MAX_ITEMS) {
    container.removeChild(container.lastChild);
}
```

### Data Fetching

`fetch` for REST. WebSocket for real-time. No axios, no libraries.

```javascript
// REST
const data = await fetch('/api/endpoint').then(r => r.json());

// Parallel
const [users, stats] = await Promise.all([
    fetch('/api/users').then(r => r.json()),
    fetch('/api/stats').then(r => r.json()),
]);
```

### WebSocket Pattern

Standard pattern used across judge, oven, and session-server dashboards:

```javascript
const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
const wsUrl = `${protocol}//${location.host}/ws`;
let ws = null;

function connectWebSocket() {
    ws = new WebSocket(wsUrl);

    ws.onopen = () => updateStatus(true);

    ws.onclose = () => {
        updateStatus(false);
        setTimeout(connectWebSocket, 2000); // auto-reconnect
    };

    ws.onmessage = (event) => {
        const data = JSON.parse(event.data);
        handleMessage(data);
    };
}

connectWebSocket();
```

Always auto-reconnect on close. 2-second delay is the convention.

### Navigation

The `h1` service name is clickable and navigates back to aesthetic.computer:

```javascript
document.querySelector('h1').onclick = () => {
    if (location.host !== 'service.aesthetic.computer') {
        location.href = '/';
    } else {
        location.href = 'https://aesthetic.computer';
    }
};
```

---

## Markup Patterns

### Page Structure

```html
<body>
    <div class="container">
        <header>
            <h1>service-name</h1>
            <div class="status">
                <div class="status-dot" id="wsStatus"></div>
                <span id="wsStatusText">Connecting...</span>
            </div>
        </header>

        <div class="content-wrapper">
            <section>
                <h2>Section Title</h2>
                <!-- content -->
            </section>
        </div>
    </div>
</body>
```

### Stat Boxes

```html
<div class="stats">
    <div class="stat-box">
        <div class="stat-label">Label</div>
        <div class="stat-value" id="statId">0</div>
    </div>
</div>
```

### Status Dot

```html
<div class="status">
    <div class="status-dot connected"></div>
    <span>Connected</span>
</div>
```

### History / Feed

```html
<div class="history">
    <div class="history-item allowed">
        <div class="message-text">Content here</div>
        <div class="message-meta">metadata · timestamp</div>
    </div>
</div>
```

---

## Server-Side HTML Generation

For pages served by an Express/Fastify server (like oven), generate HTML via template literals:

```javascript
app.get('/', (req, res) => {
    res.send(`<!DOCTYPE html>
<html>
<head>
    <title>service · Aesthetic Computer</title>
    <style>${CSS_STRING}</style>
</head>
<body>
    <!-- markup -->
    <script>${JS_STRING}</script>
</body>
</html>`);
});
```

This keeps the same single-file pattern while allowing the server to inject dynamic values.

---

## Dark / Light Mode

### System-Preference Following

AC pages follow the OS theme by default. No manual toggle -- respect the user's system setting:

```javascript
function applySystemTheme() {
    if (window.matchMedia('(prefers-color-scheme: light)').matches) {
        document.body.classList.add('light-mode');
        document.body.classList.remove('dark-mode');
    } else {
        document.body.classList.remove('light-mode');
        document.body.classList.add('dark-mode');
    }
}

// Apply on load
applySystemTheme();

// Listen for system preference changes (live switching)
window.matchMedia('(prefers-color-scheme: light)')
    .addEventListener('change', applySystemTheme);
```

### CSS Structure

Dark is default (`:root`). Light overrides via `body.light-mode`. Override every variable:

```css
:root {
    --bg: #1a1a2e;
    --text: #e8e8e8;
    --dim: #888;
    --pink: #ff6b9d;
    --box-bg: rgba(255,255,255,0.03);
    --box-border: rgba(255,255,255,0.1);
    --shadow-soft: 0 2px 8px rgba(0,0,0,0.15);
}

body.light-mode {
    --bg: #f5f5f5;
    --text: #1a1a2e;
    --dim: #666;
    --pink: rgb(205, 92, 155);
    --box-bg: rgba(0,0,0,0.03);
    --box-border: rgba(0,0,0,0.12);
    --shadow-soft: 0 2px 8px rgba(0,0,0,0.08);
}
```

For simpler service dashboards (judge, censor) that don't need dark mode, a light-only `#f5f5f5` background is fine. Full theming is reserved for public-facing pages (give, kidlisp).

### Light-Mode Component Overrides

When a component needs more than variable swaps, use `body.light-mode .component`:

```css
body.light-mode .stat-item {
    background: rgba(0,0,0,0.03);
    border-color: rgba(0,0,0,0.08);
}

body.light-mode .gift-btn:hover {
    background: var(--pink);
    color: white;
}
```

---

## Internationalization (i18n)

AC supports 5 languages: **English** (default), **Dansk**, **Deutsch**, **Espanol**, **中文**.

### Two Approaches

**1. CSS-driven (give.aesthetic.computer)** -- for static/prose content:

Markup has multiple versions of each text block, tagged with `data-lang`:

```html
<p data-lang="en">Thank you for your support!</p>
<p data-lang="da">Tak for din stotte!</p>
<p data-lang="de">Danke fur deine Unterstutzung!</p>
<p data-lang="es">Gracias por tu apoyo!</p>
<p data-lang="zh">感谢您的支持!</p>
```

CSS hides/shows based on a body class:

```css
/* Default: show English, hide others */
[data-lang="da"]:not(.lang-option),
[data-lang="zh"]:not(.lang-option),
[data-lang="de"]:not(.lang-option),
[data-lang="es"]:not(.lang-option) { display: none !important; }

/* When Danish selected */
body.lang-da [data-lang="da"] { display: block !important; }
body.lang-da [data-lang="en"] { display: none !important; }
/* ... repeat for each language */
```

Switching is just adding/removing a body class:

```javascript
function setLanguage(lang) {
    currentLang = lang;
    document.body.classList.remove('lang-da', 'lang-zh', 'lang-de', 'lang-es');
    if (lang !== 'en') document.body.classList.add(`lang-${lang}`);
}
```

**2. JS-driven (kidlisp.com)** -- for dynamic/UI content:

A translation function `t()` with a translations object:

```javascript
const translations = {
    en: { consoleTitle: 'Console', referenceTab: 'Reference', guideTab: 'Guide' },
    da: { consoleTitle: 'Konsol', referenceTab: 'Reference', guideTab: 'Guide' },
    // ...
};

function t(key) {
    return translations[currentLang]?.[key] || translations.en[key] || key;
}

function setLanguage(lang) {
    currentLang = lang;
    localStorage.setItem('kidlisp-lang', lang);
    updateUILanguage();
}
```

Elements can also use `data-i18n` attributes for automatic updates:

```html
<span data-i18n="pressToRun">Press ▶ to run</span>
```

```javascript
document.querySelectorAll('[data-i18n]').forEach(el => {
    el.textContent = t(el.getAttribute('data-i18n'));
});
```

### Language Selector UI

Flag dropdown in the top bar using [flag-icons](https://github.com/lipis/flag-icons) CSS:

```html
<div class="lang-selector" id="langSelector">
    <span class="lang-flag fi fi-us" id="lang-flag"></span>
    <span class="lang-text">English</span>
    <span class="lang-arrow">▼</span>
    <div class="lang-dropdown">
        <div class="lang-option" data-lang="en" data-flag="us">
            <span class="fi fi-us"></span> English
        </div>
        <div class="lang-option" data-lang="da" data-flag="dk">
            <span class="fi fi-dk"></span> Dansk
        </div>
        <!-- ... -->
    </div>
</div>
```

### Persistence

Save preference to localStorage and restore on load:

```javascript
// Save
localStorage.setItem('gift-prefs', JSON.stringify({ lang }));

// Restore
const saved = JSON.parse(localStorage.getItem('gift-prefs') || '{}');
if (saved.lang) setLanguage(saved.lang);
```

### When to Add i18n

- **Public-facing pages** (give, kidlisp) -- yes, always
- **Internal dashboards** (silo, judge, oven) -- not needed, English only

---

## Design & Color Language

### The AC Palette

| Variable | Dark Mode | Light Mode | Usage |
|----------|-----------|------------|-------|
| `--bg` | `#1a1a2e` | `#f5f5f5` | Page background |
| `--text` | `#e8e8e8` | `#1a1a2e` | Primary text |
| `--dim` | `#888` | `#666` | Secondary/meta text |
| `--pink` | `#ff6b9d` | `rgb(205, 92, 155)` | AC accent, links, highlights |
| `--cyan` | `#4ecdc4` | `#0891b2` | Secondary accent |
| `--green` | `#6bcb77` | `#059669` | Success, connected, progress |
| `--gold` | `#ffd93d` | `#d97706` | Warnings, special highlights |
| `--box-bg` | `rgba(255,255,255,0.03)` | `rgba(0,0,0,0.03)` | Card/box backgrounds |
| `--box-border` | `rgba(255,255,255,0.1)` | `rgba(0,0,0,0.12)` | Subtle borders |

### Pink is the Accent

The AC pink (`--pink`) appears everywhere as the brand accent:
- `h1` service name color + hover to `purple`
- Button hover backgrounds
- Stat value highlights
- Active/selected tab indicators
- Link colors
- Border accents on focused inputs

```css
h1 { color: rgb(205, 92, 155); }
h1:hover { color: purple; }
h1:active { color: gray; }

button:hover {
    background: rgb(205, 92, 155);
    color: white;
    border-color: rgb(205, 92, 155);
}

textarea:focus {
    border-color: rgb(205, 92, 155);
}
```

### Typography Scale

Monospace-first. Sizes are deliberately restrained:

| Element | Size | Weight |
|---------|------|--------|
| `h1` (service name) | `22px` | normal |
| `h2` (section title) | `1.2em` | normal |
| Body text | `14px` | normal |
| `.subtitle`, meta | `0.85em` | normal |
| Stat values | `1.8em` | normal |
| Stat labels | `0.85em` | normal |

Note: `font-weight: normal` everywhere. AC doesn't use bold for hierarchy -- size and color do the work.

### Button Style

Minimal, monospace, border-based:

```css
button {
    padding: 0.7em 1.2em;
    font-family: monospace;
    font-size: 14px;
    border: 1px solid #000;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
}

button:hover {
    background: rgb(205, 92, 155);
    color: white;
    border-color: rgb(205, 92, 155);
}

button:active {
    transform: translateY(1px);
}
```

### Cards / Boxes

Subtle backgrounds, thin borders, no heavy shadows:

```css
.stat-box {
    padding: 1em;
    background: white;          /* or var(--box-bg) */
    border: 1px solid #ddd;     /* or var(--box-border) */
}

/* History items use left-border accent */
.history-item {
    padding: 0.8em;
    background: white;
    border-left: 3px solid #ddd;
}
.history-item.allowed { border-color: #4ade80; }
.history-item.blocked { border-color: #ef4444; }
```

### Spacing

Use `em` units for component spacing, keep it compact:
- Section margins: `2em 0`
- Component padding: `1em`
- Gaps: `0.5em` to `0.8em`
- No large whitespace -- dashboards should feel dense and informative

### User Selection

Disabled by default, re-enabled on text content:

```css
body {
    user-select: none;
    -webkit-user-select: none;
}

/* Re-enable on copyable content */
a[href], input, textarea, .prose, .description {
    user-select: text;
    -webkit-user-select: text;
}
```

---

## Live Reload & Development

### Server-Side: nodemon

All AC services use `nodemon` for auto-restart during development:

```json
{
    "dev": "nodemon -I --watch server.mjs --watch baker.mjs server.mjs"
}
```

The `-I` flag (ignore) prevents restart loops. Watch only the files that matter.

### Client-Side: Session Server + Chokidar

The session server watches the filesystem and broadcasts reload messages to all connected browsers via WebSocket:

```javascript
// session-server/session.mjs (dev mode only)
import chokidar from "chokidar";

if (dev) {
    // Watch pieces -- hot reload the specific piece
    chokidar.watch("../system/public/aesthetic.computer/disks")
        .on("all", (event, path) => {
            if (event === "change") {
                const piece = path.split("/").pop().replace(/\.mjs|\.lisp$/, "");
                everyone(pack("reload", { piece }, "local"));
            }
        });

    // Watch system files -- full page refresh
    chokidar.watch([
        "../system/public/aesthetic.computer/lib",
        "../system/public/aesthetic.computer/boot.mjs",
        "../system/public/aesthetic.computer/style.css",
        "../system/public/give.aesthetic.computer",
        "../system/public/kidlisp.com",
    ]).on("all", (event, path) => {
        if (event === "change")
            everyone(pack("reload", { piece: "*refresh*" }, "local"));
    });
}
```

Reload types:
- `{ piece: "name" }` -- hot reload a specific piece (preserves context)
- `{ piece: "*refresh*" }` -- full page refresh (system file changed)
- `{ piece: "*piece-reload*" }` -- reload current piece without full refresh

### KidLisp Live Coding: postMessage

kidlisp.com sends code to the AC iframe for live execution without page reload:

```javascript
// kidlisp.com → iframe
iframe.contentWindow.postMessage({
    type: "kidlisp-reload",
    code: sourceCode,
    codeId: "$nece",
    enableTrace: false
}, "*");

// For slide mode (preserve state/buffers):
iframe.contentWindow.postMessage({
    type: "kidlisp-slide",
    code: sourceCode
}, "*");
```

### Redis Pub/Sub for Backend Reload

User pieces hosted on backends can be reloaded via Redis:

```javascript
// Netlify function: reload.js
const client = createClient({ url: redisConnectionString });
await client.connect();
await client.publish("reload", JSON.stringify({ piece }));
```

### Dev Environment Contexts

Different dev configurations via `netlify.toml`:

```toml
[dev]
command = "caddy run --config Caddyfile"
port = 8888
targetPort = 8111

# Local dev: HTTPS with local SSL certs
[context.local.dev.https]
certFile = "../ssl-dev/localhost.pem"
keyFile = "../ssl-dev/localhost-key.pem"

# Codespaces: no HTTPS (proxy handles it)
[context.codespace.dev]
```

Dev scripts per context:
```json
{
    "dev": "netlify dev -o",
    "local-dev": "netlify dev --context local -o",
    "devcontainer-dev": "netlify dev --context codespace -o"
}
```

---

## What We Don't Use

- React, Vue, Svelte, or any component framework
- TypeScript on the frontend
- CSS preprocessors (Sass, Less, PostCSS)
- Bundlers for frontend code (webpack, vite, esbuild)
- CSS frameworks (Tailwind, Bootstrap)
- State management libraries
- Routing libraries (each page is one page)
- Package managers for frontend dependencies

If you need a library (auth0, qrcode-generator, split.js), load it from a CDN with a `<script>` tag.

---

## Naming

Services get short, evocative names. The name appears as the `h1` and the subdomain:

| Service | Name | Domain |
|---------|------|--------|
| Video processing | **oven** | oven.aesthetic.computer |
| Chat moderation | **judge** | judge.aesthetic.computer |
| Content filter | **censor** | censor.aesthetic.computer |
| Data & storage | **silo** | silo.aesthetic.computer |

The name should suggest what the service *does*, not what technology it uses.

---

## Reference Implementations

- **Minimal dashboard:** [judge/index.html](../judge/index.html) (~670 lines) -- the cleanest example
- **Rich dashboard:** [give.aesthetic.computer/index.html](../system/public/give.aesthetic.computer/index.html) -- full theming, canvas, auth
- **Server-generated:** [oven/server.mjs](../oven/server.mjs) -- HTML built in Express route handler
- **Modular CSS (exception):** [kidlisp.com/index.html](../system/public/kidlisp.com/index.html) -- when one file isn't enough
