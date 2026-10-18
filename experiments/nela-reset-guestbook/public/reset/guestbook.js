// Read the book and add a signature.
const form = document.querySelector('#guestbook-form');
const list = document.querySelector('#guestbook-entries');
const count = document.querySelector('#guestbook-count');
const empty = document.querySelector('#guestbook-empty');
const status = document.querySelector('#guestbook-status');
const submit = form.querySelector('button[type="submit"]');
const retry = document.querySelector('#guestbook-retry');
const endpoint = new URL('api/guestbook', document.baseURI);
const dates = new Intl.DateTimeFormat('en-US', { month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: '2-digit', timeZoneName: 'short', timeZone: 'America/Los_Angeles' });
const ago = new Intl.RelativeTimeFormat('en-US', { numeric: 'always' });
let entries = [];
let pendingId = crypto.randomUUID();
let submitting = false;

// Four bytes pick each ghost. The signature ID keeps it the same.
// Each number is one row of a 12-pixel sprite. Guest writing stays text.
function haunt(image, seed) {
  let hash = 2166136261;
  for (const letter of seed) hash = Math.imul(hash ^ letter.charCodeAt(0), 16777619);
  const [body, face, tint, wiggle] = [0, 8, 16, 24].map(shift => (hash >>> shift) & 255);
  const rows = [
    [0, 0, 0x070, 0x0f0, 0x1f8, 0x3fc, 0x3fc, 0x3fc, 0x3fc, 0x3fc, 0x3fc, 0x36c, 0x124],
    [0, 0x010, 0x030, 0x1f8, 0x3fc, 0x7fe, 0x7fe, 0x7fe, 0x7fe, 0x7fe, 0x7fe, 0x666, 0x222],
    [0, 0x070, 0x0f0, 0x1f8, 0x1f8, 0x1f8, 0x1f8, 0x1f8, 0x0f0, 0x0f0, 0x1f8, 0x36c, 0x224],
    [0, 0x088, 0x1dc, 0x1fc, 0x3fe, 0x3fe, 0x3fe, 0x7fe, 0x7fe, 0x3fc, 0x3fc, 0x36c, 0x124],
  ][body % 4];
  let outline = '', sheet = '';
  const filled = (x, y) => (rows[y] || 0) & (1 << x);
  rows.forEach((row, y) => {
    for (let x = 0; x < 12; x++) if (filled(x, y)) {
      const pixel = `M${x} ${y}h1v1h-1z`;
      outline += pixel;
      if (filled(x - 1, y) && filled(x + 1, y) && filled(x, y - 1) && filled(x, y + 1)) sheet += pixel;
    }
  });
  const color = ['#f8e7ff', '#d5f4ca', '#ffe8aa', '#ffd3e5'][tint % 4];
  const eyes = face % 4 === 0 ? 'M5 5h2v3H5z' : 'M4 5h1v2H4zM7 5h1v2H7z';
  const ghost = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  ghost.setAttribute('viewBox', '0 0 12 14');
  ghost.setAttribute('shape-rendering', 'crispEdges');
  ghost.setAttribute('class', 'boo');
  ghost.setAttribute('aria-hidden', 'true');
  ghost.style.cssText = `--drift:${2.4 + wiggle / 100}s;--blink:${3 + face / 80}s;--phase:-${wiggle / 30}s`;
  ghost.innerHTML = `<g fill="${color}">
    <path class="boo-arms" d="M0 7h1v1h3v1H0zM9 8h2V6h1v3H9z"/>
    <path d="${outline}" fill="#6b4d80"/><path d="${sheet}"/>
    <g class="boo-blink" fill="#281537"><path d="${eyes}${face % 4 === 3 ? 'M6 3h1v1H6z' : ''}"/>${face % 4 === 0 ? '<path d="M5 5h1v1H5z" fill="white"/>' : ''}</g>
    <path d="${face % 2 ? 'M5 8h1v1h1V8h1v2H5z' : 'M5 8h1v1H5z'}" fill="#281537"/>
    <path d="M3 7h1v1H3zM8 7h1v1H8z" fill="#e995b2"/>
  </g>`;
  image.replaceWith(ghost);
}
document.querySelectorAll('.guestbook-heading .boo').forEach((image, i) => haunt(image, `boo-${i}`));

// The OO looks toward the pointer, one pixel at a time. No animation loop.
const eyes = document.querySelector('.boo-eyes');
const pupils = document.querySelector('#boo-pupils');
const still = matchMedia('(prefers-reduced-motion: reduce)');
const restEyes = () => pupils.removeAttribute('transform');
window.addEventListener('pointermove', event => {
  if (still.matches || event.pointerType === 'touch') return;
  const box = eyes.getBoundingClientRect();
  const step = distance => Math.round(Math.max(-1, Math.min(1, distance / 80)));
  pupils.setAttribute('transform', `translate(${step(event.clientX - box.x - box.width / 2)} ${step(event.clientY - box.y - box.height / 2)})`);
}, { passive: true });
document.documentElement.addEventListener('pointerleave', restEyes);
window.addEventListener('blur', restEyes);
still.addEventListener('change', restEyes);

// One clock for the whole book; keep the exact time on hover and for readers.
function updateTimes() {
  for (const time of list.querySelectorAll('time')) {
    const minutes = Math.max(0, Math.floor((Date.now() - Date.parse(time.dateTime)) / 60000));
    const [amount, unit] = minutes < 60 ? [minutes, 'minute'] : minutes < 1440 ? [Math.floor(minutes / 60), 'hour'] : [Math.floor(minutes / 1440), 'day'];
    time.textContent = minutes ? ago.format(-amount, unit) : 'just now';
    time.setAttribute('aria-label', `${time.textContent}, ${time.title}`);
  }
}

function render() {
  const coming = entries.filter(entry => entry.attending).length;
  count.textContent = `${entries.length} ${entries.length === 1 ? 'ghost' : 'ghosts'} · ${coming} coming`;
  empty.hidden = true;
  empty.textContent = '';
  list.replaceChildren(...entries.map(entry => {
    const item = document.querySelector('#guestbook-entry').content.firstElementChild.cloneNode(true);
    haunt(item.querySelector('.boo'), entry.id);
    // Always textContent: a guest's writing cannot become page markup.
    item.querySelector('h3').textContent = entry.name;
    const date = item.querySelector('time');
    date.dateTime = entry.createdAt;
    date.title = dates.format(new Date(entry.createdAt));
    item.querySelector('.guestbook-message').textContent = entry.message;
    item.querySelector('.guestbook-message').hidden = !entry.message;
    item.querySelector('.guestbook-rsvp').hidden = !entry.attending;
    return item;
  }));
  updateTimes();
}

async function request(options) {
  const response = await fetch(endpoint, { ...options, signal: AbortSignal.timeout(12000) });
  let data;
  try { data = await response.json(); } catch { throw new Error('The guestbook is unavailable. Please try again.'); }
  if (!response.ok) throw new Error(data.error || 'The guestbook is unavailable. Please try again.');
  return data;
}

async function load() {
  retry.hidden = true;
  try {
    const data = await request();
    entries = data.entries;
    render();
  } catch {
    count.textContent = '';
    empty.hidden = false;
    empty.textContent = 'Could not load the guestbook.';
    retry.hidden = false;
  }
}

form.addEventListener('input', () => { pendingId = crypto.randomUUID(); });
form.addEventListener('submit', async event => {
  event.preventDefault();
  if (submitting) return;
  const name = form.elements.name;
  name.setCustomValidity(name.value.trim() ? '' : 'Please leave your name.');
  if (!form.reportValidity()) return;
  submitting = true;
  const body = JSON.stringify({
    id: pendingId,
    name: name.value,
    message: form.elements.message.value,
    attending: form.elements.attending.checked,
    website: form.elements.website.value,
  });
  submit.disabled = true;
  form.querySelector('fieldset').disabled = true;
  submit.textContent = 'Saving…';
  status.textContent = '';
  try {
    const { entry } = await request({ method: 'POST', headers: { 'Content-Type': 'application/json' }, body });
    entries = [entry, ...entries.filter(item => item.id !== entry.id)];
    render();
    form.reset();
    pendingId = crypto.randomUUID();
    status.textContent = 'Signed.';
    status.dataset.state = 'saved';
    load();
  } catch (error) {
    status.textContent = error.name === 'TimeoutError' || error instanceof TypeError ? 'Could not save. Your draft is still here. Try again.' : error.message;
    status.dataset.state = 'error';
  } finally {
    submitting = false;
    submit.disabled = false;
    form.querySelector('fieldset').disabled = false;
    submit.textContent = 'Sign';
  }
});
form.elements.name.addEventListener('input', () => form.elements.name.setCustomValidity(''));
retry.addEventListener('click', load);
setInterval(updateTimes, 60000);
load();
