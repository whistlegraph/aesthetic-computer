const table = document.getElementById('pubs');
const dialog = document.createElement('dialog');
dialog.className = 'record-preview';
dialog.setAttribute('aria-labelledby', 'record-heading');
dialog.innerHTML = `<header><h2 id="record-heading"></h2><button aria-label="Close record" autofocus>×</button></header><article aria-live="polite"></article><nav><a target="_blank" rel="noopener">Open in tab ↗</a></nav>`;
document.body.append(dialog);
let opener;
let request;
dialog.querySelector('button').onclick = () => dialog.close();
dialog.addEventListener('close', () => {
  request?.abort();
  document.documentElement.classList.remove('record-open');
  opener?.focus({ preventScroll: true });
});
dialog.addEventListener('click', event => {
  if (event.target !== dialog) return;
  const r = dialog.getBoundingClientRect();
  if (event.clientX < r.left || event.clientX > r.right || event.clientY < r.top || event.clientY > r.bottom) dialog.close();
});
table?.addEventListener('click', async event => {
  const link = event.target.closest('a[href*="/records/"]');
  if (!link || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  const url = new URL(link.href);
  if (url.origin !== location.origin) return;
  event.preventDefault();
  opener = link;
  dialog.querySelector('h2').textContent = link.textContent;
  dialog.querySelector('a').href = url.href;
  const article = dialog.querySelector('article');
  article.textContent = 'Loading…';
  article.scrollTop = 0;
  document.documentElement.classList.add('record-open');
  dialog.showModal();
  request?.abort();
  const current = request = new AbortController();
  try {
    const response = await fetch(url, { signal: current.signal });
    if (!response.ok) throw new Error('Unavailable');
    const page = new DOMParser().parseFromString(await response.text(), 'text/html');
    const content = page.querySelector('main');
    if (!content) throw new Error('Unavailable');
    content.querySelector('h1')?.remove();
    if (request === current && dialog.open) article.replaceChildren(...content.childNodes);
  } catch (error) {
    if (error.name !== 'AbortError' && request === current) article.textContent = 'Preview unavailable. Open the record in a tab to retry.';
  }
});
