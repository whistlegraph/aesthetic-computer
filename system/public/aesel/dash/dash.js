// Aesel dash: what Aesel keeps for one signed-in AC account, and the means to
// delete it. Every account read derives ownership from the token on the server;
// this page never asks for data by handle except public published pieces.
const API = 'https://aesthetic.computer';
const callback = `${location.origin}/dash/`;
const $ = id => document.getElementById(id);
let auth, user, threadTitles = new Map();

function el(tag, props = {}, ...children) {
  const node = Object.assign(document.createElement(tag), props);
  node.append(...children.filter(c => c !== null && c !== undefined));
  return node;
}
const plural = (n, one, many = one + 's') => `${n.toLocaleString()} ${n === 1 ? one : many}`;
const day = iso => iso ? new Date(iso).toLocaleDateString(undefined, {year: 'numeric', month: 'short', day: 'numeric'}) : '';
const bytes = n => n < 1024 ? `${n} B` : n < 1048576 ? `${(n / 1024).toFixed(1)} KB` : `${(n / 1048576).toFixed(1)} MB`;

function gate(message, error = false) {
  $('gate').hidden = false; $('dash').hidden = true;
  $('gate-status').textContent = message;
  $('gate-status').className = error ? 'error' : '';
}

// One authenticated JSON call; the subject check guards against a sign-in
// change in another tab between token and reply.
async function account(path, {method = 'GET', body} = {}) {
  const token = await auth.getTokenSilently();
  if ((await auth.getUser())?.sub !== user.sub) throw Error('Your account changed. Reload the dash.');
  const response = await fetch(API + path, {
    method, cache: 'no-store',
    headers: {Authorization: 'Bearer ' + token, ...(body ? {'Content-Type': 'application/json'} : {})},
    body: body ? JSON.stringify(body) : undefined,
  });
  if (response.status === 401) throw Error('Please sign in again.');
  if (!response.ok) throw Error((await response.json().catch(() => ({}))).error || `Request failed (${response.status}).`);
  return response.json();
}

// --- Braincells ---
async function loadCells() {
  try {
    const c = await account('/api/easel-credits');
    const parts = [`${c.remaining.toLocaleString()} of ${c.limit.toLocaleString()} left today`, `${c.used.toLocaleString()} used`];
    if (c.purchased) parts.push(`${c.purchased.toLocaleString()} purchased`);
    $('ledger-cells').textContent = parts.join(' · ') + `. Resets ${new Date(c.resetsAt).toLocaleString()}.`;
    return c.handle;
  } catch (error) { $('ledger-cells').textContent = error.message; return null; }
}

// --- What is recorded + artifacts ---
async function loadSummary() {
  try {
    const s = await account('/api/easel-transcripts?scope=mine&view=summary');
    const c = s.counts;
    $('ledger-transcripts').textContent = c.threads
      ? `${plural(c.threads, 'conversation')}, ${plural(c.messages, 'message')} (${c.yours.toLocaleString()} from you), first ${day(s.first)}, latest ${day(s.last)}.`
      : 'None. Nothing from your sessions has been uploaded.';
    $('ledger-artifacts').textContent = c.artifacts
      ? `${plural(c.artifacts, 'artifact')} across ${plural(c.revisions, 'revision')}. Revisions are recorded by ID, medium and number; the artwork itself is not in the transcript.`
      : 'None recorded.';
    if (s.truncated) $('ledger-transcripts').append(' Counts cover your most recent records only.');
    renderArtifacts(s.artifacts);
  } catch (error) {
    $('ledger-transcripts').textContent = $('ledger-artifacts').textContent = error.message;
    $('artifacts-status').textContent = error.message;
  }
}

function renderArtifacts(list) {
  const table = $('artifacts');
  table.hidden = !list.length;
  $('artifacts-status').textContent = list.length ? '' : 'No artifacts recorded in your uploaded conversations.';
  table.tBodies[0].replaceChildren(...list.map(a => el('tr', {},
    el('td', {}, a.medium),
    el('td', {}, el('code', {textContent: a.id})),
    el('td', {}, `v${a.revision}`),
    el('td', {}, day(a.at)),
    el('td', {}, el('button', {className: 'link', textContent: threadTitles.get(a.sessionId) || 'Open', onclick: () => openThread({id: a.sessionId, title: threadTitles.get(a.sessionId)})})),
  )));
}

// --- Conversations ---
let cursor = null, threads = [];
async function loadThreads(reset = false) {
  $('more').disabled = true;
  $('threads-status').textContent = 'Loading conversations…';
  try {
    const query = new URLSearchParams({scope: 'mine', ...(!reset && cursor ? cursor : {})});
    const data = await account('/api/easel-transcripts?' + query);
    threads = reset ? data.threads : [...threads, ...data.threads];
    cursor = data.nextBefore;
    for (const t of threads) threadTitles.set(t.id, t.title || `${t.medium || 'Aesel'} · ${t.id.slice(0, 8)}`);
    renderThreads();
    $('threads-status').textContent = threads.length ? '' : 'No uploaded conversations.';
  } catch (error) { $('threads-status').textContent = error.message; }
  finally { $('more').disabled = false; $('more').hidden = !cursor; $('delete-all').hidden = !threads.length; }
}

function renderThreads() {
  $('threads').replaceChildren(...threads.map(t => el('li', {},
    el('button', {className: 'link', textContent: threadTitles.get(t.id), onclick: () => openThread({id: t.id, title: threadTitles.get(t.id)})}),
    el('small', {textContent: [t.medium, day(t.createdAt)].filter(Boolean).join(' · ')}),
  )));
}

let reading = 0;
async function openThread({id, title}) {
  const version = ++reading, reader = $('reader');
  const messages = el('ol'), status = el('p', {className: 'fine', textContent: 'Opening…'});
  const more = el('button', {className: 'web-button', textContent: 'Read more', hidden: true});
  const remove = el('button', {className: 'web-button danger', textContent: 'Delete this conversation'});
  const close = el('button', {className: 'web-button', textContent: 'Close', onclick: () => { reading++; reader.hidden = true; reader.replaceChildren(); }});
  reader.replaceChildren(el('h3', {textContent: title || 'Conversation'}), el('p', {className: 'row'}, close, ' ', remove), messages, status, more);
  reader.hidden = false; reader.scrollIntoView({behavior: 'smooth', block: 'nearest'});
  let after = 0;
  async function page() {
    more.disabled = true;
    try {
      const data = await account('/api/easel-transcripts?' + new URLSearchParams({scope: 'mine', sessionId: id, afterSeq: String(after)}));
      if (version !== reading) return;
      for (const r of data.records) {
        if (r.type === 'message') messages.append(el('li', {}, el('strong', {textContent: r.role === 'user' ? 'You' : 'Aesel'}), r.text));
        if (r.type === 'artifact') messages.append(el('li', {className: 'revision', textContent: `${r.medium} ${r.artifactId} · revision ${r.revision}`}));
      }
      after = data.nextAfterSeq; more.hidden = after === null;
      status.textContent = messages.childElementCount ? '' : 'No messages recorded in this conversation.';
    } catch (error) { if (version === reading) { status.textContent = error.message; more.hidden = false; } }
    finally { more.disabled = false; }
  }
  more.onclick = page;
  remove.onclick = async () => {
    if (!confirm(`Delete “${title || 'this conversation'}” from Aesel's servers? This cannot be undone. Published work stays published.`)) return;
    remove.disabled = true;
    try {
      await account('/api/easel-transcripts', {method: 'DELETE', body: {sessionId: id}});
      reading++; reader.hidden = true; reader.replaceChildren();
      await Promise.all([loadThreads(true), loadSummary()]);
    } catch (error) { status.textContent = error.message; remove.disabled = false; }
  };
  await page();
}

$('delete-all').onclick = async () => {
  if (!confirm('Delete every uploaded conversation for your account from Aesel\'s servers? This cannot be undone. Published work stays published, and terminal sessions you still share will upload new messages.')) return;
  $('delete-all').disabled = true;
  try {
    const {deleted} = await account('/api/easel-transcripts', {method: 'DELETE', body: {all: true}});
    $('threads-status').textContent = `Deleted ${plural(deleted, 'record')}.`;
    reading++; $('reader').hidden = true; $('reader').replaceChildren();
    await Promise.all([loadThreads(true), loadSummary()]);
  } catch (error) { $('threads-status').textContent = error.message; }
  finally { $('delete-all').disabled = false; }
};
$('more').onclick = () => loadThreads();

// --- This browser ---
// The /try/ desk keeps drafts in localStorage under the Auth0 subject.
function browserKeys() {
  const prefix = `aesel.web.v1:${encodeURIComponent(user.sub)}:`;
  return Object.keys(localStorage).filter(k => k.startsWith(prefix));
}
function renderBrowser() {
  const keys = browserKeys(), cell = $('ledger-browser');
  if (!keys.length) { cell.textContent = 'No Aesel drafts saved here. Sign-in tokens are kept by Auth0 until you sign out.'; return; }
  const size = keys.reduce((n, k) => n + k.length + (localStorage.getItem(k) || '').length, 0) * 2;
  const clear = el('button', {className: 'link danger', textContent: 'Clear', onclick: () => {
    if (!confirm('Remove Aesel drafts saved in this browser? This cannot be undone.')) return;
    for (const k of browserKeys()) localStorage.removeItem(k);
    renderBrowser();
  }});
  cell.replaceChildren(`${plural(keys.length, 'saved item')} of drafts (${bytes(size)}), on this device only. `, clear);
}

// --- Published pieces (public) ---
async function loadPieces(handle) {
  if (!handle) { $('pieces-status').textContent = 'Claim an @handle at aesthetic.computer/handle to publish pieces.'; return; }
  try {
    const response = await fetch(`${API}/media-collection?for=${encodeURIComponent(handle)}/piece`);
    const {files = []} = await response.json();
    $('pieces-status').textContent = files.length ? `${plural(files.length, 'piece')} published to ${handle}. These are public.` : `Nothing published to ${handle} yet.`;
    $('pieces').replaceChildren(...files.slice().reverse().map(url => {
      const slug = url.split('/').pop().replace(/\.(mjs|lisp)$/, '');
      return el('li', {}, el('a', {href: `${API}/${handle}/${slug}`, target: '_blank', rel: 'noopener', textContent: `${handle}/${slug}`}));
    }));
  } catch { $('pieces-status').textContent = 'Published pieces are unavailable right now.'; }
}

async function enter() {
  user = await auth.getUser();
  if (!user?.sub) throw Error('Sign in again to open your dash.');
  $('gate').hidden = true; $('dash').hidden = false; $('logout').hidden = false;
  renderBrowser();
  const cells = loadCells();
  await loadThreads(true); // Artifacts name their conversation, so titles come first.
  const [handle] = await Promise.all([cells, loadSummary()]);
  if (handle) { $('handle').textContent = handle; $('handle').href = `${API}/${handle}`; $('handle').hidden = false; }
  void loadPieces(handle);
}

async function boot() {
  if (!window.auth0 || !crypto.subtle) throw Error('This browser could not load secure sign-in. Try a current Firefox, Safari or Chrome.');
  auth = new window.auth0.Auth0Client({
    domain: 'hi.aesthetic.computer', clientId: 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt',
    cacheLocation: 'localstorage', useRefreshTokens: true, useRefreshTokensFallback: true,
    httpTimeoutInSeconds: 30, authorizationParams: {redirect_uri: callback},
  });
  const query = new URLSearchParams(location.search);
  if (query.has('state') && (query.has('code') || query.has('error'))) {
    try { await auth.handleRedirectCallback(); } finally { history.replaceState({}, '', callback); }
  } else await auth.checkSession();
  $('login').disabled = false;
  if (await auth.isAuthenticated()) await enter(); else gate('');
}

$('login').onclick = () => {
  $('login').disabled = true;
  auth.loginWithRedirect({authorizationParams: {redirect_uri: callback}}).catch(error => { gate(error.message, true); $('login').disabled = false; });
};
$('logout').onclick = () => { gate('Signing out…'); auth.logout({logoutParams: {returnTo: callback}}); };

// Module scripts run after deferred classic scripts, so auth0 is loaded here.
boot().catch(error => { gate(error.message, true); $('login').disabled = !auth; });
