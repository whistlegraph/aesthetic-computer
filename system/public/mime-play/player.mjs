// Same-origin host for pooled AC runtimes: boot silently, hold the first frame,
// and resume in place when MIME selects the card. No signed-in session is used.
const params = new URLSearchParams(location.search);
const code = params.get('code'), piece = params.get('piece');
const note = document.getElementById('note');
let frame, source, injected = false, drawn = false, finishing = false;
let playing = params.get('warm') !== '1';
function settle(play) {
  playing = play;
  const win = frame?.contentWindow;
  win?.AC?.setMasterVolume?.(drawn && playing ? 1 : 0);
  if (!drawn) return;
  if (playing) { win?.acRESUME?.(); win?.AC?.startAudio?.(); }
  else win?.acPAUSE?.();
}
window.__mimePlayerSettle = settle;
window.__mimeSettle ||= settle;
window.addEventListener('message', event => {
  if (event.source === parent && parent !== window) {
    if (event.data?.type === 'mime:playing') settle(event.data.playing === true);
    if (event.data?.type === 'ac:start-audio' && playing && drawn) frame?.contentWindow?.AC?.startAudio?.();
  }
  if (!frame || event.source !== frame.contentWindow || event.origin !== location.origin || event.data?.type !== 'ready') return;
  const win = frame.contentWindow;
  if (!win.preloaded) return; // Ignore AC's initial blank disk.
  if (source && !injected) {
    injected = true;
    win.preloaded = false;
    win.acSEND({ type: 'dropped:piece', content: { name: 'mime', source,
      search: 'noauth=true&noplot=true&nogap=true&nolabel=true&mime=1', isKidLisp: false } });
    return;
  }
  if (finishing || drawn) return;
  finishing = true;
  win.requestAnimationFrame(() => win.requestAnimationFrame(() => setTimeout(() => {
    drawn = true; note.remove(); settle(playing);
    window.__mimeReady = true;
    parent.postMessage({ type: 'mime:ready' }, '*');
  }, 250)));
});
async function run() {
  let address;
  if (code) {
    if (!/^wg[A-Za-z0-9]{1,32}$/.test(code)) throw new Error('no such piece');
    const response = await fetch('/api/mime-feed?code=' + encodeURIComponent(code) + (params.has('version') ? '&version=' + encodeURIComponent(params.get('version')) : ''));
    if (response.status === 409) throw new Error('New version available · refresh the feed');
    if (!response.ok) throw new Error('no such piece');
    ({ source } = await response.json());
    if (typeof source !== 'string' || !source) throw new Error('this piece has no code yet');
    address = 'wipe';
  } else {
    if (!/^\$?[A-Za-z0-9_-]{1,64}$/.test(piece || '')) throw new Error('no such piece');
    address = piece;
  }
  frame = document.createElement('iframe');
  frame.title = code ? 'Whistlegraph piece ' + code : 'AC piece ' + piece;
  frame.allow = "autoplay; camera 'none'; microphone 'none'";
  const query = new URLSearchParams({ noauth: 'true', noplot: 'true', nogap: 'true', nolabel: 'true', mime: '1' });
  if (source) query.set('preview', 'walkieware');
  if (params.has('ac-automation')) query.set('ac-automation', '1');
  frame.src = '/' + (address.startsWith('$') ? '$' + encodeURIComponent(address.slice(1)) : encodeURIComponent(address)) + '?' + query;
  document.body.append(frame);
}
run().catch(error => {
  note.textContent = error.message || 'could not play this piece';
  window.__mimeReady = true;
  parent.postMessage({ type: 'mime:ready', error: true }, '*');
});
