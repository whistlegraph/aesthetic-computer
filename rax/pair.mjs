// 📱 The phone's side of a pairing (pairing.mjs). A screen showed a QR; this
// page reads the code from the address's fragment, shows the number that
// screen shows, and on a tap (never before) sends the code with this phone's
// own AC token to the rax server, which signs that one screen in.
import { accountToken } from './account.mjs';
import { pairNumber, CODE } from './pair-number.mjs';

const $ = (id) => document.getElementById(id);
const say = (text) => ($('say').textContent = text);
const show = (...ids) => {
  for (const id of ['number', 'note', 'yes', 'in', 'no']) $(id).hidden = !ids.includes(id);
};

// The code leaves the address bar (and the history) at once; a reload of
// this tab still has it, another tab never does.
let code = location.hash.slice(1);
try {
  if (CODE.test(code)) sessionStorage.setItem('rax-pair', code);
  else code = sessionStorage.getItem('rax-pair') || '';
  history.replaceState(null, '', location.pathname);
} catch {}

const post = async (path, body) => {
  try {
    const r = await fetch(new URL(path, import.meta.url), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
      referrerPolicy: 'no-referrer',
      cache: 'no-store',
    });
    return await r.json();
  } catch {
    return null;
  }
};

const WHY = {
  gone: 'That code was used or ran out. Scan the screen again.',
  slow: 'Too many tries. Wait a minute, then scan again.',
  account: 'This phone’s sign-in was not accepted. Sign in again, then scan again.',
  handle: 'This account has no @handle yet. Make one at aesthetic.computer, then scan again.',
};

let busy = false;
async function start() {
  if (busy) return;
  busy = true;
  try {
    if (!CODE.test(code)) return void (show(), say('Nothing to sign in. Scan the code on the screen running rax.'));
    const number = await pairNumber(code);
    void number; // (the screen no longer shows it, so the phone does not ask for it to be checked)
    show();
    say('one moment…');
    const token = await accountToken();
    if (!token) return void (show('in'), say('Sign in on this phone, then come back to this page.'));
    // Whose token this is, as the rax server will see it.
    const me = await post('./pair/who', { accessToken: token }), handle = me?.handle;
    if (!me?.ok) return void (show(...(me?.error === 'account' ? ['in'] : [])), say(WHY[me?.error] || 'The rax server did not answer. Try again in a moment.'));
    say(`Sign in the screen you just scanned as @${handle}?`);
    $('yes').textContent = `sign in that screen as @${handle}`;
    $('yes').disabled = false;
    show('note', 'yes', 'no');
    $('yes').onclick = async () => {
      $('yes').disabled = true;
      const answer = await post('./pair', { code, accessToken: token });
      forget();
      show('number');
      say(answer?.ok ? `Done. The screen showing ${number} is now @${answer.handle}.` : WHY[answer?.error] || 'That did not go through. Scan the screen again.');
    };
  } finally {
    busy = false;
  }
}
function forget() {
  code = '';
  try { sessionStorage.removeItem('rax-pair'); } catch {}
}
$('no').onclick = () => { forget(); show(); say('Cancelled. Nothing was signed in.'); };
// Back from signing in (it opens beside this page): look again.
document.addEventListener('visibilitychange', () => { if (!document.hidden && !$('in').hidden) void start(); });
void start();
