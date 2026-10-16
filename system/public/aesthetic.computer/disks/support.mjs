// Native monthly support. Stripe owns card entry; AC owns the amount and return.
import { palette, wrap } from '../lib/onboarding-ui.mjs';
let amount, keypad, error, busy, source, lifetime = 0, buttons, music, focus, scroll, scrollMax;
function boot($) {
  lifetime++; amount = /^\d+(\.\d{1,2})?$/.test($.query?.amount || '') ? $.query.amount : '4';
  keypad = false; error = ''; busy = false; buttons = {}; music = true; focus = null; scroll = scrollMax = 0;
  source = $.query?.source === 'signup' ? 'signup' : 'handle-colors';
  $.hud?.label(' ', [0, 0, 0, 0]); $.send?.({ type: 'onboarding:audio', content: { action: 'enter' } });
}
function open($, url) {
  if ($.net?.iframe) $.send({ type: 'post-to-parent', content: { type: 'openExternal', url } });
  else $.jump(url);
}
async function checkout($) {
  if (busy) return;
  const cents = Math.round(Number(amount) * 100);
  if (!/^\d+(\.\d{1,2})?$/.test(amount) || cents < 100 || cents > 250000) { error = 'Choose $1 to $2,500.'; $.needsPaint(); return; }
  busy = true; error = ''; $.needsPaint(); const mine = lifetime;
  try {
    const body = { amount: cents, currency: 'usd', recurring: true, source, surface: 'piece' };
    const result = await $.net.userRequest('POST', '/api/give', body);
    if (mine !== lifetime) return;
    if (result.code === 'already_supporting') return $.jump('handle');
    if (result.status === 401) throw Error('Please log in again.');
    if (result.status !== 200 || !result.url) throw Error('Checkout unavailable. Try again.');
    const url = new URL(result.url);
    if (url.protocol !== 'https:' || !['checkout.stripe.com', 'pay.aesthetic.computer'].includes(url.hostname)) throw Error('Checkout unavailable.');
    open($, url.href);
  } catch (e) { if (mine === lifetime) error = e.message || 'Please try again.'; }
  finally { if (mine === lifetime) { busy = false; $.needsPaint(); } }
}
function paint($) {
  const { ink, screen, ui } = $; $.wipe(palette.ground);
  const w = Math.min(252, screen.width - 24), x = Math.floor((screen.width - w) / 2);
  const total = keypad ? 286 : 266;
  scrollMax = Math.max(0, total - screen.height); scroll = Math.min(scroll, scrollMax);
  let y = Math.max(40, Math.floor((screen.height - total) / 3) + 40) - scroll;
  for (const entry of Object.values(buttons)) entry.visible = false;
  const line = (text, color = palette.text) => { for (const l of wrap(text, w)) { ink(color).write(l, { x, y }, undefined, undefined, false, 'MatrixChunky8'); y += 11; } y += 7; };
  const button = (id, label, bx, by, bw, action, selected = false) => {
    const entry = buttons[id] ||= { button: new ui.Button() }; entry.action = action; entry.visible = true;
    Object.assign(entry.button.box, { x: bx, y: by, w: bw, h: 25 });
    entry.button.disabled = busy || by < 34 || by + 25 > screen.height;
    ink(selected ? palette.pink : palette.panel).box(entry.button.box, 'fill');
    ink(focus === id ? palette.mint : palette.muted).box(entry.button.box, 'outline');
    ink(selected ? palette.ground : palette.text).write(label, { x: bx + bw / 2, y: by + 8, center: 'x' }, undefined, undefined, false, 'MatrixChunky8');
  };
  line('Support AC monthly', palette.pink);
  if (!keypad) line('Monthly support unlocks handle colors across AC.', palette.muted);
  line(`$${amount || '0'} / month`, palette.mint);
  if (keypad) {
    const keys = ['1','2','3','4','5','6','7','8','9','.','0','<'];
    const cell = Math.floor((w - 8) / 3);
    keys.forEach((key, i) => button('key' + key, key, x + (i % 3) * (cell + 4), y + Math.floor(i / 3) * 29, cell, () => {
      if (key === '<') amount = amount.slice(0, -1);
      else if (amount.length < 7 && !(key === '.' && amount.includes('.')) && !/\.\d{2}$/.test(amount)) amount += key;
    }));
    y += 120;
    button('done', 'Done', x, y, w, () => { keypad = false; scroll = 0; }, true); y += 33;
  } else {
    const cell = Math.floor((w - 12) / 4);
    [1,4,8,16].forEach((value, i) => button('amount' + value, '$' + value, x + i * (cell + 4), y, cell, () => { amount = String(value); }, Number(amount) === value)); y += 31;
    button('custom', 'Another amount', x, y, w, () => { keypad = true; amount = ''; scroll = 0; }); y += 34;
    button('checkout', busy ? 'Opening...' : 'Continue to checkout', x, y, w, () => checkout($), true); y += 34;
    line('Renews monthly. Cancel anytime.', palette.muted);
    line('Not tax deductible.', palette.muted);
  }
  if (error) line(error, palette.pink);
  // Error copy can extend the page on the smallest screens.
  scrollMax = Math.max(scrollMax, y + scroll + 8 - screen.height);
  ink(palette.ground).box(0, 0, screen.width, 34, 'fill');
  button('back', 'Back', 8, 5, 42, () => keypad ? (keypad = false, scroll = 0) : $.jump(['signup', 'handle-colors'].includes(source) ? 'handle' : 'prompt'));
  button('music', music ? 'Music on' : 'Music off', screen.width - 80, 5, 72, () => $.send({ type: 'onboarding:audio', content: { action: 'toggle' } }));
  buttons.back.button.disabled = busy; buttons.music.button.disabled = false;
}
function act($) {
  const e = $.event, visible = Object.entries(buttons).filter(([, b]) => b.visible && !b.button.disabled);
  const push = entry => { if (entry?.button.disabled) return; $.send?.({ type: 'onboarding:audio', content: { action: 'tap' } }); error = ''; entry?.action(); $.needsPaint(); };
  if (e.is('keyboard:down:tab')) { const keys = visible.map(([id]) => id); focus = keys[(keys.indexOf(focus) + 1) % keys.length]; }
  else if (e.is('keyboard:down:enter') && focus) push(buttons[focus]);
  else if (e.is('scroll')) scroll = Math.max(0, Math.min(scrollMax, scroll - e.y));
  else if (e.is('draw:1') && !visible.some(([, b]) => b.button.down)) scroll = Math.max(0, Math.min(scrollMax, scroll - e.delta.y));
  for (const [, entry] of visible) entry.button.act(e, { push: () => push(entry) });
  $.needsPaint();
}
function receive({ type, content }, $) { if (type === 'onboarding:audio-state') { music = content.enabled; $.needsPaint(); } }
function leave($) { lifetime++; $.send?.({ type: 'onboarding:audio', content: { action: 'leave' } }); }
function meta() { return { title: 'Support AC', desc: 'Monthly support unlocks handle colors across AC.' }; }
export { boot, paint, act, receive, leave, meta };
