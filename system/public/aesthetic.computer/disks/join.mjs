import { signupLayout, drawOnboarding } from '../lib/onboarding-ui.mjs';
let view, dimensions;
function boot($) {
  view = null; dimensions = ''; $.hud?.label(' ', [0, 0, 0, 0]);
  $.send({ type: 'keyboard:lock' });
  $.send({ type: 'signup:canvas', content: { action: 'ready', width: $.screen.width, height: $.screen.height } });
}
function paint($) {
  const key = `${$.screen.width}:${$.screen.height}`;
  if (key !== dimensions) { dimensions = key; $.send({ type: 'signup:canvas', content: { action: 'ready', width: $.screen.width, height: $.screen.height } }); }
  if (!view) { $.wipe(25, 19, 37); return; }
  const height = Math.min($.screen.height, view.height || $.screen.height);
  const layout = signupLayout(view, $.screen.width, height, view.scroll);
  if (view.top) for (const item of layout.items) item.y += view.top;
  drawOnboarding($, view, layout);
}
function receive({ type, content }, $) { if (type === 'signup:view') { view = content; $.needsPaint(); } }
function act($) {
  const e = $.event;
  if (e.is('scroll') || e.is('draw:1')) $.send({ type: 'signup:canvas', content: { action: 'scroll', delta: e.is('scroll') ? -e.y : -e.delta.y } });
}
function leave($) { $.send({ type: 'signup:canvas', content: { action: 'leave' } }); $.send({ type: 'keyboard:unlock' }); }
export { boot, paint, receive, act, leave };
