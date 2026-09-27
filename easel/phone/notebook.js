// The same sanitized renderer and selection/scroll behavior as the desktop.
window.aesel = {openLink(url) {
  if (/^https?:\/\//i.test(url)) window.webkit?.messageHandlers?.notebook?.postMessage({url});
}};
window.updatePhoneNotebook = value => {
  for (const [name, property] of [['background','--aesel-background'],['foreground','--aesel-foreground'],['userInk','--user-ink'],['error','--error-ink'],['number','--number-ink']]) {
    const color = value.theme?.[name];
    if (/^#[0-9a-f]{6}$/i.test(color || '')) document.documentElement.style.setProperty(property, color);
  }
  if (['light','dark'].includes(value.theme?.colorScheme)) document.documentElement.style.colorScheme = value.theme.colorScheme;
  layoutPreview(value.exclusion);
  window.setNotebookHandle('', []);
  window.updateConversation({entries:value.entries});
  placeActivity(!!value.busy, value.activity || '');
  alignBaselines();
  reportHeight();
  donkey?.paint();
};
// The desktop's pencil companion and activity caption, following the latest
// user prose while a turn runs (renderer.js placeNotebookActivity).
const feedback = document.createElement('span'); feedback.id = 'prompt-feedback'; feedback.hidden = true; feedback.setAttribute('role', 'img');
const caption = document.createElement('span'); caption.id = 'activity-caption'; caption.hidden = true;
const activity = document.createElement('span'); activity.id = 'notebook-activity'; activity.hidden = true; activity.append(feedback, caption);
document.body.append(activity);
const donkey = window.installNotebookDonkey?.(feedback);
const exclusion = document.createElement('div');
exclusion.id = 'notebook-preview-space';
exclusion.setAttribute('aria-hidden', 'true');
document.getElementById('notebook-content').prepend(exclusion);
function layoutPreview(box = {}) {
  const size = name => Number.isFinite(box[name]) ? Math.max(0, box[name]) : 0;
  exclusion.hidden = !size('width') || !size('height');
  exclusion.style.width = `${size('width')}px`;
  exclusion.style.height = `${size('height')}px`;
  exclusion.style.shapeOutside = `inset(${size('top')}px 0 0 0)`;
}
function placeActivity(busy, text) {
  const last = Array.from(document.querySelectorAll('#notebook-page article[data-kind="user"]')).at(-1);
  const target = last?.lastElementChild || last;
  if (target && activity.parentElement !== target) target.append(activity);
  activity.hidden = feedback.hidden = !busy || !target;
  feedback.dataset.status = busy ? text : '';
  feedback.setAttribute('aria-label', busy ? text || 'working' : 'Idle');
  caption.hidden = activity.hidden || !text;
  caption.textContent = caption.hidden ? '' : text;
}
// The app sizes this pane to its prose, so the sheet scrolls as one page.
let reported = 0;
// WebKit and native text have different font ascenders. Measure an actual
// inline baseline, then place it on the shared rule without changing wraps.
function alignBaselines() {
  const content = document.getElementById('notebook-content');
  if (!content) return;
  const origin = content.getBoundingClientRect().top;
  const blocks = content.querySelectorAll('article:not(.rich-reply), .rich-reply p, .rich-reply pre, .rich-reply h1, .rich-reply h2, .rich-reply h3, .rich-reply li:not(:has(p))');
  for (const block of blocks) {
    block.style.top = '0px';
    const marker = document.createElement('span');
    marker.setAttribute('aria-hidden', 'true');
    marker.style.cssText = 'display:inline-block;width:0;height:0;padding:0;margin:0;vertical-align:baseline';
    block.prepend(marker);
    const baseline = marker.getBoundingClientRect().top - origin;
    marker.remove();
    const shift = (24 - baseline % 24) % 24;
    block.style.position = 'relative';
    block.style.top = `${shift}px`;
  }
}
const reportHeight = () => {
  const content = document.getElementById('notebook-content');
  // The preview float and the last article's margin are not output. Reserve
  // one painting row for shifted baselines/descenders; native overlaps that
  // row with the top of its editor so typing continues on the next rule.
  const last = content?.querySelector('article:last-of-type');
  const blocks = last?.querySelectorAll('p, h1, h2, h3, li:not(:has(p)), pre');
  const block = blocks?.length ? blocks[blocks.length - 1] : last;
  let baseline = 0;
  if (block) {
    const marker = document.createElement('span');
    marker.setAttribute('aria-hidden', 'true');
    marker.style.cssText = 'display:inline-block;width:0;height:0;padding:0;margin:0;vertical-align:baseline';
    block.append(marker);
    baseline = marker.getBoundingClientRect().top - content.getBoundingClientRect().top;
    marker.remove();
  }
  const height = Math.max(24, Math.ceil(baseline / 24) * 24 + 24);
  if (height === reported) return;
  reported = height;
  window.webkit?.messageHandlers?.notebook?.postMessage({height});
};
new ResizeObserver(() => { alignBaselines(); reportHeight(); }).observe(document.getElementById('notebook-content'));
document.fonts?.ready.then(() => { alignBaselines(); reportHeight(); });
