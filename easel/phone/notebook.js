// The same sanitized renderer and selection/scroll behavior as the desktop.
window.aesel = {openLink(url) {
  if (/^https?:\/\//i.test(url)) window.webkit?.messageHandlers?.notebook?.postMessage({url});
}};
window.updatePhoneNotebook = value => {
  for (const [name, property] of [['background','--aesel-background'],['foreground','--aesel-foreground'],['userInk','--user-ink']]) {
    const color = value.theme?.[name];
    if (/^#[0-9a-f]{6}$/i.test(color || '')) document.documentElement.style.setProperty(property, color);
  }
  window.setNotebookHandle(value.handle || '', value.colors || []);
  window.updateConversation({entries:value.entries});
  placeActivity(!!value.busy, value.activity || '');
  reportHeight();
};
// The desktop's pencil companion and activity caption, following the latest
// user prose while a turn runs (renderer.js placeNotebookActivity).
const feedback = document.createElement('span'); feedback.id = 'prompt-feedback'; feedback.hidden = true; feedback.setAttribute('role', 'img');
const caption = document.createElement('span'); caption.id = 'activity-caption'; caption.hidden = true;
const activity = document.createElement('span'); activity.id = 'notebook-activity'; activity.hidden = true; activity.append(feedback, caption);
document.body.append(activity);
window.installNotebookDonkey?.(feedback);
function placeActivity(busy, text) {
  const last = Array.from(document.querySelectorAll('#notebook-page article[data-kind="user"]')).at(-1);
  const target = last?.lastElementChild || last;
  if (target && activity.parentElement !== target) target.append(activity);
  activity.hidden = feedback.hidden = !busy || !target;
  feedback.setAttribute('aria-label', busy ? text || 'working' : 'Idle');
  caption.hidden = activity.hidden || !text;
  caption.textContent = caption.hidden ? '' : text;
}
// The app sizes this pane to its prose, so the sheet scrolls as one page.
let reported = 0;
const reportHeight = () => {
  const height = Math.ceil(document.documentElement.scrollHeight);
  if (height === reported) return;
  reported = height;
  window.webkit?.messageHandlers?.notebook?.postMessage({height});
};
new ResizeObserver(reportHeight).observe(document.documentElement);
document.fonts?.ready.then(reportHeight);
