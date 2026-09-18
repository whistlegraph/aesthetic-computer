// The same sanitized renderer and selection/scroll behavior as the desktop.
window.aesel = {openLink(url) {
  if (/^https?:\/\//i.test(url)) window.webkit?.messageHandlers?.notebook?.postMessage({url});
}};
window.updatePhoneNotebook = value => {
  window.setNotebookHandle(value.handle || '', value.colors || []);
  window.updateConversation({entries:value.entries});
};
