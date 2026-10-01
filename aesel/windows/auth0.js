// Native Windows credential owner; refresh tokens never enter this document.
(() => {
  if (window !== window.top || location.origin !== 'https://aesel.app') return;
  let next = 0;
  const pending = new Map();
  window.chrome.webview.addEventListener('message', ({data}) => {
    const promise = pending.get(data.id);
    if (!promise) return;
    pending.delete(data.id);
    data.error ? promise.reject(new Error(data.error)) : promise.resolve(data.result);
  });
  const call = method => new Promise((resolve, reject) => {
    const id = ++next;
    pending.set(id, {resolve, reject});
    window.chrome.webview.postMessage({id, method});
  });
  window.auth0 = {Auth0Client: class {
    checkSession() { return call('checkSession'); }
    isAuthenticated() { return call('isAuthenticated'); }
    getUser() { return call('getUser'); }
    getTokenSilently() { return call('getTokenSilently'); }
    async loginWithRedirect() { await call('loginWithRedirect'); location.reload(); }
    async logout() { await call('logout'); location.reload(); }
  }};
})();
