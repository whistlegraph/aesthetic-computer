(() => {
  const main = document.querySelector('main');
  if (!main) return;
  function place() {
    const entry = document.querySelector('#tl-norman-index-entry');
    const list = main.querySelector('.tl-oct-project-index');
    const next = list?.querySelector('a[href*="/pat-douthewaite-art-in-context/"]')?.closest('article');
    if (!entry || !next) return false;
    next.before(entry);
    document.querySelector('#tl-norman-index-fallback')?.remove();
    return true;
  }
  if (place()) return;
  const observer = new MutationObserver(() => { if (place()) observer.disconnect(); });
  observer.observe(main, {childList: true, subtree: true});
  setTimeout(() => observer.disconnect(), 10000);
})();
