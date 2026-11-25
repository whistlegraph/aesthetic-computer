// Aesel follows the system appearance. The media-qualified night stylesheet
// does the work even without JavaScript; this keeps the browser chrome in step.
(() => {
  const system = matchMedia('(prefers-color-scheme: dark)');
  const tint = () => document.querySelector('meta[name="theme-color"]')?.setAttribute('content', system.matches ? '#211c29' : '#fcf0e9');
  try { localStorage.removeItem('aesel.appearance'); } catch {}
  tint();
  system.addEventListener('change', tint);
})();
