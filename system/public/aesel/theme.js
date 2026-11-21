// The media-qualified stylesheet handles Auto even when JavaScript is disabled.
(() => {
  const key = 'aesel.appearance';
  const palette = document.getElementById('night-palette');
  const system = matchMedia('(prefers-color-scheme: dark)');
  let choice = 'auto';
  function apply(value) {
    choice = ['dark','light'].includes(value) ? value : 'auto';
    palette.media = choice === 'auto' ? system.media : choice === 'dark' ? 'all' : 'not all';
    const dark = choice === 'dark' || (choice === 'auto' && system.matches);
    document.querySelector('meta[name="theme-color"]')?.setAttribute('content', dark ? '#211c29' : '#fcf0e9');
    const select = document.getElementById('appearance');
    if (select) select.value = choice;
  }
  try { apply(localStorage.getItem(key)); } catch { apply('auto'); }
  system.addEventListener('change', () => apply(choice));
  addEventListener('storage', e => { if (e.key === key || e.key === null) apply(e.newValue); });
  document.addEventListener('DOMContentLoaded', () => {
    const select = document.getElementById('appearance');
    select.value = choice;
    select.parentElement.hidden = false;
    select.onchange = () => {
      apply(select.value);
      try { localStorage.setItem(key,choice); } catch {}
    };
  });
})();
