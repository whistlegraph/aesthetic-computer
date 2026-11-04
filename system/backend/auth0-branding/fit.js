// Measure Auth0's rendered widget, including late validation and Turnstile.
// Do not read credentials, replace inputs, navigate, or reach into iframes.
(function () {
  var root = document.documentElement;
  var fit = document.getElementById('ac-login-fit');
  var stage = document.getElementById('ac-login-stage');
  if (!fit || !stage || typeof ResizeObserver === 'undefined') return;
  var pending = false, previous = 0;
  var initialRatio = window.devicePixelRatio;
  function update() {
    pending = false;
    // Explicit browser zoom must remain an accessibility escape hatch.
    if (window.devicePixelRatio !== initialRatio) {
      root.classList.remove('ac-fit-ready');
      return;
    }
    root.classList.add('ac-fit-ready');
    var w = Math.max(stage.offsetWidth, stage.scrollWidth);
    var h = Math.max(stage.offsetHeight, stage.scrollHeight);
    if (!w || !h) return;
    var width = document.documentElement.clientWidth;
    // Use the layout viewport: focusing an input must not shrink the card
    // again when a phone's keyboard opens. Pinch zoom stays browser-controlled.
    var height = window.innerHeight;
    var scale = Math.min(1, Math.max(1, width - 24) / w, Math.max(1, height - 24) / h);
    if (Math.abs(scale - previous) > 0.0001) {
      stage.style.setProperty('--ac-login-scale', String(scale));
      previous = scale;
    }
    fit.style.height = height + 'px';
  }
  function schedule() {
    if (!pending) { pending = true; requestAnimationFrame(update); }
  }
  new ResizeObserver(schedule).observe(stage);
  // Auth0 can replace a screen or add a validation message without navigation.
  new MutationObserver(schedule).observe(stage, { childList: true, subtree: true, characterData: true });
  window.addEventListener('resize', schedule);
  window.addEventListener('pageshow', schedule);
  if (document.fonts) document.fonts.ready.then(schedule);
  schedule();
})();
