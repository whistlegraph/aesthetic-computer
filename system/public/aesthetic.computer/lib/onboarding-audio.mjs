// A small original pentatonic loop, started only by a user's interaction.
export function createOnboardingAudio(win) {
  let context, timer, step = 0, active = false;
  let enabled = true;
  try { enabled = win.sessionStorage.getItem('ac:onboarding:music') !== 'off'; } catch {}
  const note = (tone, duration, volume, type = 'sine') => {
    if (!context || context.state !== 'running') return;
    const t = context.currentTime, oscillator = context.createOscillator(), gain = context.createGain();
    oscillator.type = type; oscillator.frequency.value = tone;
    gain.gain.setValueAtTime(0, t); gain.gain.linearRampToValueAtTime(volume, t + .008);
    gain.gain.exponentialRampToValueAtTime(.0001, t + duration);
    oscillator.connect(gain); gain.connect(context.destination); oscillator.start(t); oscillator.stop(t + duration + .02);
    oscillator.onended = () => { oscillator.disconnect(); gain.disconnect(); };
  };
  const stop = () => { win.clearInterval(timer); timer = null; };
  const run = () => {
    if (!enabled || !active || win.document.hidden || context?.state !== 'running') { stop(); return; }
    if (timer) return;
    const melody = [0, 7, 12, 16, 14, 12, 7, 4, 0, 7, 12, 19, 16, 14, 12, 7];
    timer = win.setInterval(() => {
      if (win.document.hidden || !active) return stop();
      note(196 * 2 ** (melody[step % melody.length] / 12), .32, .025, 'triangle');
      if (step % 4 === 0) note(step % 16 < 8 ? 98 : 130.81, .7, .035);
      step++;
    }, 375);
  };
  const unlock = () => {
    if (!active) return;
    try {
      context ||= new (win.AudioContext || win.webkitAudioContext)();
      if (context.state === 'running') run();
      else Promise.resolve(context.resume()).then(run).catch(() => {});
    } catch {}
  };
  win.document.addEventListener('pointerdown', unlock, true);
  win.document.addEventListener('keydown', unlock, true);
  win.document.addEventListener('visibilitychange', () => { if (win.document.hidden) stop(); else run(); });
  return {
    get enabled() { return enabled; },
    enter() { active = true; run(); },
    leave() { active = false; stop(); },
    tap() { note(784, .075, .09, 'triangle'); },
    toggle() { enabled = !enabled; try { win.sessionStorage.setItem('ac:onboarding:music', enabled ? 'on' : 'off'); } catch {} run(); return enabled; },
  };
}
