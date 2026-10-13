// A mounted editor can keep rendering and finishing work while it is silent.
// WebKit's media playback suspension alone does not stop Web Audio worklets.
(() => {
  let active = window.__whistlegraphPlaybackActive !== false;
  const contexts = new Map();
  const AudioContext = window.AudioContext || window.webkitAudioContext;
  if (AudioContext) {
    const resume = AudioContext.prototype.resume;
    const suspend = AudioContext.prototype.suspend;
    const settle = (context, wanted) => {
      if (context.state === 'closed') { contexts.delete(context); return; }
      const action = active ? (wanted ? resume : null) : suspend;
      if (action) void action.call(context).catch(() => {});
    };
    AudioContext.prototype.resume = function () {
      contexts.set(this, true);
      return active ? resume.call(this) : Promise.resolve();
    };
    AudioContext.prototype.suspend = function () {
      contexts.set(this, false);
      return suspend.call(this);
    };
    const GuardedContext = new Proxy(AudioContext, {
      construct(Target, args, NewTarget) {
        const context = Reflect.construct(Target, args, NewTarget);
        contexts.set(context, context.state === 'running');
        context.addEventListener('statechange', () => {
          if (context.state === 'closed') contexts.delete(context);
          // A resume already queued before navigation may finish afterwards.
          else if (!active && context.state === 'running') settle(context, false);
        });
        settle(context, contexts.get(context));
        return context;
      },
    });
    window.AudioContext = GuardedContext;
    if (window.webkitAudioContext) window.webkitAudioContext = GuardedContext;
    window.whistlegraphSetPlaybackActive = value => {
      active = value === true;
      window.__whistlegraphPlaybackActive = active;
      for (const [context, wanted] of contexts) settle(context, wanted);
      for (let i = 0; i < window.frames.length; i++) {
        window.frames[i].postMessage({type: 'whistlegraph:playback', active}, '*');
      }
    };
    window.addEventListener('message', event => {
      if (window !== window.top && event.source === window.parent && event.data?.type === 'whistlegraph:playback') {
        window.whistlegraphSetPlaybackActive(event.data.active);
      }
    });
  }
  // New documents start silent in MIME, then ask the native owner for its
  // current state. This also covers a WAD/frame created after leaving editor.
  window.webkit?.messageHandlers?.whistlegraph?.postMessage({action: 'playbackReady'});
  if (window.__whistlegraphPlaybackTest) setInterval(() => {
    const states = [...contexts.keys()].map(context => context.state);
    const waveform = window.AC?.readOutputWaveform?.() || [];
    const peak = waveform.reduce((value, sample) => Math.max(value, Math.abs(sample)), 0);
    window.webkit?.messageHandlers?.whistlegraph?.postMessage({action: 'playbackProbe', states, peak});
  }, 200);
})();
