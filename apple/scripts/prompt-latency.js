// Opt-in native benchmark. Records timing/lengths only, never typed content.
(() => {
  const samples = [], rafGaps = [];
  let lastRAF, sequence = 0, active;
  const now = () => performance.now();
  function frame(time) {
    if (lastRAF && samples.length) rafGaps.push(time - lastRAF);
    lastRAF = time;
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);
  document.addEventListener('keydown', e => {
    if (!e.isTrusted || e.key.length !== 1) return;
    active = { sequence: ++sequence, keydown: now(), trusted: true, repeat: e.repeat };
    samples.push(active);
  }, true);
  document.addEventListener('input', e => {
    if (active && e.target.id === 'software-keyboard-input') active.input = now();
  }, true);
  const post = Worker.prototype.postMessage;
  Worker.prototype.postMessage = function(message, ...args) {
    if (message?.type === 'prompt:text:replace' && active && active.input && !active.sent) {
      active.sent = now();
      message.content.inputLatencySequence = active.sequence;
    }
    return post.call(this, message, ...args);
  };
  window.acInputLatencyProbe = {
    received(sequence) {
      const sample = samples.find(s => s.sequence === sequence);
      if (sample && !sample.received) sample.received = now();
    },
    rendered(sequence) {
      const sample = samples.find(s => s.sequence === sequence);
      if (!sample || sample.rendered) return;
      sample.rendered = now();
      requestAnimationFrame(() => { sample.nextRAF = now(); });
    },
    result() {
      const summarize = values => {
        values.sort((a,b)=>a-b);
        return values.length ? { count:values.length, median:values[Math.floor(values.length*.5)], p95:values[Math.min(values.length-1,Math.floor(values.length*.95))], max:values.at(-1) } : null;
      };
      const deltas=(a,b)=>samples.filter(s=>s[a] !== undefined && s[b] !== undefined).map(s=>s[b]-s[a]);
      return {
        userAgent:navigator.userAgent, pixelRatio:devicePixelRatio,
        viewport:{width:innerWidth,height:innerHeight},
        inputLength:document.querySelector('#software-keyboard-input')?.value.length,
        keys:samples.length, rendered:samples.filter(s=>s.rendered).length,
        keyToDOM:summarize(deltas('keydown','input')),
        DOMToWorkerSend:summarize(deltas('input','sent')),
        sendToFrame:summarize(deltas('sent','received')),
        renderer:summarize(deltas('received','rendered')),
        keyToRendered:summarize(deltas('keydown','rendered')),
        keyToNextRAF:summarize(deltas('keydown','nextRAF')),
        displayFrameInterval:summarize(rafGaps), samples,
        note:'DOM keydown to canvas submission; next RAF is a presentation opportunity, not a physical display measurement.'
      };
    }
  };
})();
