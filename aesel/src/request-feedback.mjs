// Observed request stages, without implying progress percentages or hidden reasoning.
export function requestFeedback(state, now = Date.now()) {
  const seconds=Math.max(0,Math.floor((now-(state.requestStartedAt||now))/1000));
  const model=/luna/i.test(state.model||'')?'Luna':String(state.model||'model').split('/').at(-1);
  const phase={preparing:'Preparing request',connecting:'Connecting',waiting:`Waiting for ${model}`,generating:'Receiving reply',composing:'Writing code',writing:'Saving changes',tool:'Running tool',approval:'Waiting for approval',interrupting:'Stopping'}[state.status]||'Working';
  const quiet=Math.max(0,Math.floor((now-(state.lastRequestEventAt||state.requestStartedAt||now))/1000));
  const activity=quiet>=15&&state.status!=='approval'?` · no update for ${quiet}s`:state.progressBytes?` · ${(state.progressBytes/1024).toFixed(1)} KB received`:'';
  return `${phase} · ${seconds}s${activity} · Ctrl-C stop`;
}
