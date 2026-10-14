import { observeInteraction } from '../mime-play/interaction.mjs';
observeInteraction();
const params = new URLSearchParams(location.search), id = params.get('v');
const start = Math.min(86400, Math.max(0, Number(params.get('start')) || 0));
const button = document.querySelector('#play'), error = document.querySelector('#error');
const original = document.querySelector('#original');
let player, playing = params.get('warm') !== '1', timer;
const activity = () => parent.postMessage({ type: 'mime:interaction', phase: 'pulse' }, '*');
function fail(message) { clearTimeout(timer); error.textContent = message; error.hidden = false; }
window.__mimeSettle = window.__mimePlayerSettle = active => {
  playing = active;
  if (!active) player?.pauseVideo?.();
};
addEventListener('message', event => {
  if (event.source === parent && event.data?.type === 'mime:playing') window.__mimeSettle(event.data.playing === true);
});
document.addEventListener('visibilitychange', () => { if (document.hidden) player?.pauseVideo?.(); });
if (/^[\w-]{11}$/.test(id || '')) {
  original.href = `https://www.youtube.com/watch?v=${id}` + (start ? `&t=${start}s` : '');
  button.querySelector('img').src = `https://i.ytimg.com/vi/${id}/hqdefault.jpg`;
  button.disabled = false;
  button.onclick = () => {
    activity();
    button.disabled = true;
    timer = setTimeout(() => fail('Player unavailable. Open this video on YouTube.'), 12000);
    window.onYouTubeIframeAPIReady = () => {
      const slot = document.createElement('div'); button.replaceWith(slot);
      player = new YT.Player(slot, {
        host: 'https://www.youtube-nocookie.com', videoId: id,
        playerVars: { playsinline: 1, start, origin: location.origin, autoplay: playing ? 1 : 0 },
        events: {
          onReady() { clearTimeout(timer); if (!playing) player.pauseVideo(); },
          onStateChange() { activity(); if (!playing) player.pauseVideo(); },
          onError() { fail('This video cannot play here. Open it on YouTube.'); },
        },
      });
    };
    const script = document.createElement('script'); script.src = 'https://www.youtube.com/iframe_api';
    script.onerror = () => fail('Player unavailable. Open this video on YouTube.');
    document.head.append(script);
  };
} else { button.remove(); original.remove(); fail('Video unavailable.'); }
// The poster is a usable first frame. Never hold MIME's loading screen on YouTube.
window.__mimeReady = true;
parent.postMessage({ type: 'mime:ready' }, '*');
