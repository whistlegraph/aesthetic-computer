import synth from './synth.txt';
import example from './example.txt';
import paste from './paste.txt';
import marimbaba from './marimbaba.txt';
import marimbabaPaste from './marimbaba-paste.txt';
import marimbabaOrbit from './marimbaba-orbit.txt';
import marimbabaOrbitPaste from './marimbaba-orbit-paste.txt';
import stone from './stone.txt';
import stonePaste from './stone-paste.txt';

// Dedicated AC host: no user data, storage, or third-party API calls.
export default {
  fetch(request) {
    const path = new URL(request.url).pathname;
    const headers = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, HEAD, OPTIONS',
      'Cache-Control': 'no-cache',
      'X-Content-Type-Options': 'nosniff',
    };
    if (request.method === 'OPTIONS') return new Response(null, { status: 204, headers });
    if (!['GET', 'HEAD'].includes(request.method)) {
      return new Response('Method not allowed', { status: 405, headers: { ...headers, Allow: 'GET, HEAD, OPTIONS' } });
    }
    const demos = { '/': example, '/demo': example, '/marimbaba': marimbaba, '/marimbaba-orbit': marimbabaOrbit, '/stone': stone, '/stonwattajetta': stone };
    if (demos[path]) {
      // The human shares this short URL; Strudel receives its native source hash.
      return new Response(null, {
        status: 302,
        headers: { ...headers, Location: 'https://strudel.cc/#' + btoa(String.fromCharCode(...new TextEncoder().encode(demos[path]))) },
      });
    }
    const files = {
      '/stone/source': [stone, 'text/plain; charset=utf-8'],
      '/stone/paste': [stonePaste, 'text/plain; charset=utf-8'],
      '/s': [synth, 'text/javascript; charset=utf-8'],
      '/synth.mjs': [synth, 'text/javascript; charset=utf-8'],
      '/paste': [paste, 'text/plain; charset=utf-8'],
      '/source': [example, 'text/plain; charset=utf-8'],
      '/marimbaba/source': [marimbaba, 'text/plain; charset=utf-8'],
      '/marimbaba/paste': [marimbabaPaste, 'text/plain; charset=utf-8'],
      '/marimbaba-orbit/source': [marimbabaOrbit, 'text/plain; charset=utf-8'],
      '/marimbaba-orbit/paste': [marimbabaOrbitPaste, 'text/plain; charset=utf-8'],
    };
    const file = files[path];
    if (!file) return new Response('Not found', { status: 404, headers });
    return new Response(request.method === 'HEAD' ? null : file[0], {
      headers: { ...headers, 'Content-Type': file[1] },
    });
  },
};
