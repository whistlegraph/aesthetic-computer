// Doom, 26.10.12
// Play generated Doom WADs with the shared Freedoom runtime.
export function boot({params,dom:{html}}){
  // A level specification is carried in the URL; the compiler emits a real PWAD.
  const value=params?.[0]||'';
  const query=value.startsWith('{')?'?level='+encodeURIComponent(value):/^[A-Za-z0-9_-]{1,100}$/.test(value)?'?file='+encodeURIComponent('https://aesthetic.computer/api/mime?file='+encodeURIComponent(value)):'';
  html`<iframe title="Doom" src="/doom-player/${query}" style="width:100%;height:100%;border:0" allow="autoplay; fullscreen"></iframe>`;
}
export function paint(){return false;}
export function meta(){return {desc:'Play a generated Doom arena or open a Doom 1 patch WAD.'};}
