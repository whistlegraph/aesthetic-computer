import { logoUrl, turnaroundUrl } from "./logo.mjs";

export const drippedPals = [
  { slug: "psycho-dripped-pink", name: "Pink sky" },
  { slug: "psycho-dripped", name: "After dark" },
];

export function renderDrippedPals() {
  const image = logoUrl(drippedPals[0].slug);
  const cards = drippedPals.map(({ slug, name }) => `<article>
    <video muted loop playsinline preload="metadata" poster="${logoUrl(slug)}" aria-label="${name}: liquid cyan Pals with gently moving highlights">
      <source src="${turnaroundUrl(slug, "mp4")}" type="video/mp4">
    </video>
    <div class="caption"><h2>${name}</h2><button type="button" aria-pressed="false" aria-label="Play ${name}">Play</button></div>
    <nav aria-label="Download ${name}">${["png", "mp4", "webp", "apng"].map((format) => `<a href="/pals-${slug}.${format}?download=1" download>${format.toUpperCase()}</a>`).join("")}</nav>
  </article>`).join("");
  return `<!doctype html>
<html lang="en"><head>
  <meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
  <title>Dripped Pals · Aesthetic Computer</title>
  <meta name="description" content="Liquid cyan Pals in a pink sunset and after dark. Download original stills, video loops, WebP and APNG animations.">
  <link rel="canonical" href="https://pals.aesthetic.computer/dripped">
  <link rel="icon" href="${image}" type="image/png">
  <meta property="og:title" content="Dripped Pals"><meta property="og:image" content="${image}">
  <meta property="og:type" content="website"><meta property="og:url" content="https://pals.aesthetic.computer/dripped">
  <meta name="twitter:card" content="summary_large_image">
  <style>
    *{box-sizing:border-box}body{margin:0;background:#241522;color:#ffe5f2;font:16px system-ui,sans-serif}
    header,main,footer{max-width:1500px;margin:auto;padding:24px}header{display:flex;align-items:baseline;justify-content:space-between;gap:20px}
    h1{font-size:clamp(26px,4vw,52px);font-weight:600;letter-spacing:-.035em;margin:0}a{color:inherit;text-underline-offset:5px}
    main{display:grid;grid-template-columns:1fr 1fr;gap:24px;padding-top:0}article{min-width:0}video{display:block;width:100%;aspect-ratio:1;background:#100b11;object-fit:contain}
    .caption{display:flex;align-items:center;justify-content:space-between;gap:16px;margin-top:12px}h2{font-size:20px;font-weight:500;margin:0}
    button{font:inherit;color:inherit;background:none;border:1px solid #9f6b88;border-radius:30px;min-width:72px;min-height:44px;cursor:pointer}
    nav{display:flex;gap:8px;flex-wrap:wrap;margin-top:2px}nav a{display:inline-flex;align-items:center;min-height:44px;padding-right:14px}a:focus-visible,button:focus-visible{outline:3px solid #4df1ff;outline-offset:4px}
    footer{padding-top:0;font-size:14px;color:#dab7cb}footer a{display:inline-block;min-height:44px;padding:12px 0}
    @media(max-width:700px){header,main,footer{padding:16px}main{grid-template-columns:1fr;padding-top:0;gap:32px}header>a{white-space:nowrap}footer{padding-top:0}}
  </style>
</head><body>
  <header><h1>Dripped Pals</h1><a href="/">All pals ↗</a></header>
  <main>${cards}</main>
  <footer><a href="https://aesthetic.computer">Aesthetic Computer</a> · Images made with OpenAI; motion with FAL Seedance 2.0.</footer>
  <script>
    const reduced = matchMedia('(prefers-reduced-motion: reduce)');
    document.querySelectorAll('article').forEach(card => {
      const video = card.querySelector('video'), button = card.querySelector('button');
      const name = card.querySelector('h2').textContent;
      let manual = false;
      const update = () => { button.textContent = video.paused ? 'Play' : 'Pause'; button.setAttribute('aria-pressed', String(!video.paused)); button.setAttribute('aria-label', (video.paused ? 'Play ' : 'Pause ') + name); };
      button.addEventListener('click', () => { manual = true; if(video.paused) video.play().catch(update); else video.pause(); });
      video.addEventListener('play', update); video.addEventListener('pause', update);
      const observer = new IntersectionObserver(entries => { for(const entry of entries) { if(!entry.isIntersecting) video.pause(); else if(!reduced.matches && !manual) video.play().catch(update); } }, {threshold:.25});
      observer.observe(video);
      reduced.addEventListener('change', () => { if(reduced.matches) video.pause(); });
    });
  </script>
</body></html>`;
}
