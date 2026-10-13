// Profile, 2026.10.12
// A shared DOM profile, also available as a modal in MIME.

let domApi;
export function meta({ piece }) {
  return { title: `${piece} - aesthetic.computer`, desc: `Profile for ${piece}.` };
}

export function boot({ params, handle, user, dom, hud, net }) {
  domApi = dom;
  const bare = String(params[0] || handle() || "").replace(/^@/, "");
  const visiting = /^[a-z0-9][a-z0-9_.-]{0,63}$/i.test(bare) ? "@" + bare.toLowerCase() : null;
  hud.label("");
  if (visiting) net.rewrite(visiting);
  const action = user ? user.email_verified ? "handle" : "email" : "imnew";
  const label = user ? user.email_verified ? "Create a handle" : "Verify your email" : "Log in or sign up";
  dom.html`
    <section id="ac-profile-page">
      <style>
        #ac-profile-page { position:fixed; inset:32px 0 0; z-index:10; overflow:auto;
          overscroll-behavior:contain; background:#f6f5ef; color:#232821; pointer-events:auto;
          padding:24px max(16px, calc((100vw - 440px) / 2)); font:16px system-ui,sans-serif; }
        #ac-profile-page a { color:inherit; }
        #ac-profile-page nav { margin-bottom:24px; }
        @media(prefers-color-scheme:dark) { #ac-profile-page { background:#101a16; color:#f3f3e9; } }
      </style>
      <nav><a href="/prompt">Aesthetic Computer</a></nav>
      ${visiting ? `<ac-profile handle="${visiting}"></ac-profile>` : `<a href="/${action}">${label}</a>`}
      <script type="module" src="/aesthetic.computer/lib/profile-piece.mjs"></script>
    </section>`;
}
export function paint({ wipe }) { wipe(16, 26, 22); }
export function leave() { domApi?.clear(); domApi = null; }
