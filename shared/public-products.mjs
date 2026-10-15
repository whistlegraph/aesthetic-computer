// Deliberate public descriptions and interfaces. Never derive this from the
// private source inventory or an application's complete route registry.
export const publicProducts = [
  {
    id: "nopaint",
    title: "No Paint",
    url: "https://nopaint.art/",
    summary: "Make paintings by accepting or rejecting proposed moves, with classic and AI tools.",
    access: "Classic tools run without an account. Cloud models require an Aesthetic Computer account and braincells.",
    resources: [
      { title: "Classic app", url: "https://nopaint.art/classic", description: "Preserved earlier application." },
      { title: "Gallery", url: "https://nopaint.art/gallery/", description: "Public painting records." },
      { title: "Gallery JSON", url: "https://nopaint.art/gallery/gallery.json", description: "Public gallery data used by the gallery page." },
    ],
    endpoints: [],
  },
  {
    id: "mime",
    title: "MIME",
    url: "https://mime.ac/",
    summary: "Browse, make and share creative works by file type, one work per screen.",
    access: "Public browsing. An Aesthetic Computer account is required for commenting and saving. The iPhone app is in early access.",
    resources: [
      { title: "MIME types", url: "https://mime.ac/mime-types/", description: "Media types, channels and examples." },
    ],
    endpoints: [
      { method: "GET", url: "https://mime.ac/api/mime?feed=1", title: "Public work feed", description: "Read the public feed. Use each work's public permalink for citations.", authentication: "None", format: "application/json" },
    ],
  },
  {
    id: "sotce-net",
    title: "Sotce Net",
    url: "https://sotce.net/",
    summary: "Sotce's writing, diary pages and community.",
    access: "Public landing page. Member content follows the site's sign-in and subscription requirements.",
    resources: [],
    endpoints: [],
  },
  {
    id: "choir",
    title: "Choir",
    url: "https://choir.ac/",
    summary: "An iPhone community for comments on culture.",
    access: "Participation is by invitation; request access on the public site.",
    resources: [],
    endpoints: [],
  },
  {
    id: "oskiewar",
    title: "oskiewar",
    url: "https://oskiewar.com/",
    summary: "A free 1v1 fighting game for the browser, with local and online play.",
    access: "Sign in with an email code and choose a handle to play.",
    resources: [
      { title: "How to play", url: "https://oskiewar.com/free-fighting-game/", description: "Controls and FAQ for new players." },
      { title: "Support", url: "https://oskiewar.com/support.html", description: "Troubleshooting and contact." },
    ],
    endpoints: [],
  },
];
