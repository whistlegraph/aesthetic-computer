# Cloudflare DNS Records — Migration Reference
# Exported 2026-03-28

## Records Pointing to Netlify (aesthetic-computer.netlify.app or 75.2.60.5)

These are what need to change when migrating to the new DO droplet.

### aesthetic.computer (zone: da794a6ae8f17b80424907f81ed0db7c)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | aesthetic.computer                | 75.2.60.5                           | proxied |
| CNAME | api.aesthetic.computer            | aesthetic-computer.netlify.app      | proxied |
| CNAME | bills.aesthetic.computer          | aesthetic-computer.netlify.app      | proxied |
| CNAME | give.aesthetic.computer           | aesthetic-computer.netlify.app      | proxied |
| CNAME | keeps.aesthetic.computer          | aesthetic-computer.netlify.app      | proxied |
| CNAME | l5.aesthetic.computer             | aesthetic-computer.netlify.app      | proxied |
| CNAME | news.aesthetic.computer           | aesthetic-computer.netlify.app      | proxied |
| CNAME | p5.aesthetic.computer             | aesthetic-computer.netlify.app      | proxied |
| CNAME | pals.aesthetic.computer           | aesthetic-computer.netlify.app      | proxied |
| CNAME | papers.aesthetic.computer         | aesthetic-computer.netlify.app      | proxied |
| CNAME | processing.aesthetic.computer     | aesthetic-computer.netlify.app      | proxied |
| CNAME | sitemap.aesthetic.computer        | aesthetic-computer.netlify.app      | proxied |
| CNAME | www.aesthetic.computer            | aesthetic-computer.netlify.app      | proxied |

### false.work (zone: 0fa28e0097b24e187f41fea0ec036c0d)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| CNAME | builds.false.work                 | aesthetic-computer.netlify.app      | proxied |

### justanothersystem.org (zone: a3366b124c7ca95fe902a54f868dcc51)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | justanothersystem.org             | 75.2.60.5                           | DNS-only |
| CNAME | www.justanothersystem.org         | aesthetic-computer.netlify.app      | DNS-only |

### kidlisp.com (zone: bac7b811ac7b4df664b696fafa9e6207)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | kidlisp.com                       | 75.2.60.5                           | proxied |
| A     | www.kidlisp.com                   | 75.2.60.5                           | proxied |
| CNAME | buy.kidlisp.com                   | aesthetic-computer.netlify.app      | proxied |
| CNAME | calm.kidlisp.com                  | aesthetic-computer.netlify.app      | proxied |
| CNAME | device.kidlisp.com                | aesthetic-computer.netlify.app      | proxied |
| CNAME | keep.kidlisp.com                  | aesthetic-computer.netlify.app      | proxied |
| CNAME | keeps.kidlisp.com                 | aesthetic-computer.netlify.app      | proxied |
| CNAME | learn.kidlisp.com                 | aesthetic-computer.netlify.app      | proxied |
| CNAME | pj.kidlisp.com                    | aesthetic-computer.netlify.app      | proxied |
| CNAME | top.kidlisp.com                   | aesthetic-computer.netlify.app      | proxied |

### notepat.com (zone: 8d289a1e56563dbcc9bc88747428c8ee)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | notepat.com                       | 75.2.60.5                           | proxied |
| CNAME | www.notepat.com                   | aesthetic-computer.netlify.app      | proxied |

### prompt.ac (zone: 1f93ca86e2d9de0def0acb0b8c4e722b)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | prompt.ac                         | 75.2.60.5                           | proxied |
| CNAME | api.prompt.ac                     | aesthetic-computer.netlify.app      | proxied |
| CNAME | l5.prompt.ac                      | aesthetic-computer.netlify.app      | proxied |
| CNAME | p5.prompt.ac                      | aesthetic-computer.netlify.app      | proxied |
| CNAME | papers.prompt.ac                  | aesthetic-computer.netlify.app      | proxied |
| CNAME | processing.prompt.ac              | aesthetic-computer.netlify.app      | proxied |
| CNAME | sitemap.prompt.ac                 | aesthetic-computer.netlify.app      | proxied |

### sotce.net (zone: 1f56f8b5fd7b3db92d31bad0714a518f)
| Type  | Name                              | Target                              | Proxy |
|-------|-----------------------------------|-------------------------------------|-------|
| A     | sotce.net                         | 75.2.60.5                           | proxied |
| A     | www.sotce.net                     | 75.2.60.5                           | proxied |

---

## Records NOT Pointing to Netlify (keep as-is)

### aesthetic.computer — DigitalOcean droplets
- A  at.aesthetic.computer          → 165.227.120.137  (PDS)
- A  *.at.aesthetic.computer        → 165.227.120.137  (PDS wildcard)
- A  chat-clock.aesthetic.computer  → 157.245.134.225  (session server)
- A  chat-system.aesthetic.computer → 157.245.134.225  (session server)
- A  feed.aesthetic.computer        → 64.23.151.169    (silo)
- A  help.aesthetic.computer        → 146.190.150.173  (help)
- A  judge.aesthetic.computer       → 64.227.102.108   (judge)
- A  oven-origin.aesthetic.computer → 137.184.237.166  (oven)
- A  oven.aesthetic.computer        → 137.184.237.166  (oven)
- A  session-server.aesthetic.computer → 157.245.134.225 (session)
- A  silo.aesthetic.computer        → 64.23.151.169    (silo)
- A  udp.aesthetic.computer         → 157.245.134.225  (session)

### aesthetic.computer — DO Spaces CDN
- CNAME art.aesthetic.computer      → art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
- CNAME assets.aesthetic.computer   → assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
- CNAME at-blobs.aesthetic.computer → at-blobs-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
- CNAME logo.aesthetic.computer     → logo.aesthetic.computer.nyc3.cdn.digitaloceanspaces.com
- CNAME music.aesthetic.computer    → music.aesthetic.computer.fra1.cdn.digitaloceanspaces.com
- CNAME private.aesthetic.computer  → private-aesthetic-computer.sfo3.digitaloceanspaces.com
- CNAME releases.aesthetic.computer → releases-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
- CNAME sotce-media.aesthetic.computer → sotce-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
- CNAME user.aesthetic.computer     → user-aesthetic-computer.sfo3.digitaloceanspaces.com
- CNAME wand.aesthetic.computer     → wand-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com

### aesthetic.computer — Third-party services
- CNAME ai.aesthetic.computer       → cname.vercel-dns.com (Vercel)
- CNAME gucci.aesthetic.computer    → glitch.edgeapp.net (Glitch)
- CNAME hi.aesthetic.computer       → Auth0 tenant
- CNAME ipfs.aesthetic.computer     → Pinata IPFS
- CNAME pay.aesthetic.computer      → Stripe hosted checkout
- CNAME shop.aesthetic.computer     → Shopify
- CNAME duckweedtri.aesthetic.computer → ida-surface.netlify.app (separate Netlify site)

### aesthetic.computer — Cloudflare Workers
- AAAA grab.aesthetic.computer      → 100:: (Cloudflare Worker)
- AAAA os.aesthetic.computer        → 100:: (Cloudflare Worker)

### aesthetic.computer — Ngrok tunnels (local dev)
- CNAME local.aesthetic.computer    → ngrok
- CNAME chat-sotce.local.aesthetic.computer → ngrok
- CNAME chat-system.local.aesthetic.computer → ngrok
- CNAME session.local.aesthetic.computer → ngrok

### Stripe DKIM + bounce records (keep as-is)
- 6x CNAME _domainkey records
- CNAME bounce.aesthetic.computer → custom-email-domain.stripe.com

### jas.life — separate site (Vercel + old droplet)
- A jas.life → 75.2.60.5 (**NETLIFY — NEEDS UPDATING to new droplet**)
- A www.jas.life → 76.76.21.21 (Vercel)
- A *.jas.life → 76.76.21.21 (Vercel wildcard)
- Various A records → 162.243.163.221 (old DO droplet for archived sites)

### sotce.net — non-Netlify
- A chat.sotce.net → 157.245.134.225 (session server)
- CNAME bounce.sotce.net → Stripe
- CNAME hi.sotce.net → Auth0 tenant
