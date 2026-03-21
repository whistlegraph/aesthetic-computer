# Aesthetic Computer Gift Page Research Report

**Created:** January 2025  
**Purpose:** Document why AC matters, what's been accomplished, what's coming, and why support is needed

---

## 🎨 What is Aesthetic Computer?

Aesthetic Computer is a **mobile-first runtime and social network for creative computing**—a platform where anyone can make, share, and play with interactive software toys.

Think of it like a musical instrument: users discover their own paths through the network of commands and published pieces. As they grow their literacy through play and exploration, they can improvise, recombine, and expand their creative repertoire.

### Origin Story

- **2020 (COVID):** Jeffrey created [No Paint](https://nopaint.art), a free painting app that [reached the front page of Hacker News](https://news.ycombinator.com/item?id=23546706)
- No Paint users started contributing custom pixel stamps and brushes
- Non-technical users learned about computing through contributing to software they loved
- **2021:** Jeffrey started writing Aesthetic Computer to solve the friction—a system with:
  - Mobile-first defaults
  - JavaScript API for creative coding
  - Networked multiplayer
  - Social handle system for publishing

---

## 📊 Community Statistics

Data from the live MongoDB database shows real community engagement:

| Metric | Count |
|--------|-------|
| **Registered Users** | 4,217 |
| **Unique Handles** | 2,723 |
| **Chat Messages (main)** | 17,508 |
| **Chat Messages (Danish community)** | 9,458 |
| **Saved Paintings** | 4,280 |
| **KidLisp Programs** | 12,933 |
| **User Moods** | 2,832 |
| **Tapes (recordings)** | 95 |

---

## 🇩🇰 Danish Community: R8dio & Laer-Klokken

One of AC's most vibrant communities emerged organically in Denmark:

### R8dio.dk Integration
- **`r8dio`** piece connects to Danish talk radio streaming
- 32-bar audio visualizer synced to broadcast
- Real-time listener experience

### Laer-Klokken ("Learn the Clock")
- Educational piece for Danish children learning to tell time
- Active community chat with **9,458+ messages**
- Teachers and kids using AC in Danish classrooms
- Community members helping each other learn

This wasn't planned—it emerged because the platform was open enough for a community to form around their own needs.

---

## 🎹 Featured Work: Notepat

**Notepat** is AC's most beloved piece—a musical instrument that lets anyone play melodies.

- [Featured on Hacker News](https://news.ycombinator.com/item?id=41526754) in 2024
- Simple interface: tap squares to create tones
- Works on any device with a browser
- Demonstrates AC's core philosophy: simple tools, deep expression

---

## 🧒 Coming Soon: KidLisp.com

**KidLisp** is a new visual programming language launching on [kidlisp.com](https://kidlisp.com):

> "Friendly coding for everyone"

### What is KidLisp?

A minimal Lisp dialect designed for creating generative art and interactive experiences. It's beginner-friendly but powerful enough for real creative work.

### Language Features

**118 functions across 12 categories:**
- Graphics: `wipe`, `ink`, `line`, `box`, `circle`, `shape`, `text`
- Animation: `wiggle`, `zoom`, `spin`, `wave`, `pulse`
- Interaction: `tap`, `drag`, `keyboard`
- Sound: `sound`, `synth`
- Math: `rand`, `clamp`, `sin`, `cos`
- Control: `repeat`, `later`, `every`
- And more...

### Example
```lisp
(wipe "black")
(ink "yellow")
(circle width/2 height/2 100)
(repeat 12 i
  (ink (rainbow i/12))
  (line width/2 height/2 
        (+ width/2 (* 90 (cos (* i 30))))
        (+ height/2 (* 90 (sin (* i 30))))))
```

### Community Impact
- **12,933** KidLisp programs already saved in the database
- Kid-friendly syntax removes barriers to creative coding
- Works on phones, tablets, computers—anywhere

---

## 🔧 Technical Infrastructure

AC runs on a complex infrastructure that costs money monthly:

### Services
- **Netlify** — Static hosting and edge functions
- **MongoDB Atlas** — Database for users, paintings, chat, KidLisp programs
- **Jamsocket** — Real-time session server for multiplayer
- **Redis** — Caching and real-time data
- **Cloudflare R2** — Asset storage
- **Shopify** — Shop integration
- **Stripe** — Payments

### Open Source
The entire codebase is [open on GitHub](https://github.com/whistlegraph/aesthetic-computer), including:
- Client runtime
- Server infrastructure
- All built-in pieces
- KidLisp interpreter
- Session server

---

## 💡 Why This Matters

### For Education
- Teachers use AC to introduce creative coding
- No installation required—just a URL
- Works on classroom devices (phones, Chromebooks, tablets)
- Danish schools actively using laer-klokken

### For Artists
- Publish interactive work without friction
- URL-addressable pieces for portfolios
- Real community of creative coders

### For Everyone
- Computing should be playful and accessible
- The web should be a place for experimentation
- Simple tools can enable deep expression

---

## 🆘 Why Support is Needed

Jeffrey has been building Aesthetic Computer since 2021—solo. The infrastructure costs real money every month, and the development continues full-time.

### What Your Gift Supports
- Server costs (databases, hosting, edge computing)
- Continued development of new pieces and features
- KidLisp.com launch and documentation
- Community growth and moderation tools
- Mobile app development

### The Mission
Keep creative computing open, accessible, and free for everyone—especially kids, students, and anyone curious about making things with code.

---

## 🔗 Links

- Main site: [aesthetic.computer](https://aesthetic.computer)
- KidLisp: [kidlisp.com](https://kidlisp.com)
- Shop: [shop.aesthetic.computer](https://shop.aesthetic.computer)
- GitHub: [github.com/whistlegraph/aesthetic-computer](https://github.com/whistlegraph/aesthetic-computer)
- Jeffrey's site: [jas.life](https://jas.life)
