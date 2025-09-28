# Aesthetic Computer: A Creative Platform
**El Cid Lecture - 45 Minutes**
*Jeffrey Alan Scudder*

---

## What is Aesthetic Computer?

Aesthetic Computer is a place to make art with code. You type commands into a prompt, and things happen. Sometimes you paint. Sometimes you program. Sometimes you just explore what's there.

I started building it in 2021 because I wanted to make creative coding more accessible. No downloads, no setup - just open a browser and start making.

---

## The Basic Idea

You go to `aesthetic.computer` and see a prompt. Type `paint` and you get a canvas. Type `notepat` and you get a musical instrument. Type anything else and you might discover something new.

Everything runs in your browser. Your phone, laptop, tablet - doesn't matter. The same tools work everywhere.

---

## What People Actually Make

Looking at the database, people have created:
- 2,778 paintings 
- 2,723 programs in KidLisp (our programming language)
- 212 custom tools and experiences
- 16,197 chat messages (people like to talk while they create)

---

## The Technical Bits

### How Code Becomes Art
```javascript
// A simple piece that draws when you touch the screen
function paint({ wipe, ink, pen }) {
  wipe("blue");
  ink("white");
  pen(64); // thick brush
}

function act({ event, sound }) {
  if (event.is("touch")) {
    // Draw where you touched
    sound.synth("c"); // play a note too
  }
}
```

Each "piece" is just a JavaScript file. Save it, run it, share it. The system handles the rest.

### KidLisp: Programming as Play
```lisp
(wipe gray)
(ink red) 
(write (* 2 3 4))
```

KidLisp is our attempt at making programming feel immediate. Type an expression, see it happen. No compilation, no ceremony.

---

## Some Things People Built

**notepat** - A tone matrix that went viral on Hacker News. Grid of buttons, touch them to make melodies.

**baktok** - Records your voice and plays it backwards. Surprisingly useful for pronunciation practice.

**wand** - Draw in VR space. Works with WebXR headsets or just mouse/touch.

**tracker** - Serious music composition tool with 12-tone support.

**tape** - Records whatever you're doing and exports to GIF/MP4. Frame-accurate.

---

## Real Classroom Use

Last year I taught UCLA students using VS Code with an Aesthetic Computer extension. They could write code in their editor and see it run instantly in a browser panel.

The students made pieces like `ucla-1` (basic graphics) through `ucla-7` (bouncing balls). These pieces now serve as examples for other learners.

Having real students use your tools teaches you things no amount of solo development can.

---

## How Jeffrey Works

I develop this every day on:
- Intel laptop (16GB RAM, nothing fancy)
- Fedora Linux in a Docker container
- Fish shell, Emacs, VS Code
- The same VS Code extension my students use

The platform runs on:
- Netlify (main site, uses Deno)
- Google Cloud (chat system, Node.js)
- MongoDB (user data, @handles)
- Auth0 (login stuff)

Everything auto-deploys when I push to git. Simple.

---

## Real Numbers

As of today, the platform has:
- 2,576 people with @handles
- 1,919 verified email addresses (people stick around)
- 5,501 total creations
- Daily chat activity (someone's always making something)

Not huge numbers, but genuine engagement. Quality over scale.

---

## The Social Bit

Everyone gets an @handle. Your creations link to your profile. There's a chat where people share techniques and debug problems together.

The community formed naturally. I built tools for making things; people started helping each other use them.

Some active folks: @ida, @georgica, @sage, @yearoftheocean, @tina, @dollmath. Plus me (@jeffrey).

---

## Code History

Started December 2022. 3,784 commits so far:
- 2022: 47 commits (getting started)
- 2023: 1,724 commits (core platform)
- 2024: 1,426 commits (UCLA class, VS Code extension)  
- 2025: 587 commits (KidLisp language)

The git history tells the story of what mattered when.

---

## Why This Matters

Creative coding shouldn't require a computer science degree. You should be able to have an idea and express it quickly, whether you're 8 or 80.

Most creative tools either assume you're a complete beginner or an expert. There's not much in between. Aesthetic Computer tries to grow with you.

---

## User Authentication & Data

We use Auth0 for login (aesthetic.us.auth0.com). MongoDB stores everything else - your @handle, your pieces, chat messages, paintings.

Cross-tenant system works on both aesthetic.computer and sotce.net (my other project).

The database shows people actually use this stuff. 74.5% email verification rate means people care enough to confirm their accounts.

---

## Tech Architecture (The Real Stuff)

**Client side**: Vanilla JavaScript + WebGL in browser. No frameworks, just web standards.

**Server side**: 
- Netlify Functions (Deno) for main platform
- Google Cloud (Node.js nanos unikernels) for chat
- JamSocket for real-time collaboration
- MongoDB Atlas for data
- Digital Ocean Spaces for assets

Auth0 handles authentication. MongoDB handles everything else. Simple division of labor.

---

## Demo Time

Let's see what happens when we...
- Open aesthetic.computer
- Try some basic commands  
- Look at what others have made
- Maybe write some code together

---

## Questions?

*[Leave time for discussion and hands-on exploration]*