# ABC123: Alphabet & Numbers Learning System

*A Wikipedia-like modular piece system for early learners (age 0 → upwards)*

## 🎯 Vision

Create 36 individual pieces (`a.mjs` through `z.mjs` + `0.mjs` through `9.mjs`) that serve as:
1. **Entry points** for babies/toddlers who type random letters
2. **Musical gateways** linking each key to notepat's QWERTY tonal system
3. **Cultural artifacts** that can diverge over time like Wikipedia pages
4. **Speak-and-spell foundation** building toward notepat and KidLisp

## 🧒 Current Baby-Friendly Infrastructure

### Existing Pieces
- **notepat** - Full 2-octave QWERTY keyboard instrument
- **clock** - Melody playback with notepat keyboard notation
- **bleep** - Simple colored boxes that make tones (needs QWERTY mapping)
- **song** - Button-based tone player
- **kid.mjs** - Curious kid chatbot character
- **textfence** - Scene-based speech synthesis ("Baby, no ego, just interaction")
- **sing** - AI generates notes from song titles

### Notepat QWERTY Mapping (The Musical Foundation)

```
First Octave (Base):           Second Octave (+1):
c  → c                         +c  → h
c# → v                         +c# → t
d  → d                         +d  → i
d# → s                         +d# → y
e  → e                         +e  → j
f  → f                         +f  → k
f# → w                         +f# → u
g  → g                         +g  → l
g# → r                         +g# → o
a  → a                         +a  → m
a# → q                         +a# → p
b  → b                         +b  → n
```

**Key insight**: Letters `a, b, c, d, e, f, g` are musical notes!
Letters `h, i, j, k, l, m, n` are the NEXT octave!
`q, r, s, t, u, v, w, o, p, y` are sharps!

## 📚 Architecture

### Shared Library: `lib/abc123.mjs`

```javascript
// Central coordination library for all letter/number pieces
export const ABC123 = {
  // Musical mapping (from notepat)
  letterToNote: {
    a: 'a', b: 'b', c: 'c', d: 'd', e: 'e', f: 'f', g: 'g',  // Natural notes
    h: '+c', i: '+d', j: '+e', k: '+f', l: '+g', m: '+a', n: '+b', // 2nd octave
    o: '+g#', p: '+a#', q: 'a#', r: 'g#', s: 'd#', t: '+c#',
    u: '+f#', v: 'c#', w: 'f#', x: null, y: '+d#', z: null
  },
  
  // Visual theming per letter (culturally divergent)
  themes: {
    a: { color: [128, 0, 128], emoji: '🍎', words: ['apple', 'ant', 'airplane'] },
    // ... each letter gets its own theme that can grow
  },
  
  // Number associations (0-9)
  numberThemes: {
    0: { color: [0, 0, 0], shape: 'void', words: ['zero', 'nothing', 'empty'] },
    1: { color: [255, 0, 0], shape: 'circle', words: ['one', 'first', 'solo'] },
    // ... through 9
  },
  
  // Shared utilities
  playLetterSound(letter, sound) { /* plays the notepat tone */ },
  drawLetterDisplay(letter, $) { /* consistent visual treatment */ },
  speakLetter(letter, speak) { /* text-to-speech the letter */ },
  getRandomWord(letter) { /* returns a word starting with that letter */ },
};
```

### Individual Piece Structure: `a.mjs` Example

```javascript
// a.mjs - The letter A
// 🍎 Apple, Ant, Airplane... and the musical note A!

import { ABC123 } from '../lib/abc123.mjs';

const LETTER = 'a';
const theme = ABC123.themes[LETTER];

function boot({ wipe, sound }) {
  // Play the note when entering
  ABC123.playLetterSound(LETTER, sound);
}

function paint({ wipe, ink, screen, write }) {
  wipe(theme.color);
  
  // 🔤 GIANT FULLSCREEN LETTER - fills 80% of smallest dimension
  const letterSize = Math.min(screen.width, screen.height) * 0.8;
  
  // Main letter - massive, centered, impossible to miss
  ink('white').write(LETTER.toUpperCase(), { 
    center: 'xy',
    size: letterSize,
  });
  
  // Subtle shadow for depth (offset by 2% of size)
  const shadowOffset = letterSize * 0.02;
  ink(0, 0, 0, 80).write(LETTER.toUpperCase(), {
    x: screen.width / 2 - letterSize / 2 + shadowOffset,
    y: screen.height / 2 - letterSize / 2 + shadowOffset,
    size: letterSize,
  });
  
  // Word + emoji at bottom (responsive font size)
  const wordSize = Math.max(16, screen.height * 0.06);
  ink('yellow').write(theme.emoji + ' ' + ABC123.getRandomWord(LETTER), {
    center: 'x',
    y: screen.height - wordSize * 2,
    size: wordSize,
  });
}

function act({ event: e, sound, jump }) {
  // Tap anywhere = replay sound + show animation
  if (e.is('touch')) {
    ABC123.playLetterSound(LETTER, sound);
  }
  
  // Any letter key jumps to that letter's piece
  if (e.is('keyboard:down') && /^[a-z]$/i.test(e.key)) {
    jump(e.key.toLowerCase());
  }
  
  // Number keys jump to number pieces
  if (e.is('keyboard:down') && /^[0-9]$/.test(e.key)) {
    jump(e.key);
  }
}

function meta() {
  return { 
    title: 'A', 
    desc: 'The letter A - 🍎 Apple, Ant, and the musical note A!' 
  };
}

export { boot, paint, act, meta };
```

## 🎹 QWERTY → Notepat Learning Path

### Stage 1: Single Letters (Age 0-2)
- Baby presses 'A' → hears note A, sees apple 🍎
- Each letter has a distinct color, sound, word
- Builds letter-sound association

### Stage 2: Musical Discovery (Age 2-4)
- Notice that A-B-C-D-E-F-G are special (musical notes!)
- H-I-J-K-L-M-N are "higher" versions
- Other letters make "in-between" sounds (sharps)

### Stage 3: Notepat Transition (Age 4+)
- Graduate from single letters to `notepat`
- Already familiar with QWERTY layout from letter pieces
- Natural understanding of two-octave keyboard

### Stage 4: Clock/Melody (Age 5+)
- Use `clock` piece with notepat notation
- Type melodies using familiar letters: `cdefgab`
- Add rhythm, duration, waveforms

### Stage 5: KidLisp (Age 6+)
- Move to `(ink "red") (box 10 10 50 50)`
- Musical concepts translate to programming
- `(sound "a")` plays the same note they learned as babies

## 🔢 Number Pieces (0-9)

Numbers follow a parallel structure:

```javascript
// 1.mjs - The number 1
const NUMBER = 1;
const theme = ABC123.numberThemes[NUMBER];

// Visual: One circle, one dot, one anything
// Sound: A single beat, or the first note of a scale
// Word: "one", "first", "solo", "single"
```

**Number-to-Music Mapping:**
- 0 = Rest/Silence (the pause between notes!)
- 1 = C (root)
- 2 = D (second)
- 3 = E (third)
- 4 = F (fourth)
- 5 = G (fifth) 
- 6 = A (sixth)
- 7 = B (seventh)
- 8 = High C (octave!)
- 9 = High D (ninth - reaching beyond!)

## 🌐 Cultural Divergence (Wikipedia Model)

Each letter piece can grow independently:
- `a.mjs` might add apple-picking mini-game
- `b.mjs` might add bouncing ball physics
- `c.mjs` might focus on music theory (C is the root!)
- `x.mjs` might become an X-ray exploration piece
- `z.mjs` might have sleep/zzz themes

**Guidelines for growth:**
1. Keep the core experience (letter display, sound, word)
2. Add interactive elements that teach the letter
3. Link to related pieces (next/prev letter, rhymes)
4. Maintain consistent navigation (type any letter to jump)

## 📁 File Structure

```
system/public/aesthetic.computer/
├── lib/
│   └── abc123.mjs          # Shared library
├── disks/
│   ├── a.mjs               # Letter A
│   ├── b.mjs               # Letter B
│   ├── c.mjs               # Letter C
│   ├── ...                 # d-z
│   ├── z.mjs               # Letter Z
│   ├── 0.mjs               # Number 0
│   ├── 1.mjs               # Number 1
│   ├── 2.mjs               # Number 2
│   ├── ...                 # 3-8
│   └── 9.mjs               # Number 9
```

## 🛠️ Implementation Plan

### Phase 1: Foundation
- [ ] Create `lib/abc123.mjs` shared library
- [ ] Implement musical note mapping from notepat
- [ ] Create color/emoji/word themes for all letters

### Phase 2: Letter Pieces (A-G Musical)
- [ ] `a.mjs` - Apple 🍎 + Note A
- [ ] `b.mjs` - Ball ⚽ + Note B  
- [ ] `c.mjs` - Cat 🐱 + Note C (THE ROOT!)
- [ ] `d.mjs` - Dog 🐕 + Note D
- [ ] `e.mjs` - Elephant 🐘 + Note E
- [ ] `f.mjs` - Fish 🐟 + Note F
- [ ] `g.mjs` - Grapes 🍇 + Note G

### Phase 3: Second Octave Letters (H-N)
- [ ] `h.mjs` - House 🏠 + High C
- [ ] `i.mjs` - Ice cream 🍦 + High D
- [ ] `j.mjs` - Jellyfish 🎐 + High E
- [ ] `k.mjs` - Kite 🪁 + High F
- [ ] `l.mjs` - Lion 🦁 + High G
- [ ] `m.mjs` - Moon 🌙 + High A
- [ ] `n.mjs` - Nest 🪹 + High B

### Phase 4: Sharp Letters (Q,R,S,T,U,V,W,O,P,Y)
- [ ] These make "in-between" sounds
- [ ] Themed around the sharp/mysterious quality
- [ ] `q.mjs` - Queen 👑 + A# (mysterious!)
- [ ] `v.mjs` - Violin 🎻 + C# (musical reference!)

### Phase 5: Non-Musical Letters (X, Z)
- [ ] `x.mjs` - X-ray ☠️ (no note, percussion sound?)
- [ ] `z.mjs` - Zebra 🦓 (no note, sleep/snore sound?)

### Phase 6: Numbers (0-9)
- [ ] `0.mjs` - Zero/Nothing 🕳️ (silence, rest, empty)
- [ ] `1.mjs` - One ☝️ + C (root)
- [ ] `2.mjs` - Two ✌️ + D (second)
- [ ] `3.mjs` - Three 🔺 + E (third)
- [ ] `4.mjs` - Four 🍀 + F (fourth)
- [ ] `5.mjs` - Five 🖐️ + G (fifth)
- [ ] `6.mjs` - Six 🎲 + A (sixth)
- [ ] `7.mjs` - Seven 🌈 + B (seventh)
- [ ] `8.mjs` - Eight 🎱 + High C (octave!)
- [ ] `9.mjs` - Nine 🎳 + High D (ninth)

### Phase 7: Integration
- [ ] Add navigation between pieces
- [ ] Create `abc` launcher piece (alphabet overview)
- [ ] Create `123` launcher piece (numbers overview)
- [ ] Link from homepage for baby-friendly entry

## 🎨 Visual Design Principles

1. **FULLSCREEN LETTERFORMS** - Letter fills 80% of screen, visible from across the room
2. **Responsive scaling** - Works on phone, tablet, TV, projector - always fills the space
3. **High contrast colors** - White on bold color backgrounds, easy for developing eyes
4. **No UI clutter** - Just the letter, nothing else to distract
5. **Simple animations** - Gentle pulse/bounce on tap, not overwhelming
6. **Touch-anywhere interaction** - The whole screen is a button, no fine motor skills needed
7. **Instant audio feedback** - Every touch plays the letter's sound
8. **Portrait + Landscape** - Adapts to any orientation seamlessly

### Responsive Sizing Formula
```javascript
// Letter size = 80% of smallest screen dimension
const letterSize = Math.min(screen.width, screen.height) * 0.8;

// Word size = 6% of screen height (min 16px)
const wordSize = Math.max(16, screen.height * 0.06);

// Padding from edges = 10% of screen
const padding = Math.min(screen.width, screen.height) * 0.1;
```

## 🔊 Audio Design Principles

1. **Pure tones** - Clean sine/triangle waves for letters
2. **Consistent volume** - No sudden loud sounds
3. **Natural decay** - Notes fade out gently
4. **Speech synthesis** - Pronounce the letter and word
5. **Link to notepat** - Use exact same frequencies

## 📊 Success Metrics

- Baby presses random key → lands on educational piece (not 404)
- All 26 letters + 10 digits (0-9) have dedicated experiences
- Each piece connects to the broader musical/programming system
- Clear learning path from ABC → notepat → KidLisp

---

*Created: 2026.01.03*
*Status: Planning*
*Related: notepat.mjs, clock.mjs, bleep.mjs, kid.mjs, KidLisp*
