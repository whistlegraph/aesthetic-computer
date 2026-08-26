// Flower Eater, 2026.8.26
// One button game about a girl who eats flowers — the 2017 concept walking
// again. She strides on her own through a scrolling meadow; the only input
// is the chomp, and timing a bite on a passing bloom is the whole physical
// skill. What she eats moves her MOOD (the 2019 role-painting idea), and
// mood is the score: it tints the paper, bends her stride, and names
// itself after the 2021 whistlegraph's arc — grieving, searching, hungry,
// bright, furious. Art direction traces the 2015 drawings: ink line, big
// eye, flowing hair, striding legs, stars raining down the margin.
// Lineage + lane plan: flower-eater/CONCEPT.md at the repo root.

const { sin, cos, abs, min, max, floor, random, PI } = Math;

// title → play; the title is her standing among what she's about to eat.
let mode = "title";
let titleT = 0;

// The stride is the clock. Everything else phases off how far she has
// walked, so speed changes (mood!) ripple through flowers, stars and hair
// without a second timeline.
let walked = 0;
let stride = 1.6;
let chompAt = -99; // frames since bite, for the jaw arc
let stumbleAt = -99; // frames since a bite of plain air
let frame = 0;

// Mood runs -1 (grieving) .. 1 (furious) and drifts home toward searching.
// Sweet blooms warm it, bitter blooms cool it, a mystery flower jolts it
// somewhere she didn't choose.
let mood = 0;
let eaten = 0;
let lastEaten = null; // she wears the last species in her hair
const moodWord = () =>
  mood < -0.6 ? "grieving"
  : mood < -0.2 ? "searching"
  : mood < 0.3 ? "hungry"
  : mood < 0.7 ? "bright" : "furious";
// The mood's own ink: cold grief blue through warm hunger to a fury red.
const moodInk = () => {
  const warm = (mood + 1) / 2;
  return [90 + warm * 140, 90 - abs(mood) * 30, 140 - warm * 90];
};

// Flowers live in world-x; the meadow scrolls past her fixed screen spot.
let flowers = [];
let nextSprout = 220;
const SPECIES = [
  { name: "sweet", petals: 6, size: 9, moodShift: 0.18,
    petal: [236, 120, 150], center: [255, 200, 90] },
  { name: "bitter", petals: 5, size: 8, moodShift: -0.22,
    petal: [116, 118, 208], center: [70, 64, 120] },
  { name: "mystery", petals: 8, size: 11, moodShift: 0,
    petal: null, center: [40, 34, 48] }, // petal hue drifts, painted live
];
const mysteryPetal = (t) => [
  150 + sin(t * 2.1) * 90, 130 + sin(t * 2.7 + 2) * 90,
  150 + sin(t * 1.7 + 4) * 90];
function sprout(worldX) {
  const roll = random() * 9;
  const species = roll < 5 ? SPECIES[0] : roll < 8 ? SPECIES[1] : SPECIES[2];
  flowers.push({ x: worldX, species, height: 26 + random() * 22,
    sway: random() * PI * 2, eaten: 0 });
}

// Bitten petals fly. World-space, short-lived, species-colored.
let petals = [];
function burst(worldX, y, color, count = 7) {
  for (let index = 0; index < count; index++) {
    const angle = random() * PI * 2;
    const force = 0.6 + random() * 1.6;
    petals.push({ x: worldX, y, vx: cos(angle) * force + 0.4,
      vy: sin(angle) * force - 1.2, life: 30 + random() * 20, color });
  }
}

// Falling margin stars, after the Tall Flower Eater's border rain.
let stars = [];

function boot({ screen }) {
  mode = "title"; titleT = 0;
  walked = 0; mood = 0; eaten = 0; lastEaten = null;
  flowers = []; petals = []; stars = [];
  chompAt = -99; stumbleAt = -99; frame = 0;
  nextSprout = 200;
  for (let index = 0; index < 14; index++)
    stars.push({ x: random(), y: random(), fall: 0.2 + random() * 0.5 });
  for (let ahead = 160; ahead < screen.width + 300; ahead += 90 + random() * 90)
    sprout(ahead);
}

function sim({ screen }) {
  frame += 1;
  for (const star of stars) {
    star.y += star.fall / 240;
    if (star.y > 1) { star.y = -0.05; star.x = random(); }
  }
  if (mode === "title") { titleT += 1 / 60; return; }
  // Mood decides the pace: fury runs, grief drags. It also decays home.
  stride = 1.2 + (mood + 1) * 0.55 + (stumbleAt > frame - 20 ? -0.8 : 0);
  stride = max(0.5, stride);
  walked += stride;
  mood *= 0.9993;
  // The meadow keeps growing just past the right edge of the world.
  if (walked + screen.width + 120 > nextSprout) {
    sprout(nextSprout + screen.width);
    nextSprout += 80 + random() * 130;
  }
  // A bitten flower wilts over a second, then the meadow forgets it.
  for (const flower of flowers) if (flower.eaten) flower.eaten += 1;
  flowers = flowers.filter((flower) =>
    flower.x > walked - 60 && flower.eaten < 60);
  for (const petal of petals) {
    petal.x += petal.vx; petal.y += petal.vy;
    petal.vy += 0.08; petal.life -= 1;
  }
  petals = petals.filter((petal) => petal.life > 0);
}

// The one verb. Any tap, any key: she bites whatever bloom is at her
// mouth, and biting nothing at all costs her the stride for a beat.
function chomp(sound) {
  chompAt = frame;
  const mouthWorld = walked + 34; // just ahead of her face
  const bloom = flowers.find((flower) => !flower.eaten &&
    abs(flower.x - mouthWorld) < 16);
  if (!bloom) {
    stumbleAt = frame;
    sound?.synth({ type: "triangle", tone: 130, duration: 0.14,
      attack: 0.01, decay: 0.9, volume: 0.35 });
    return;
  }
  bloom.eaten = 1;
  eaten += 1;
  const { species } = bloom;
  lastEaten = species;
  burst(bloom.x, 0, species.petal || mysteryPetal(frame / 60));
  if (species.name === "mystery") {
    mood = max(-1, min(1, mood + (random() * 2 - 1) * 1.2));
    sound?.synth({ type: "sine", tone: 880, duration: 0.16,
      attack: 0.01, decay: 0.6, volume: 0.3 });
    sound?.synth({ type: "sine", tone: 1320, duration: 0.22,
      attack: 0.02, decay: 0.6, volume: 0.22 });
  } else {
    mood = max(-1, min(1, mood + species.moodShift));
    // The bite: a short crunch, and a blip whose pitch is her mood.
    sound?.synth({ type: "noise-white", tone: 800, duration: 0.05,
      attack: 0, decay: 0.9, volume: 0.35 });
    sound?.synth({ type: "square", tone: 300 + (mood + 1) * 220,
      duration: 0.09, attack: 0, decay: 0.8, volume: 0.3 });
  }
}

function act({ event: e, sound }) {
  const pressed = e.is("touch") || e.is("keyboard:down:space") ||
    e.is("keyboard:down:enter");
  if (!pressed) return;
  if (mode === "title") {
    mode = "play";
    sound?.synth({ type: "sine", tone: 440, duration: 0.12,
      attack: 0.01, decay: 0.7, volume: 0.3 });
    sound?.synth({ type: "sine", tone: 660, duration: 0.2,
      attack: 0.03, decay: 0.7, volume: 0.25 });
    return;
  }
  chomp(sound);
}

// ── drawing ──────────────────────────────────────────────────────────
const INK = [28, 26, 32];

function paperColor() {
  const warm = (mood + 1) / 2;
  return [
    224 + warm * 26, 214 - abs(mood) * 10 + warm * 4, 200 - warm * 40];
}

function drawStars({ ink, line }, screen) {
  for (const star of stars) {
    const x = floor(star.x * screen.width);
    const y = floor(star.y * (screen.height - 8));
    ink(196, 168, 96);
    line(x - 2, y, x + 2, y); line(x, y - 2, x, y + 2);
    ink(160, 138, 84);
    line(x - 1, y - 1, x + 1, y + 1);
  }
}

function drawFlower({ ink, line, circle }, flower, x, ground) {
  const wilt = flower.eaten ? min(1, flower.eaten / 40) : 0;
  const sway = sin(flower.sway + walked * 0.02) * 3 * (1 - wilt);
  const top = ground - flower.height * (1 - wilt * 0.5);
  ink(74, 112, 62);
  line(x, ground, x + sway, top); // stem
  const { species } = flower;
  if (!flower.eaten) {
    const petal = species.petal || mysteryPetal(frame / 60);
    for (let index = 0; index < species.petals; index++) {
      const angle = (index / species.petals) * PI * 2 + walked * 0.01;
      ink(...petal);
      line(x + sway, top, x + sway + cos(angle) * species.size,
        top + sin(angle) * species.size);
      circle(x + sway + cos(angle) * species.size,
        top + sin(angle) * species.size, 1.6, true);
    }
    ink(...species.center);
    circle(x + sway, top, 2.6, true);
    if (species.name === "mystery") {
      ink(240, 236, 240);
      circle(x + sway - 2, top - 1, 0.8, true);
      circle(x + sway + 2, top - 1, 0.8, true);
      line(x + sway - 2, top + 3, x + sway + 2, top + 3);
    }
  } else {
    ink(130, 126, 122);
    circle(x + sway, top, 2);
  }
}

function drawGirl({ ink, line, circle }, girlX, ground, idle) {
  const phase = idle ? frame * 0.03 : walked * 0.11;
  const pace = idle ? 0 : stride;
  const bob = abs(sin(phase)) * (idle ? 1 : 2);
  const headY = ground - 44 - bob;
  const jaw = max(0, 8 - (frame - chompAt)) / 8;
  const stumble = stumbleAt > frame - 20;
  ink(...INK);
  circle(girlX, headY, 8); // head
  ink(60, 44, 80);
  circle(girlX + 3, headY - 1, 2.5); // the big eye
  ink(...INK);
  circle(girlX + 3.6, headY - 1.4, 1, true); // its pupil
  // mouth: open with the bite
  line(girlX + 6, headY + 3, girlX + 10 + jaw * 5, headY + 3 - jaw * 4);
  line(girlX + 6, headY + 3, girlX + 10 + jaw * 5, headY + 3 + jaw * 3);
  // hair, flowing back harder the faster she goes
  for (let strand = 0; strand < 3; strand++)
    line(girlX - 4, headY - 6 + strand * 3,
      girlX - 14 - pace * 5 - strand * 3,
      headY - 4 + strand * 4 + sin(phase + strand) * 2);
  // she wears the last flower she ate
  if (lastEaten) {
    const worn = lastEaten.petal || mysteryPetal(frame / 60);
    ink(...worn);
    circle(girlX - 5, headY - 7, 2.4, true);
    ink(...(lastEaten.center || INK));
    circle(girlX - 5, headY - 7, 1, true);
  }
  ink(...INK);
  const hipY = ground - 22;
  line(girlX, headY + 8, girlX - (stumble ? 4 : -2), hipY); // torso
  line(girlX - 1, headY + 14, girlX + 12, headY + 12 + sin(phase) * 3); // arm
  const kick = idle ? 3 : sin(phase) * 9;
  line(girlX - (stumble ? 4 : -2), hipY, girlX + kick, ground);
  line(girlX - (stumble ? 4 : -2), hipY, girlX - kick, ground);
}

function paint(api) {
  const { wipe, ink, line, circle, write, screen } = api;
  wipe(...paperColor());
  const ground = floor(screen.height * 0.78);
  const girlX = floor(screen.width * (mode === "title" ? 0.5 : 0.3));

  drawStars(api, screen);

  // Ground: an ink rule with grass ticks in green.
  ink(...INK);
  line(0, ground, screen.width, ground);
  ink(96, 138, 82);
  for (let x = -(floor(walked) % 24); x < screen.width; x += 24)
    line(x, ground + 2, x + 7, ground + 5);

  for (const flower of flowers) {
    const x = floor(flower.x - walked + floor(screen.width * 0.3));
    if (x < -20 || x > screen.width + 20) continue;
    drawFlower(api, flower, x, ground);
  }

  for (const petal of petals) {
    const x = floor(petal.x - walked + floor(screen.width * 0.3));
    ink(...petal.color);
    circle(x, floor(ground - 30 + petal.y), 1.4, true);
  }

  drawGirl(api, girlX, ground, mode === "title");

  if (mode === "title") {
    // The title floats over her head in her own ink, with a paper shadow.
    const title = "FLOWER EATER";
    const size = screen.width > 500 ? 3 : 2;
    const width = title.length * 6 * size;
    const x = floor((screen.width - width) / 2);
    const y = floor(screen.height * 0.22);
    ink(180, 150, 120);
    write(title, { x: x + 2, y: y + 2, size });
    ink(...INK);
    write(title, { x, y, size });
    ink(120, 96, 130);
    write("a girl who eats flowers", { center: "x", y: y + size * 10 + 6 });
    const pulse = 0.55 + (sin(titleT * 3.4) + 1) * 0.22;
    ink(196 * pulse, 120 * pulse, 60 * pulse);
    write("tap to eat", { center: "x", y: floor(screen.height * 0.62) });
    return;
  }

  // HUD lives in the bottom-right corner, out of the runtime's own label,
  // in the mood's ink with a paper shadow.
  const label = `${eaten} · ${moodWord()}`;
  const labelX = screen.width - label.length * 6 - 8;
  const labelY = screen.height - 16;
  ink(200, 190, 172);
  write(label, { x: labelX + 1, y: labelY + 1 });
  ink(...moodInk());
  write(label, { x: labelX, y: labelY });
}

export { boot, sim, act, paint };
