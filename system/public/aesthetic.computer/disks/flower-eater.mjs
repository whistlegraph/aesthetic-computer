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
const moodWord = () =>
  mood < -0.6 ? "grieving"
  : mood < -0.2 ? "searching"
  : mood < 0.3 ? "hungry"
  : mood < 0.7 ? "bright" : "furious";

// Flowers live in world-x; the meadow scrolls past her fixed screen spot.
// Each is a stem with a bloom, and the rare mystery one grows a face.
let flowers = [];
let nextSprout = 220;
const SPECIES = [
  { name: "sweet", petals: 6, size: 9, moodShift: 0.18, weight: 5 },
  { name: "bitter", petals: 5, size: 8, moodShift: -0.22, weight: 3 },
  { name: "mystery", petals: 8, size: 11, moodShift: 0, weight: 1 },
];
function sprout(worldX) {
  const roll = random() * 9;
  const species = roll < 5 ? SPECIES[0] : roll < 8 ? SPECIES[1] : SPECIES[2];
  flowers.push({ x: worldX, species, height: 26 + random() * 22,
    sway: random() * PI * 2, eaten: 0 });
}

// Falling margin stars, after the Tall Flower Eater's border rain.
let stars = [];

function boot({ screen }) {
  walked = 0; mood = 0; eaten = 0; flowers = []; stars = [];
  chompAt = -99; stumbleAt = -99; frame = 0;
  nextSprout = 200;
  for (let index = 0; index < 12; index++)
    stars.push({ x: random(), y: random(), fall: 0.2 + random() * 0.5 });
  for (let ahead = 160; ahead < screen.width + 300; ahead += 90 + random() * 90)
    sprout(ahead);
}

function sim({ screen }) {
  frame += 1;
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
  for (const star of stars) {
    star.y += star.fall / 240;
    if (star.y > 1) { star.y = -0.05; star.x = random(); }
  }
}

// The one verb. Any tap, any key: she bites whatever bloom is at her
// mouth, and biting nothing at all costs her the stride for a beat.
function chomp() {
  chompAt = frame;
  const mouthWorld = walked + 34; // just ahead of her face
  const bloom = flowers.find((flower) => !flower.eaten &&
    abs(flower.x - mouthWorld) < 16);
  if (!bloom) { stumbleAt = frame; return; }
  bloom.eaten = 1;
  eaten += 1;
  const { species } = bloom;
  mood = species.name === "mystery"
    ? max(-1, min(1, mood + (random() * 2 - 1) * 1.2))
    : max(-1, min(1, mood + species.moodShift));
}

function act({ event: e }) {
  if (e.is("touch") || e.is("keyboard:down:space") ||
      e.is("keyboard:down:enter")) chomp();
}

function paint({ wipe, ink, line, circle, box, write, screen }) {
  // Paper tinted by mood: cold grey-blue grief through warm paper to an
  // angry blush. Ink stays ink.
  const warm = (mood + 1) / 2;
  wipe(228 + warm * 20, 222 - abs(mood) * 8 + warm * 6, 216 - warm * 36);
  const inkColor = [28, 26, 32];
  const ground = floor(screen.height * 0.78);
  const girlX = floor(screen.width * 0.3);

  // Margin star rain.
  for (const star of stars) {
    const x = floor(star.x * screen.width);
    const y = floor(star.y * (screen.height - 8));
    ink(90, 86, 96);
    line(x - 2, y, x + 2, y); line(x, y - 2, x, y + 2);
    line(x - 1, y - 1, x + 1, y + 1);
  }

  // Ground: a hand-ruled line with a scribble every few steps.
  ink(...inkColor);
  line(0, ground, screen.width, ground);
  for (let x = -(floor(walked) % 24); x < screen.width; x += 24)
    line(x, ground + 2, x + 7, ground + 5);

  // Flowers scroll by in world space.
  for (const flower of flowers) {
    const x = floor(flower.x - walked + girlX);
    if (x < -20 || x > screen.width + 20) continue;
    const wilt = flower.eaten ? min(1, flower.eaten / 40) : 0;
    const sway = sin(flower.sway + walked * 0.02) * 3 * (1 - wilt);
    const top = ground - flower.height * (1 - wilt * 0.5);
    ink(...inkColor);
    line(x, ground, x + sway, top);
    const { species } = flower;
    if (!flower.eaten) {
      for (let petal = 0; petal < species.petals; petal++) {
        const angle = (petal / species.petals) * PI * 2 + walked * 0.01;
        ink(...inkColor);
        line(x + sway, top, x + sway + cos(angle) * species.size,
          top + sin(angle) * species.size);
      }
      if (species.name === "mystery") {
        ink(...inkColor);
        circle(x + sway - 2, top - 1, 1);
        circle(x + sway + 2, top - 1, 1);
        line(x + sway - 2, top + 3, x + sway + 2, top + 3);
      }
    } else {
      ink(120, 116, 122); // petals gone: a bare, greying crown
      circle(x + sway, top, 2);
    }
  }

  // The girl: head, one big eye, flowing hair, leaning torso, scissor
  // legs phased off the walk, one arm reaching for what's next.
  const phase = walked * 0.11;
  const bob = abs(sin(phase)) * 2;
  const headY = ground - 44 - bob;
  const jaw = max(0, 8 - (frame - chompAt)) / 8; // bite arc decays fast
  const stumble = stumbleAt > frame - 20;
  ink(...inkColor);
  circle(girlX, headY, 8); // head
  circle(girlX + 3, headY - 1, 2.5); // the big eye
  ink(...inkColor);
  // mouth: open with the bite
  line(girlX + 6, headY + 3, girlX + 10 + jaw * 5, headY + 3 - jaw * 4);
  line(girlX + 6, headY + 3, girlX + 10 + jaw * 5, headY + 3 + jaw * 3);
  // hair, flowing back harder the faster she goes
  for (let strand = 0; strand < 3; strand++)
    line(girlX - 4, headY - 6 + strand * 3,
      girlX - 14 - stride * 5 - strand * 3,
      headY - 4 + strand * 4 + sin(phase + strand) * 2);
  // torso leans into the walk (and buckles on a stumble)
  const hipY = ground - 22;
  line(girlX, headY + 8, girlX - (stumble ? 4 : -2), hipY);
  // reaching arm
  line(girlX - 1, headY + 14, girlX + 12, headY + 12 + sin(phase) * 3);
  // scissor legs
  const kick = sin(phase) * 9;
  line(girlX - (stumble ? 4 : -2), hipY, girlX + kick, ground);
  line(girlX - (stumble ? 4 : -2), hipY, girlX - kick, ground);

  // Two quiet words of HUD; the meadow speaks otherwise.
  ink(...inkColor);
  write(`${eaten} · ${moodWord()}`, { x: 6, y: 6 });
}

export { boot, sim, act, paint };
