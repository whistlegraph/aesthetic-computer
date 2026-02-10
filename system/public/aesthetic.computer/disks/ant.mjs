// Ant, 2026.2.10
// A colony of ants foraging for food with pheromone trails.

/* #region 🏁 TODO
  - [x] Ants wander from a nest.
  - [x] Ants leave pheromone trails that fade over time.
  - [x] Ants find food and carry it back to the nest.
  - [x] Other ants follow strong pheromone trails.
  - [x] Tap to place food sources.
  - [x] Ants have a lifespan and eventually die.
  - [x] Food is individual pixels that disappear when taken.
  - [x] Food delivered to pile turns red, ants navigate around it.
#endregion */

const ANT_COUNT = 80;
const PHEROMONE_DECAY = 0.992;
const PHEROMONE_STRENGTH = 200;
const ANT_SPEED = 0.6;
const WANDER_STRENGTH = 0.4;
const SENSE_DISTANCE = 10;
const SENSE_ANGLE = Math.PI / 4;
const ANT_LIFESPAN = 3600;
const SPAWN_RATE = 4;
const FOOD_SCATTER = 8;
const DELIVER_RADIUS = 6; // How close to queen to drop food

// Grid cell values.
const EMPTY = 0;
const FOOD = 1;  // Green, pickupable.
const EATEN = 2; // Red, obstacle.

let ants = [];
let foodTrail;
let homeTrail;
let grid; // 0=empty, 1=food, 2=eaten(red pile)
let gridW, gridH;
let queen;
let simTick = 0;

function boot({ screen, num }) {
  gridW = screen.width;
  gridH = screen.height;
  foodTrail = new Float32Array(gridW * gridH);
  homeTrail = new Float32Array(gridW * gridH);
  grid = new Uint8Array(gridW * gridH);

  queen = { x: gridW >> 1, y: gridH >> 1 };

  for (let i = 0; i < ANT_COUNT; i++) {
    ants.push(spawnAnt(num));
  }

  const margin = 20;
  for (let i = 0; i < 3; i++) {
    scatterFood(
      num.randIntRange(margin, gridW - margin),
      num.randIntRange(margin, gridH - margin),
      40,
      num,
    );
  }
}

function sim({ num }) {
  simTick++;

  for (let i = 0; i < foodTrail.length; i++) {
    foodTrail[i] *= PHEROMONE_DECAY;
    homeTrail[i] *= PHEROMONE_DECAY;
    if (foodTrail[i] < 0.01) foodTrail[i] = 0;
    if (homeTrail[i] < 0.01) homeTrail[i] = 0;
  }

  for (const ant of ants) {
    ant.life++;

    const gi = gridIndex(ant.x | 0, ant.y | 0);
    if (gi !== -1) {
      if (ant.hasFood) {
        foodTrail[gi] = Math.min(foodTrail[gi] + PHEROMONE_STRENGTH, 255);
      } else {
        homeTrail[gi] = Math.min(homeTrail[gi] + PHEROMONE_STRENGTH, 255);
      }
    }

    const trail = ant.hasFood ? homeTrail : foodTrail;
    const sL = sense(ant, trail, -SENSE_ANGLE);
    const sC = sense(ant, trail, 0);
    const sR = sense(ant, trail, SENSE_ANGLE);

    if (sC >= sL && sC >= sR) {
      ant.angle += (num.rand() - 0.5) * WANDER_STRENGTH * 0.5;
    } else if (sL > sR) {
      ant.angle -= WANDER_STRENGTH;
    } else if (sR > sL) {
      ant.angle += WANDER_STRENGTH;
    }

    ant.angle += (num.rand() - 0.5) * WANDER_STRENGTH;

    const aging = ant.life > ANT_LIFESPAN * 0.8 ? 0.5 : 1;
    const nx = ant.x + Math.cos(ant.angle) * ANT_SPEED * aging;
    const ny = ant.y + Math.sin(ant.angle) * ANT_SPEED * aging;

    // Bounce off walls.
    if (nx < 1 || nx >= gridW - 1) {
      ant.angle = Math.PI - ant.angle;
      ant.x = Math.max(1, Math.min(gridW - 2, ant.x));
    } else if (ny < 1 || ny >= gridH - 1) {
      ant.angle = -ant.angle;
      ant.y = Math.max(1, Math.min(gridH - 2, ant.y));
    } else {
      // Bounce off red pile pixels.
      const ni = gridIndex(nx | 0, ny | 0);
      if (ni !== -1 && grid[ni] === EATEN) {
        ant.angle += Math.PI + (num.rand() - 0.5) * 1.0;
      } else {
        ant.x = nx;
        ant.y = ny;
      }
    }

    // Pick up a food pixel underfoot.
    if (!ant.hasFood) {
      const fi = gridIndex(ant.x | 0, ant.y | 0);
      if (fi !== -1 && grid[fi] === FOOD) {
        grid[fi] = EMPTY;
        ant.hasFood = true;
        ant.angle += Math.PI;
      }
    }

    // Deliver food to the pile near the queen.
    if (ant.hasFood) {
      const dx = ant.x - queen.x;
      const dy = ant.y - queen.y;
      if (dx * dx + dy * dy < DELIVER_RADIUS * DELIVER_RADIUS) {
        // Drop as a red eaten pixel at the ant's position.
        const di = gridIndex(ant.x | 0, ant.y | 0);
        if (di !== -1 && grid[di] === EMPTY) {
          grid[di] = EATEN;
        }
        ant.hasFood = false;
        ant.angle += Math.PI;
      }
    }
  }

  ants = ants.filter((a) => a.life < ANT_LIFESPAN);
  if (ants.length < ANT_COUNT && simTick % SPAWN_RATE === 0) {
    ants.push(spawnAnt(num));
  }
}

function paint({ ink, screen }) {
  const { width, height, pixels } = screen;
  const w = gridW;
  const h = gridH;
  const drawW = Math.min(width, w);
  const drawH = Math.min(height, h);

  for (let y = 0; y < drawH; y++) {
    for (let x = 0; x < drawW; x++) {
      const gi = y * w + x;
      const pi = (y * width + x) << 2;
      const cell = grid[gi];

      if (cell === FOOD) {
        pixels[pi] = 140; pixels[pi + 1] = 255; pixels[pi + 2] = 50;
        pixels[pi + 3] = 255;
        continue;
      }
      if (cell === EATEN) {
        pixels[pi] = 180; pixels[pi + 1] = 30; pixels[pi + 2] = 20;
        pixels[pi + 3] = 255;
        continue;
      }

      const ft = foodTrail[gi];
      const ht = homeTrail[gi];
      if (ft > 1 || ht > 1) {
        const bright = (ft > ht ? ft : ht) * 0.7;
        const mix = Math.min(1, bright / 200);
        pixels[pi] = (20 * (1 - mix)) | 0;
        pixels[pi + 1] = ((12 * (1 - mix)) + Math.min(255, ft * 0.6) * mix) | 0;
        pixels[pi + 2] = ((8 * (1 - mix)) + Math.min(255, ht * 0.4) * mix) | 0;
      } else {
        pixels[pi] = 20; pixels[pi + 1] = 12; pixels[pi + 2] = 8;
      }
      pixels[pi + 3] = 255;
    }
  }

  // Draw ants.
  for (const ant of ants) {
    const ax = ant.x | 0;
    const ay = ant.y | 0;
    if (ax < 0 || ax >= width || ay < 0 || ay >= height) continue;
    const pi = (ay * width + ax) << 2;
    const fade = ant.life > ANT_LIFESPAN * 0.8
      ? 1 - (ant.life - ANT_LIFESPAN * 0.8) / (ANT_LIFESPAN * 0.2)
      : 1;
    if (ant.hasFood) {
      pixels[pi] = 0;
      pixels[pi + 1] = (255 * fade) | 0;
      pixels[pi + 2] = (80 * fade) | 0;
    } else {
      pixels[pi] = (220 * fade) | 0;
      pixels[pi + 1] = (180 * fade) | 0;
      pixels[pi + 2] = (140 * fade) | 0;
    }
    pixels[pi + 3] = 255;
  }

  // Queen: bright yellow dot.
  ink(255, 220, 60).plot(queen.x, queen.y);
  ink(255, 220, 60).plot(queen.x + 1, queen.y);
  ink(255, 220, 60).plot(queen.x, queen.y + 1);
  ink(255, 220, 60).plot(queen.x + 1, queen.y + 1);

  ink(180, 180, 180, 100).write("tap to place food", {
    x: 4,
    y: height - 13,
  });
}

function act({ event: e, num }) {
  if (e.is("touch")) {
    scatterFood(e.x | 0, e.y | 0, 40, num);
  }
}

function meta() {
  return {
    title: "Ant",
    desc: "A colony of ants foraging for food with pheromone trails.",
  };
}

export const system = "noprompt";
export { boot, sim, paint, act, meta };

// 📚 Library

function spawnAnt(num) {
  return {
    x: queen.x,
    y: queen.y,
    angle: num.randIntRange(0, 360) * (Math.PI / 180),
    hasFood: false,
    life: 0,
  };
}

function scatterFood(cx, cy, count, num) {
  for (let i = 0; i < count; i++) {
    const a = num.rand() * Math.PI * 2;
    const r = num.rand() * FOOD_SCATTER;
    const fx = (cx + Math.cos(a) * r) | 0;
    const fy = (cy + Math.sin(a) * r) | 0;
    const gi = gridIndex(fx, fy);
    if (gi !== -1 && grid[gi] === EMPTY) grid[gi] = FOOD;
  }
}

function gridIndex(x, y) {
  if (x < 0 || x >= gridW || y < 0 || y >= gridH) return -1;
  return y * gridW + x;
}

function sense(ant, trail, angleOffset) {
  const sx = (ant.x + Math.cos(ant.angle + angleOffset) * SENSE_DISTANCE) | 0;
  const sy = (ant.y + Math.sin(ant.angle + angleOffset) * SENSE_DISTANCE) | 0;
  const i = gridIndex(sx, sy);
  return i === -1 ? 0 : trail[i];
}
