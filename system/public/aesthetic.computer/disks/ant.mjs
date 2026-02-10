// Ant, 2026.2.10
// A colony of ants foraging for food with pheromone trails.

/* #region 🏁 TODO
  - [x] Ants wander from a nest.
  - [x] Ants leave pheromone trails that fade over time.
  - [x] Ants find food and carry it back to the nest.
  - [x] Other ants follow strong pheromone trails.
  - [x] Click to place food sources.
#endregion */

const ANT_COUNT = 80;
const PHEROMONE_DECAY = 0.992;
const PHEROMONE_STRENGTH = 200;
const ANT_SPEED = 0.6;
const WANDER_STRENGTH = 0.4;
const SENSE_DISTANCE = 10;
const SENSE_ANGLE = Math.PI / 4;
const FOOD_RADIUS = 8;
const NEST_RADIUS = 10;
const FOOD_PER_SOURCE = 40;

let ants = [];
let foodTrail; // Pheromone grid: "food is this way"
let homeTrail; // Pheromone grid: "home is this way"
let gridW, gridH;
let nest;
let foodSources = [];
let foodCollected = 0;

function boot({ screen, num }) {
  gridW = screen.width;
  gridH = screen.height;
  foodTrail = new Float32Array(gridW * gridH);
  homeTrail = new Float32Array(gridW * gridH);

  nest = { x: Math.floor(gridW / 2), y: Math.floor(gridH / 2) };

  // Spawn ants at the nest.
  for (let i = 0; i < ANT_COUNT; i++) {
    ants.push({
      x: nest.x,
      y: nest.y,
      angle: num.randIntRange(0, 360) * (Math.PI / 180),
      hasFood: false,
      life: 0,
    });
  }

  // Place some initial food clusters.
  const margin = 30;
  for (let i = 0; i < 3; i++) {
    foodSources.push({
      x: num.randIntRange(margin, gridW - margin),
      y: num.randIntRange(margin, gridH - margin),
      amount: FOOD_PER_SOURCE,
    });
  }
}

function sim({ num }) {
  // Decay pheromones.
  for (let i = 0; i < foodTrail.length; i++) {
    foodTrail[i] *= PHEROMONE_DECAY;
    homeTrail[i] *= PHEROMONE_DECAY;
    if (foodTrail[i] < 0.01) foodTrail[i] = 0;
    if (homeTrail[i] < 0.01) homeTrail[i] = 0;
  }

  // Update each ant.
  for (const ant of ants) {
    ant.life++;

    // Deposit pheromones.
    const gi = gridIndex(Math.floor(ant.x), Math.floor(ant.y));
    if (gi !== -1) {
      if (ant.hasFood) {
        foodTrail[gi] = Math.min(foodTrail[gi] + PHEROMONE_STRENGTH, 255);
      } else {
        homeTrail[gi] = Math.min(homeTrail[gi] + PHEROMONE_STRENGTH, 255);
      }
    }

    // Sense pheromones ahead (left, center, right).
    const trail = ant.hasFood ? homeTrail : foodTrail;
    const sL = sense(ant, trail, -SENSE_ANGLE);
    const sC = sense(ant, trail, 0);
    const sR = sense(ant, trail, SENSE_ANGLE);

    // Steer toward strongest pheromone.
    if (sC >= sL && sC >= sR) {
      // Go straight — slight random wander.
      ant.angle += (num.random() - 0.5) * WANDER_STRENGTH * 0.5;
    } else if (sL > sR) {
      ant.angle -= WANDER_STRENGTH;
    } else if (sR > sL) {
      ant.angle += WANDER_STRENGTH;
    }

    // Random wander on top.
    ant.angle += (num.random() - 0.5) * WANDER_STRENGTH;

    // Move forward.
    ant.x += Math.cos(ant.angle) * ANT_SPEED;
    ant.y += Math.sin(ant.angle) * ANT_SPEED;

    // Bounce off walls.
    if (ant.x < 1 || ant.x >= gridW - 1) {
      ant.angle = Math.PI - ant.angle;
      ant.x = Math.max(1, Math.min(gridW - 2, ant.x));
    }
    if (ant.y < 1 || ant.y >= gridH - 1) {
      ant.angle = -ant.angle;
      ant.y = Math.max(1, Math.min(gridH - 2, ant.y));
    }

    // Check food pickup.
    if (!ant.hasFood) {
      for (const food of foodSources) {
        if (food.amount <= 0) continue;
        const dx = ant.x - food.x;
        const dy = ant.y - food.y;
        if (dx * dx + dy * dy < FOOD_RADIUS * FOOD_RADIUS) {
          ant.hasFood = true;
          food.amount--;
          ant.angle += Math.PI; // Turn around.
          break;
        }
      }
    }

    // Check nest delivery.
    if (ant.hasFood) {
      const dx = ant.x - nest.x;
      const dy = ant.y - nest.y;
      if (dx * dx + dy * dy < NEST_RADIUS * NEST_RADIUS) {
        ant.hasFood = false;
        foodCollected++;
        ant.angle += Math.PI; // Turn around to forage again.
      }
    }
  }

  // Remove depleted food sources.
  foodSources = foodSources.filter((f) => f.amount > 0);
}

function paint({ wipe, ink, screen }) {
  wipe(20, 12, 8);

  // Draw pheromone trails.
  for (let y = 0; y < gridH; y++) {
    for (let x = 0; x < gridW; x++) {
      const i = y * gridW + x;
      const ft = foodTrail[i];
      const ht = homeTrail[i];
      if (ft > 1 || ht > 1) {
        // Food trail = green, home trail = blue, blended.
        ink(
          0,
          Math.min(255, Math.floor(ft * 0.6)),
          Math.min(255, Math.floor(ht * 0.4)),
          Math.min(200, Math.floor(Math.max(ft, ht) * 0.7)),
        ).plot(x, y);
      }
    }
  }

  // Draw food sources.
  for (const food of foodSources) {
    const brightness = 100 + Math.floor((food.amount / FOOD_PER_SOURCE) * 155);
    ink(brightness, 255, 0, 200).circle(food.x, food.y, FOOD_RADIUS, true);
    ink(200, 255, 100).circle(food.x, food.y, FOOD_RADIUS);
  }

  // Draw nest.
  ink(160, 80, 40, 200).circle(nest.x, nest.y, NEST_RADIUS, true);
  ink(200, 120, 60).circle(nest.x, nest.y, NEST_RADIUS);

  // Draw ants.
  for (const ant of ants) {
    const ax = Math.floor(ant.x);
    const ay = Math.floor(ant.y);
    if (ant.hasFood) {
      ink(0, 255, 80).plot(ax, ay);
    } else {
      ink(220, 180, 140).plot(ax, ay);
    }
  }

  // Draw score.
  ink(255, 200, 100).write(`food: ${foodCollected}`, { x: 4, y: 4 });
  ink(180, 180, 180, 150).write("click to place food", {
    x: 4,
    y: screen.height - 13,
  });
}

function act({ event: e }) {
  if (e.is("touch")) {
    foodSources.push({
      x: Math.floor(e.x),
      y: Math.floor(e.y),
      amount: FOOD_PER_SOURCE,
    });
  }
}

function meta() {
  return {
    title: "Ant",
    desc: "A colony of ants foraging for food with pheromone trails.",
  };
}

export { boot, sim, paint, act, meta };

// 📚 Library

function gridIndex(x, y) {
  if (x < 0 || x >= gridW || y < 0 || y >= gridH) return -1;
  return y * gridW + x;
}

function sense(ant, trail, angleOffset) {
  const sx = Math.floor(ant.x + Math.cos(ant.angle + angleOffset) * SENSE_DISTANCE);
  const sy = Math.floor(ant.y + Math.sin(ant.angle + angleOffset) * SENSE_DISTANCE);
  const i = gridIndex(sx, sy);
  return i === -1 ? 0 : trail[i];
}
