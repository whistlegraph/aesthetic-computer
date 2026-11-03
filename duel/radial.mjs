// Hold-to-build: a ring of phrases that composes an Aesel prompt without a
// keyboard. A controller points with a stick; touch taps a slice.

// The first ring picks a kind of thing, the second the thing itself.
export const CATALOG = {
  animal: ['a cat', 'a dog', 'a bird', 'a fish', 'a frog', 'a horse', 'a dragon', 'a butterfly'],
  vehicle: ['a car', 'a train', 'a boat', 'a rocket', 'a bike', 'a plane', 'a bus', 'a submarine'],
  building: ['a house', 'a tower', 'a castle', 'a bridge', 'a temple', 'a lighthouse', 'a hut', 'a windmill'],
  nature: ['a tree', 'a flower', 'a rock', 'a mountain', 'a cloud', 'a mushroom', 'a cactus', 'a volcano'],
  furniture: ['a chair', 'a table', 'a bed', 'a lamp', 'a sofa', 'a shelf', 'a desk', 'a throne'],
  toy: ['a ball', 'a robot', 'a kite', 'a spinning top', 'a yo-yo', 'blocks', 'a teddy bear', 'a drum'],
  food: ['a cake', 'a pizza', 'a burger', 'an apple', 'a donut', 'an ice cream', 'a banana', 'a sandwich'],
  play: ['a ramp', 'stairs', 'a slide', 'a fountain', 'a swing', 'a trampoline', 'a maze', 'a seesaw'],
  sound: ['a bell', 'a chime', 'a gong', 'a drum', 'a horn', 'a music box', 'a whistle', 'a speaker'],
  // Items you carry: run over to pick up, RB to use, X to drop.
  shoot: ['a pistol', 'a blaster', 'a ray gun', 'a water gun', 'a slingshot', 'a bow', 'a crossbow', 'a cannon'],
  poke: ['a sword', 'a spear', 'a stick', 'a hammer', 'a wand', 'an umbrella', 'a fork', 'a lance'],
  throw: ['a baseball', 'a frisbee', 'a bomb', 'a snowball', 'a boomerang', 'a dart', 'a rock', 'a pie'],
};
export const ITEM_CATEGORIES = ['shoot', 'poke', 'throw'];
// Chalk: build what you drew in the air (hold RT to draw), or wipe it.
CATALOG.chalk = ['a sculpture', 'a creature', 'a building', 'a vehicle', 'a plant', 'a machine', 'a sign', 'erase my chalk'];
export const CATEGORIES = Object.keys(CATALOG);
export const THINGS = CATALOG.play;
export const TRAITS = [
  'humming',
  'big',
  'tiny',
  'spinning',
  'bouncing',
  'red',
  'blue',
  'striped',
  'tall',
];
export const CHANGES = [
  'make it hum',
  'make it sing',
  'make it bigger',
  'make it smaller',
  'make it spin',
  'make it bounce',
  'paint it red',
  'paint it blue',
  'add wheels',
  'add a roof',
];

// Sound things pick how they sound: pitch, character and rhythm.
export const SOUND_TRAITS = ['low', 'high', 'deep', 'bright', 'ringing', 'buzzing', 'pulsing', 'chiming'];

// Animals can move like animals.
export const ANIMAL_TRAITS = ['singing', 'big', 'tiny', 'walking', 'flying', 'red', 'striped', 'fluffy'];

export const HOLD_SECONDS = 0.45;

export class Radial {
  constructor() {
    this.close();
  }
  close() {
    this.open = false;
    this.target = null;
    this.category = null;
    this.picks = [];
    this.selected = -1;
  }
  begin(target = null) {
    this.open = true;
    this.target = target;
    this.category = null;
    this.picks = [];
    this.selected = -1;
  }
  // Editing an object offers changes. A new object first names a thing,
  // then each further pick adds a trait to it.
  get items() {
    if (this.target) return CHANGES.filter((c) => !this.picks.includes(c));
    if (!this.category) return CATEGORIES;
    if (!this.picks.length) return CATALOG[this.category];
    const traits =
      this.category === 'animal' ? ANIMAL_TRAITS : this.category === 'sound' ? SOUND_TRAITS : TRAITS;
    return traits.filter((t) => !this.picks.includes(t));
  }
  // A build from an item category makes something you can carry.
  get chalk() {
    return this.category === 'chalk';
  }
  get item() {
    return ITEM_CATEGORIES.includes(this.category) ? this.category : null;
  }
  get title() {
    if (this.target)
      return `${this.target.code ? '$' + this.target.code : this.target.geometry.name} V${this.target.version}`;
    if (!this.category) return 'BUILD';
    return this.picks.length ? 'ADD A DETAIL' : this.category.toUpperCase();
  }
  // x right, y up; a short stick or tap near the middle selects nothing.
  point(x, y, threshold = 0.5) {
    if (Math.hypot(x, y) < threshold) return (this.selected = -1);
    const n = this.items.length;
    const turn = (Math.atan2(x, y) / (2 * Math.PI) + 1) % 1;
    return (this.selected = Math.round(turn * n) % n);
  }
  pick(index = this.selected) {
    const item = this.items[index];
    if (!item || this.picks.length >= 6) return false;
    if (!this.target && !this.category) {
      this.category = item;
      this.selected = -1;
      return true;
    }
    this.picks.push(item);
    this.selected = -1;
    return true;
  }
  back() {
    if (this.picks.length) this.picks.pop();
    else if (this.category) this.category = null;
    else return false;
    return true;
  }
  text() {
    if (this.target) return this.picks.join(', ');
    if (!this.picks.length) return '';
    const [thing, ...traits] = this.picks;
    const noun = thing.replace(/^an? /, '');
    if (!/^an? /.test(thing)) return [...traits, noun].join(' ');
    if (!traits.length) return thing;
    const lead = /^[aeiou]/.test(traits[0]) ? 'an' : 'a';
    return `${lead} ${traits.join(' ')} ${noun}`;
  }
}

// Slice centres on a unit circle, first slice at the top, clockwise.
export function radialSlots(count) {
  return Array.from({ length: count }, (_, i) => {
    const a = (i / count) * 2 * Math.PI;
    return { x: Math.sin(a), y: Math.cos(a) };
  });
}

// Shared by the renderer and the touch hit test: centre offset up (y up) to
// clear the prompt panel below and the dev strip above.
export function ringGeometry(width, height) {
  return { cy: height * 0.06, r: Math.min(width, height) * 0.25 };
}
