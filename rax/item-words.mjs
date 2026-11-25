// 🔫 What kind of item a thing is, from its words: 'shoot', 'poke', 'throw'
// or null. Shared by the builders (a typed build) and the store (a thing
// placed before typed items existed, named by its prompt or its own name).
export const ITEM_KINDS = ['shoot', 'poke', 'throw'];
// A typed build is an item when its main noun is one: "a big gun" can be
// carried, "a house with a cannon on top" is a house. Only the words before
// the first "with", "on", "that"… are read. The ring's own kind wins.
const ITEM_WORDS = {
  shoot: /\b(gun|pistol|blaster|ray ?gun|water ?gun|rifle|shotgun|revolver|uzi|musket|bazooka|launcher|slingshot|crossbow|bow(?! ?tie)|cannon|laser)s?\b/i,
  poke: /\b(sword|spear|hammer|axe|wand|lance|dagger|knife|katana|mace|trident|scythe|pickaxe|bat|club|staff|saber|sabre)s?\b/i,
  throw: /\b(ball|baseball|football|frisbee|bomb|grenade|dynamite|snowball|boomerang|dart|shuriken|pie|javelin)s?\b/i,
};
export function itemOf(prompt = '') {
  const head = String(prompt).split(/\b(?:with|on|that|which|holding|made of|in|under|over|next to|beside|and)\b/i)[0];
  return ITEM_KINDS.find((kind) => ITEM_WORDS[kind].test(head)) || null;
}

// A placed thing with no kind: its first prompt, or else its own name
// (a $code called in has only the code as its prompt; "; Pistol" names it).
export function itemOfObject(first = '', name = '') {
  return itemOf(first) || itemOf(name);
}

// 🎯 A thing built to be hit: it stands where it was built, takes every
// round, poke, throw and blast, and the page pops how much (match.mjs strike).
const TARGET_WORDS = /\b(scarecrows?|dumm(y|ies)|targets?|zombies?|bullseyes?|punching ?bags?|mannequins?|skeletons?|ghouls?|undead|training ?dumm(y|ies))\b/i;
export function targetOf(prompt = '') {
  const head = String(prompt).split(/\b(?:with|on|that|which|holding|made of|in|under|over|next to|beside|and)\b/i)[0];
  return TARGET_WORDS.test(head);
}
export function targetOfObject(first = '', name = '') {
  return targetOf(first) || targetOf(name);
}
