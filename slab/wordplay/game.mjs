const bank = [
  ['Closest in meaning to “fleeting”?','brief','distant','fragile','frequent'],
  ['Opposite of “scarce”?','abundant','valuable','hidden','ordinary'],
  ['Which word means to make something clearer?','clarify','conceal','complicate','compare'],
  ['Closest in meaning to “reluctant”?','unwilling','careless','uncertain','unnoticed'],
  ['Opposite of “rigid”?','flexible','solid','straight','narrow'],
  ['Which word describes a person who gives generously?','benevolent','competitive','meticulous','reserved'],
  ['Closest in meaning to “candid”?','frank','polite','cheerful','casual'],
  ['Opposite of “expand”?','contract','extend','enlarge','expose'],
  ['Which word means to postpone something?','defer','deter','deduce','define'],
  ['Closest in meaning to “meticulous”?','thorough','nervous','brilliant','stubborn'],
  ['Opposite of “temporary”?','permanent','recent','fragile','frequent'],
  ['Which word means to reduce the severity of something?','mitigate','imitate','magnify','migrate'],
  ['Closest in meaning to “tranquil”?','peaceful','empty','distant','silent'],
  ['Opposite of “conceal”?','reveal','repair','repeat','retain'],
  ['Which word means able to recover after difficulty?','resilient','resistant','restless','reticent'],
  ['Closest in meaning to “obsolete”?','outdated','broken','unusual','forgotten'],
  ['Opposite of “hostile”?','friendly','fearful','forceful','familiar'],
  ['Which word means to examine very carefully?','scrutinize','summarize','surmise','symbolize'],
  ['Closest in meaning to “vivid”?','striking','faint','simple','plausible'],
  ['Opposite of “diminish”?','increase','vanish','finish','divide'],
  ['Which word describes something that can be understood in multiple ways?','ambiguous','accurate','apparent','absolute'],
  ['Closest in meaning to “prudent”?','cautious','proud','prompt','patient'],
  ['Opposite of “artificial”?','natural','original','simple','useful'],
  ['Which word means to arrive at a conclusion from evidence?','infer','invent','insist','ignore'],
  ['Closest in meaning to “elated”?','delighted','relieved','surprised','excited'],
  ['Opposite of “timid”?','bold','kind','quiet','calm'],
  ['Which word means an apparent contradiction that may still be true?','paradox','analogy','summary','metaphor'],
  ['Closest in meaning to “concise”?','succinct','precise','simple','clear'],
  ['Opposite of “chaotic”?','orderly','silent','vacant','formal'],
  ['Which word describes a sound repeated by reflection?','echo','rhythm','chord','whisper'],
  ['Closest in meaning to “novice”?','beginner','visitor','student','stranger'],
  ['Which word means to prove a claim false?','refute','refuse','revise','restate'],
];
function shuffle(items) {
  const result = [...items];
  for (let i = result.length - 1; i > 0; i--) {
    const value = crypto.getRandomValues(new Uint32Array(1))[0];
    const j = Math.floor(value / 2 ** 32 * (i + 1));
    [result[i], result[j]] = [result[j], result[i]];
  }
  return result;
}
const el = id => document.getElementById(id);
let deck, round, score, started, answered;
function start() {
  deck = shuffle(bank).slice(0, 8); round = 0; score = 0;
  el('history').replaceChildren(); el('again').hidden = true;
  el('score').textContent = 'Score: 0 / 0'; show();
}
function show() {
  answered = false;
  el('progress').textContent = `Round ${round + 1} of ${deck.length}`;
  el('clue').textContent = deck[round][0];
  el('feedback').textContent = ''; el('next').hidden = true;
  el('choices').replaceChildren(...shuffle(deck[round].slice(1)).map(word => {
    const button = document.createElement('button'); button.textContent = word;
    button.onclick = () => answer(word, button); return button;
  }));
  started = performance.now();
}
function answer(word, button) {
  if (answered) return;
  answered = true;
  const correct = word === deck[round][1];
  if (correct) score++;
  const seconds = ((performance.now() - started) / 1000).toFixed(2);
  for (const b of el('choices').children) {
    b.disabled = true;
    if (b.textContent === deck[round][1]) b.classList.add('correct');
  }
  if (!correct) button.classList.add('wrong');
  el('score').textContent = `Score: ${score} / ${round + 1}`;
  el('feedback').textContent = correct ? `Correct · ${seconds}s` : `The answer is ${deck[round][1]} · ${seconds}s`;
  const item = document.createElement('li');
  item.textContent = `${round + 1}. ${word} ${correct ? '✓' : `→ ${deck[round][1]}`} · ${seconds}s`;
  el('history').append(item);
  if (round + 1 === deck.length) {
    el('progress').textContent = 'Complete'; el('again').hidden = false;
  } else el('next').hidden = false;
}
el('next').onclick = () => { round++; show(); };
el('again').onclick = start;
document.addEventListener('keydown', event => {
  if (event.altKey || event.metaKey || event.ctrlKey || event.repeat) return;
  if (/^[1-4]$/.test(event.key)) el('choices').children[Number(event.key)-1]?.click();
});
start();
