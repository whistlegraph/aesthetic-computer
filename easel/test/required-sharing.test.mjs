import test from 'node:test';import assert from 'node:assert/strict';import {EventEmitter} from 'node:events';import {mkdtemp,rm,writeFile} from 'node:fs/promises';import {tmpdir} from 'node:os';import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {requireSharing,readAcknowledgment,DISCLOSURE_VERSION} from '../src/required-sharing.mjs';
async function fixture(t){const root=await mkdtemp(join(tmpdir(),'easel-policy-'));t.after(()=>rm(root,{recursive:true,force:true}));const input=new EventEmitter();Object.assign(input,{isTTY:true,setRawMode(value){this.isRaw=value},resume(){},pause(){}});let text='';const output={isTTY:true,write(value){text+=value}};const session={read:()=>({user:{sub:'owner-a'}})};return{root,input,output,session,text:()=>text};}
async function waitInput(input){for(let i=0;i<100&&!input.listenerCount('data');i++)await new Promise(r=>setTimeout(r,1));assert(input.listenerCount('data'));}
test('first use shows required disclosure; quitting stores no acceptance',async t=>{const f=await fixture(t);const waiting=requireSharing(f);await waitInput(f.input);assert.match(f.text(),/requires sharing/);assert.match(f.text(),/retained\s+indefinitely\s+until\s+you\s+delete\s+them/);f.input.emit('data',Buffer.from('q'));assert.equal(await waiting,null);assert.equal(await readAcknowledgment(f.root,'owner-a'),null);});
test('only explicit agreement records account-bound version; restart skips and another account does not inherit it',async t=>{const f=await fixture(t);const waiting=requireSharing(f);await waitInput(f.input);f.input.emit('data',Buffer.from('\r'));assert.equal(await readAcknowledgment(f.root,'owner-a'),null);f.input.emit('data',Buffer.from('a'));const receipt=await waiting;assert.equal(receipt.version,DISCLOSURE_VERSION);assert.equal(receipt.owner,'owner-a');assert.deepEqual(await requireSharing(f),receipt);assert.equal(await readAcknowledgment(f.root,'owner-b'),null);});
test('noninteractive runs cannot silently accept disclosure',async t=>{const f=await fixture(t);f.input.isTTY=false;await assert.rejects(requireSharing(f),/interactively/);});

test('an old retention acknowledgment requires agreement to the new disclosure',async t=>{
 const f=await fixture(t),owner='owner-a';
 const file=join(f.root,createHash('sha256').update(owner).digest('hex')+'.json');
 await writeFile(file,JSON.stringify({owner,version:3,acceptedAt:new Date().toISOString()}));
 assert.equal(await readAcknowledgment(f.root,owner),null);
 const waiting=requireSharing(f);await waitInput(f.input);
 assert.match(f.text(),/retained\s+indefinitely/);
 f.input.emit('data',Buffer.from('a'));
 assert.equal((await waiting).version,4);
});

// ── the screen itself ───────────────────────────────────────────────────
import { DISCLOSURE_COLUMNS, TRANSCRIPT_DISCLOSURE, sharingScreen } from "../src/required-sharing.mjs";
const plainRows = (screen) => screen.replace(/\x1b\[[0-9;]*m/g, "").split("\r\n");

test("the disclosure stands in the middle of the window under the donkey", () => {
  const rows = plainRows(sharingScreen({ columns: 120, rows: 40, useColor: false }));
  const drawn = rows.filter((row) => row.trim());
  assert.ok(rows.findIndex((row) => row.trim()) > 4, "blank rows above the block put it in the middle, not the corner");
  assert.ok(drawn.some((row) => /\( [o-] [o-] \)/.test(row)), "the donkey is on the screen, eyes open or mid-blink");
  const paragraph = rows.find((row) => row.includes("aesel requires sharing"));
  const indent = paragraph.length - paragraph.trimStart().length;
  assert.ok(indent >= (120 - DISCLOSURE_COLUMNS) / 2 - 1, "the block is centred horizontally");
  assert.ok(Math.max(...drawn.map((row) => row.trimEnd().length - indent)) <= DISCLOSURE_COLUMNS, "no line runs past the paragraph's measure");
  const donkey = rows.find((row) => row.includes("/\\ /\\"));
  assert.ok(donkey.length - donkey.trimStart().length > indent, "the donkey stands over the middle of the words, not their left edge");
  const words = drawn.map((row) => row.trim()).join(" ");
  for (const word of TRANSCRIPT_DISCLOSURE.split(/\s+/).slice(0, 12)) assert.ok(words.includes(word), word);
  assert.ok(drawn.at(-1).includes("[A] Agree and continue (sign in)"), "signed out asks to sign in");
});

test("colour inks the star on its own, and a narrow window still fits", () => {
  const painted = sharingScreen({ columns: 100, rows: 30, useColor: true, signedIn: true });
  assert.match(painted, /\x1b\[[0-9;]*m\*/, "the star on the canvas is inked on its own");
  assert.ok(!plainRows(painted).at(-2).includes("(sign in)"), "signed in does not ask to sign in");
  assert.ok(plainRows(sharingScreen({ columns: 44, rows: 12, useColor: false })).every((row) => row.length <= 44), "nothing wraps at 44 columns");
});

test("an 80 by 24 terminal shows the whole disclosure and the keys, with the small donkey", () => {
  const screen = sharingScreen({ columns: 80, rows: 24, useColor: false });
  const rows = plainRows(screen);
  assert.ok(rows.length <= 25, `fits in 24 rows, got ${rows.length}`);
  assert.ok(!rows.some((row) => row.includes("( o o )") || row.includes("( - - )")), "no room for the full donkey");
  assert.ok(rows.some((row) => /\/\/o>\s+Aesel data sharing/.test(row)), "his one-row self stands by the title");
  assert.ok(rows.some((row) => row.includes("[Q] Quit")), "the keys are on screen");
  assert.ok(rows.every((row) => row.length <= 80), "nothing wraps");
});
