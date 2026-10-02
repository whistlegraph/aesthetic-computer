import './harness-tui-fixture.mjs';
import {appendFileSync} from 'node:fs';
import {FrameDiff} from '../src/frame-diff.mjs';
import {BACKENDS} from '../src/backends.mjs';

const log=value=>appendFileSync(process.env.AESEL_TEST_LOG,JSON.stringify(value)+'\n');
let chunk=0;
process.stdin.prependListener('data',()=>log({event:'input',chunk:++chunk}));
const update=FrameDiff.prototype.update;
FrameDiff.prototype.update=function(...args){
  log({event:'paint',chunk});
  return update.apply(this,args);
};
const startTurn=BACKENDS.codex.Engine.prototype.startTurn;
BACKENDS.codex.Engine.prototype.startTurn=function(text){
  log({event:'prompt',text});
  return startTurn.call(this,text);
};
