import './harness-tui-fixture.mjs';
import {appendFileSync, readFileSync} from 'node:fs';
import {BACKENDS} from '../src/backends.mjs';

const log = value => appendFileSync(process.env.AESEL_TEST_LOG, JSON.stringify(value) + '\n');
const Engine = BACKENDS.codex.Engine;
const connect = Engine.prototype.connect;
Engine.prototype.connect = async function () {
  const result = await connect.call(this);
  const at = process.argv.indexOf('--checkpoint');
  log({event: 'restart-connection', pid: process.pid, resume: this.resumeThreadId, entry: process.env.AESEL_STARTUP_ENTRY, execArgv:process.execArgv,
    model: this.model, effort: this.effort, args: process.argv.slice(2),
    sessionId: this.environment.AESEL_SESSION_ID,
    checkpoint: at < 0 ? null : JSON.parse(readFileSync(process.argv[at + 1], 'utf8'))});
  if (this.resumeThreadId) result.thread = {turns: [{items: [
    {id: 'original-prompt', type: 'userMessage', content: [{type: 'text', text: 'startup-once'}]},
  ]}]};
  return result;
};
const startTurn = Engine.prototype.startTurn;
Engine.prototype.startTurn = async function (text) {
  log({event: 'prompt', text});
  if (text === 'reify through control') {
    this.emit('notification', {method: 'turn/started', params: {turn: {id: 'reify-turn'}}});
    log({event: 'reify-response', result: await this.settings({action: 'reify'})});
    this.emit('notification', {method: 'item/agentMessage/delta', params: {itemId: 'reify-final', delta: 'Reify queued after this reply.'}});
    this.emit('notification', {method: 'turn/completed', params: {turn: {status: 'completed'}}});
    return;
  }
  if (text === 'hold turn') {
    this.emit('notification', {method: 'turn/started', params: {turn: {id: 'held-turn'}}});
    setTimeout(() => {
      this.emit('notification', {method: 'turn/completed', params: {turn: {status: 'completed'}}});
      log({event: 'held-turn-complete'});
    }, 1000);
    return;
  }
  return startTurn.call(this, text);
};
