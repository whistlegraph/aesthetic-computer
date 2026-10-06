import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import { createNativeAccountBridge } from '../native-account.mjs';
import { nativeSource } from '../../tools/oskiewar-native-source.mjs';
function fixture() {
  let time = 1000, reads = 0;
  let state = { status: 'signed-in', handle: 'jeffrey', savedFighter: {
    handle: '@jeffrey', fighter: { valid: true }, validUntil: 999999 } };
  const actions = [];
  const bridge = createNativeAccountBridge({ read: () => { reads++; return state; },
    act: action => { actions.push(action); state = { status: 'signed-out' }; },
    validate: value => { assert.equal(value.valid, true); return { shirt: [1, 2, 3] }; },
    now: () => time });
  return { bridge, actions, get reads() { return reads; }, set state(value) { state = value; },
    advance: (ms = 500) => { time += ms; } };
}
test('native identity restores only its own validated fighter and bounds the lease', () => {
  const f = fixture(), result = f.bridge.refresh();
  assert.equal(result.fighter.handle, '@jeffrey');
  assert.equal(result.fighter.validUntil, 31000);
  f.bridge.refresh(); assert.equal(f.reads, 1, 'host state is polled at most twice per second');
  f.state = { status: 'signed-in', handle: 'other', savedFighter: {
    handle: '@jeffrey', validUntil: 999999, fighter: { valid: true } } };
  f.advance(); assert.equal(f.bridge.refresh().fighter, null);
});
test('expiry, sign-out, invalid results, and host errors cannot retain an appearance', () => {
  const f = fixture(); assert.ok(f.bridge.refresh().fighter);
  f.bridge.action('logout'); assert.deepEqual(f.actions, ['logout']);
  assert.equal(f.bridge.refresh().fighter, null);
  f.state = { status: 'signed-in', handle: 'jeffrey', savedFighter: {
    handle: '@jeffrey', fighter: { valid: false }, validUntil: 999999 } };
  f.advance(); assert.equal(f.bridge.refresh().fighter, null);
  f.state = { status: 'signed-in', handle: 'jeffrey', savedFighter: {
    handle: '@jeffrey', fighter: { valid: true }, validUntil: 1000 } };
  f.advance(); assert.equal(f.bridge.refresh().fighter, null);
  f.bridge.action('delete'); assert.deepEqual(f.actions, ['logout']);
});
test('native bundles expose the same validated model and account bridge without module syntax', () => {
  const context = vm.createContext({});
  vm.runInContext(nativeSource('globalThis.gameLoaded = true;'), context);
  assert.equal(context.gameLoaded, true);
  assert.equal(context.__oskiewarFighterModel.version, 2);
  assert.equal(typeof context.__oskiewarFighterModel.validate, 'function');
  assert.equal(typeof context.__oskiewarCreateNativeAccount, 'function');
});

import { readFileSync } from 'node:fs';
const game = readFileSync(new URL('../oskiewar.js', import.meta.url), 'utf8');
function gameHost() {
  let time = 1000, state = { status: 'signed-out' };
  const calls = [], noop = () => {};
  const context = vm.createContext({ runtime: () => ({ monotonicUs: time * 1000 }),
    capabilities: () => ({ platform: 'xbox-uwp' }), Date: class extends Date { static now() { return time; } },
    wipe: noop, box: noop, line: noop, triangle: noop, triangle3d: noop, write: noop, systemWrite: noop,
    accountState: () => state, accountAction: name => { calls.push(['action', name]); state = { status: 'signed-out' }; },
    telemetry: noop, publishLive: (...args) => calls.push(['publish', ...args]), saveReplay: (...args) => calls.push(['replay', ...args]) });
  vm.runInContext(nativeSource(game) + `
    globalThis.testGame = { syncNativeAccount, syncSignedInFighter, players, parkPeers, receiveParkPeers,
      publishSession, publishVersus, publishSpectator, captureSurvivalRun, generatedAppearance, drawGeneratedRunner,
      get menu() { return freeskateMenu; },
      setup() { configureWorldMap('skatepark','pool'); fightOpponent='freeskate'; shellMode='GAME'; sessionName='test-room'; },
      menuKey(key) { padSnapshots=[{down:key?[key]:[]}]; updateFreeskateMenu(1000000); },
      openMenu() { freeskateMenu={row:4,level:0,previous:[]}; },
      model() { const appearance=generatedAppearance(players[0]); return appearance && __oskiewarFighterModel.build(appearance); }
    };`, context);
  return { context, api: context.testGame, calls, set state(value) { state = value; }, advance() { time += 500; } };
}
const saved = { handle: '@jeffrey', validUntil: 20000, fighter: { version: 1, recipe: 'oskiewar-capsule-fighter-v1',
  hash: 'a'.repeat(64), appearance: { skin:'#debbaa',hair:'#493626',shirt:'#eeeeee',pants:'#224466',shoes:'#111111',
    hairStyle:'long',sleeves:'long',beard:false,glasses:false } } };
test('main native game restores the saved model, keeps it local, and clears it on sign-out', () => {
  const h = gameHost(); h.api.setup();
  h.state = { status:'signed-in',handle:'jeffrey',savedFighter:saved };
  h.api.syncNativeAccount(); h.api.syncSignedInFighter();
  assert.equal(h.api.players[0].name, '@JEFFREY');
  assert.equal(h.context.__oskiewarLocalPractice, true);
  assert.equal(h.api.model().version, 2);
  h.api.publishSession(1000000); h.api.publishVersus(1000000); h.api.publishSpectator(1000000, {target:'test',force:true});
  h.context.__oskiewarCaptureSurvival = true; h.api.captureSurvivalRun(1000000, 'SUMMIT');
  assert.equal(h.calls.length, 0, 'private appearances cannot publish native snapshots or replays');
  h.api.receiveParkPeers({peers:[{id:3,x:0,y:0,z:0,yaw:0}],self:1});
  assert.equal(h.api.parkPeers.size, 0);
  h.state = { status:'signed-out' }; h.advance(); h.api.syncNativeAccount(); h.api.syncSignedInFighter();
  assert.equal(h.api.generatedAppearance(h.api.players[0]), null);
  assert.equal(h.context.__oskiewarLocalPractice, false);
  assert.notEqual(h.api.players[0].name, '@JEFFREY');
});
test('native pause menu starts pairing only on selection and hides logout inside the account panel', () => {
  const h=gameHost(); h.api.setup(); h.api.syncNativeAccount();
  assert.equal(h.calls.length, 0);
  h.api.openMenu(); h.api.menuKey('A');
  assert.equal(h.api.menu.account,true); assert.deepEqual(h.calls,[['action','login']]);
  h.api.menuKey(''); h.api.menuKey('B'); assert.equal(h.api.menu.account,false);
  h.state={status:'signed-in',handle:'jeffrey',savedFighter:saved}; h.advance(); h.api.syncNativeAccount();
  h.api.menuKey(''); h.api.menuKey('A'); assert.equal(h.api.menu.account,true);
  assert.equal(h.calls.length,1,'opening account does not log out');
  h.api.menuKey(''); h.api.menuKey('A'); assert.deepEqual(h.calls,[['action','login'],['action','logout']]);
  h.api.syncNativeAccount(); assert.equal(h.context.__oskiewarFighterAppearance,null);
});
