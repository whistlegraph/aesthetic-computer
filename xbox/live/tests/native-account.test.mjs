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
