const {test} = require('node:test');
const assert = require('node:assert/strict');
const {EventEmitter} = require('node:events');
const {createUpdater} = require('../updater.cjs');
function fixture(packaged) {
  const calls = [], native = new EventEmitter();
  native.checkForUpdates = async () => { calls.push('check'); return {}; };
  native.quitAndInstall = () => calls.push('install');
  const app = {isPackaged:packaged,relaunch:()=>calls.push('relaunch'),exit:()=>calls.push('exit')};
  const updater = createUpdater({app,notify:()=>{},requestRestart:a=>calls.push('checkpoint:'+a),load:()=>native});
  return {calls,native,updater};
}
test('download requires checkpoint before binary replacement', async () => {
  const {calls,native,updater} = fixture(true);
  await updater.check();
  assert.equal(native.autoInstallOnAppQuit,false);
  native.emit('update-downloaded');
  assert.deepEqual(calls,['check','checkpoint:update']);
  await updater.afterCheckpoint('update');
  assert.equal(calls.at(-1),'install');
});
test('development update checkpoints and relaunches without replacing checkout', async () => {
  const {calls,updater} = fixture(false);
  await updater.check();
  assert.deepEqual(calls,['checkpoint:restart']);
  await updater.afterCheckpoint('restart');
  assert.deepEqual(calls,['checkpoint:restart','relaunch','exit']);
});
test('slash update waits for download before installing', async () => {
  const {calls,native,updater} = fixture(true);
  native.checkForUpdates = async () => ({downloadPromise:Promise.resolve().then(()=>native.emit('update-downloaded'))});
  await updater.afterCheckpoint('update');
  assert.deepEqual(calls,['install']);
});

test('continuation is prepared only after checkpoint for relaunch and binary install',async()=>{
 for(const packaged of [false,true]){
  const calls=[],native=new EventEmitter();
  native.checkForUpdates=async()=>({});native.quitAndInstall=()=>calls.push('install');
  const updater=createUpdater({app:{isPackaged:packaged,relaunch:()=>calls.push('relaunch'),exit:()=>{}},notify:()=>{},requestRestart:()=>calls.push('checkpoint'),prepareRelaunch:()=>calls.push('continuation'),load:()=>native});
  await updater.check();
  assert(!calls.includes('continuation'));
  if(packaged)native.emit('update-downloaded');
  await updater.afterCheckpoint('restart');
  assert.deepEqual(calls.slice(-2),['continuation',packaged?'install':'relaunch']);
 }
});
