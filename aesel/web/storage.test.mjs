import test from 'node:test';
import assert from 'node:assert/strict';
import { accountStorage } from './storage.mjs';

test('drafts and last-opened piece survive reload, isolated by account without credentials',()=>{
  const values=new Map();
  const backing={getItem:key=>values.get(key)||null,setItem:(key,value)=>values.set(key,value)};
  const a=accountStorage(backing,'auth0|alice');
  a.set('session',JSON.stringify({token:'private-token',handle:'alice',threadID:'piece-1'}));
  a.set('threads','saved draft');
  const reload=accountStorage(backing,'auth0|alice');
  assert.equal(reload.get('threads'),'saved draft');
  assert.deepEqual(JSON.parse(reload.get('session')),{threadID:'piece-1'});
  assert.equal(accountStorage(backing,'auth0|bob').get('threads'),null);
  assert.equal(accountStorage(backing,'auth0|bob').get('session'),null);
  assert(!JSON.stringify([...values]).includes('private-token'));
  assert.throws(()=>accountStorage(backing,''),/account/);
});
