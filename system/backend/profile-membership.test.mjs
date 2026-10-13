import test from 'node:test';
import assert from 'node:assert/strict';
import {createMembershipReader} from './profile-membership.mjs';

test('membership uses restricted fields, shares token and pending reads, and expires cached counts', async () => {
  let time = 0;
  const requests = [];
  const read = createMembershipReader({now: () => time, env: {AUTH0_M2M_CLIENT_ID:'id',AUTH0_M2M_SECRET:'secret'}, request: async (url, options) => {
    requests.push({url, options});
    return {ok:true, json:async () => url.endsWith('/oauth/token')
      ? {access_token:'token',expires_in:86400}
      : {created_at:'2023-02-17T23:15:00Z',logins_count:814,email:'never return',user_id:'never return'}};
  }});
  const results = await Promise.all([read('owner'), read('owner'), read('another')]);
  assert.deepEqual(results[0], {registeredAt:'2023-02-17',loginCount:814});
  assert.equal(requests.length, 3);
  assert.ok(requests[1].url.includes('fields=created_at,logins_count&include_fields=true'));
  await read('owner');
  assert.equal(requests.length, 3);
  time = 1800001;
  await read('owner');
  assert.equal(requests.length, 4);
});

test('missing provider credentials or fields never invent membership stats', async () => {
  const absent = createMembershipReader({env:{}, request: () => {throw Error('must not request');}});
  assert.deepEqual(await absent('owner'), {registeredAt:null,loginCount:null});
  const read = createMembershipReader({env:{AUTH0_M2M_CLIENT_ID:'id',AUTH0_M2M_SECRET:'secret'}, request:async url => ({ok:true,json:async () => url.endsWith('/oauth/token') ? {access_token:'t'} : {created_at:'invalid',logins_count:-1}})});
  assert.deepEqual(await read('owner'), {registeredAt:null,loginCount:null});
});

test('provider failures can be retried', async () => {
  let fail = true;
  const read = createMembershipReader({env:{AUTH0_M2M_CLIENT_ID:'id',AUTH0_M2M_SECRET:'secret'}, request:async url => ({ok:!fail,json:async () => url.endsWith('/oauth/token') ? {access_token:'t'} : {}})});
  await assert.rejects(read('owner'), /unavailable/);
  fail = false;
  assert.deepEqual(await read('owner'), {registeredAt:null,loginCount:null});
});
