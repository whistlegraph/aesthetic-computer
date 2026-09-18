import test from 'node:test';
import assert from 'node:assert/strict';
import {createCredits} from '../phone/credits.mjs';

test('phone allowance adds daily and purchased braincells without persisting credentials', async () => {
  const events = [];
  const credits = createCredits({token:()=> 'account-token', emit:event=>events.push(event), site:'https://example.test',
    fetch:async (url, options) => {
      assert.equal(url, 'https://example.test/api/easel-credits');
      assert.equal(options.headers.Authorization, 'Bearer account-token');
      return Response.json({remaining:1200,purchased:3000});
    }});
  await credits.refresh();
  assert.deepEqual(events, [{type:'credits',total:4200,status:'ready'}]);
});

test('late allowance responses cannot cross an account boundary', async () => {
  let token = 'first', finish;
  const events = [];
  const credits = createCredits({token:()=>token,emit:event=>events.push(event),site:'https://example.test',
    fetch:()=>new Promise(resolve=>finish=resolve)});
  const pending = credits.refresh();
  token = '';
  credits.clear();
  finish(Response.json({remaining:999,purchased:100}));
  await pending;
  assert.deepEqual(events, [{type:'credits',total:null,status:'Sign in to view braincells'}]);
});

test('failed or malformed allowances clear the displayed balance', async () => {
  const events = [];
  let response = Response.json({remaining:100,purchased:0});
  const credits = createCredits({token:()=> 'token',emit:event=>events.push(event),site:'https://example.test',fetch:async()=>response});
  await credits.refresh();
  response = Response.json({remaining:'100',purchased:0});
  await credits.refresh();
  assert.equal(events[0].total, 100);
  assert.equal(events[1].total, null);
  response = new Response('', {status:503});
  await credits.refresh();
  assert.equal(events.at(-1).total, null);
});
