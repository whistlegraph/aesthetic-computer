import test from 'node:test';
import assert from 'node:assert/strict';
import {rollToward} from '../src/braincell-meter.mjs';
test('the meter rolls to the held balance and stops on it',()=>{
 let shown=10_000_000,steps=0;
 while(shown!==9_996_431&&steps<200){const next=rollToward(shown,9_996_431);assert.ok(next<shown&&next>=9_996_431);shown=next;steps++;}
 assert.equal(shown,9_996_431);assert.ok(steps>3&&steps<80);
 assert.equal(rollToward(5,9),6);assert.equal(rollToward(null,7),7);assert.equal(rollToward(7,7),7);
});
