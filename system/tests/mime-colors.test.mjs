import test from 'node:test';
import assert from 'node:assert/strict';
import {publicPosts} from '../backend/mime-media.mjs';

test('public feed carries only saved letter RGB values alongside current handles', async () => {
  const db = {collection(name) {
    assert.equal(name,'@handles');
    return {find(query,options) {
      assert.deepEqual(options.projection,{handle:1,colors:1});
      return {toArray:async()=>[{_id:'private-id',handle:'artist',colors:[{r:24,g:128,b:240,secret:'hidden'},null,{r:500,g:0,b:-1}]}]};
    }};
  }};
  const [post] = await publicPosts(db,[{code:'sample',user:'private-id',board:'text/plain',text:'hello'}]);
  assert.equal(post.name,'@artist');
  assert.deepEqual(post.nameColors,[{r:24,g:128,b:240},null,{r:255,g:0,b:0}]);
  assert.ok(!JSON.stringify(post).includes('private-id'));
});
