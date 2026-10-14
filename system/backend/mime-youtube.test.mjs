import test from 'node:test';
import assert from 'node:assert/strict';
import {youtubeEmbed} from './mime-youtube.mjs';
import {chatPost} from './mime-feed-engine.mjs';
const id='8pT-d9juqCo';
test('Prutti chat keeps its global identity and receives a YouTube player',()=>{
  const post=chatPost({_id:'6acfadf5fcbe3ccb3a32290d',text:`https://www.youtube.com/watch?v=${id}&list=RD${id}&start_radio=1`},'@prutti','clock','qjj');
  assert.equal(post.media.kind,'chat');assert.equal(post.media.code,'qjj');assert.equal(post.origin.name,'laer-klokken');assert.equal(post.name,'@prutti');assert.equal(post.board,'text/plain');
  assert.deepEqual(post.embed,{kind:'youtube',id,start:0,caption:'',url:`https://www.youtube.com/watch?v=${id}`});
});
test('YouTube watch, short, live, embed and short-domain links preserve timestamps',()=>{
  for(const url of [`https://youtu.be/${id}?t=1h2m3s`,`https://youtube.com/watch?v=${id}&t=3723`,`https://m.youtube.com/shorts/${id}?start=3723`,`https://www.youtube.com/live/${id}?t=3723s`,`https://www.youtube-nocookie.com/embed/${id}?start=3723`])assert.equal(youtubeEmbed(`Here (${url}).`).start,3723);
});
test('unrelated links, spoofed hosts, credentials and malformed IDs remain text',()=>{
  for(const url of [`https://youtube.com.evil.example/watch?v=${id}`,`https://youtube.com@evil.example/watch?v=${id}`,`https://user@youtube.com/watch?v=${id}`,`https://youtube.com:123/watch?v=${id}`,`https://youtu.be/${id}/extra`,'https://youtube.com/watch?v=bad','javascript:alert(1)','hello world'])assert.equal(youtubeEmbed(url),null,url);
});

test('accompanying words survive the embed',()=>{assert.equal(youtubeEmbed(`Listen to this https://youtu.be/${id} please`).caption,'Listen to this  please');});
