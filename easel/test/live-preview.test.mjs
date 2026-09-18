import test from 'node:test';
import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
const {matchesPreview}=createRequire(import.meta.url)('../desktop/frame-capture.cjs');
const view=(url,channel)=>({getURL:()=>url,aeselLiveChannel:channel});
test('captures public routes and explicitly bound live-channel routes',()=>{
 assert.equal(matchesPreview(view('https://aesthetic.computer/@jeffrey/soge'),'jeffrey/soge'),true);
 assert.equal(matchesPreview(view('https://aesthetic.computer/soge','jeffrey/soge'),'jeffrey/soge'),true);
 assert.equal(matchesPreview(view('https://aesthetic.computer/soge'),'jeffrey/soge'),false);
 assert.equal(matchesPreview(view('https://aesthetic.computer/soge','someone/soge'),'jeffrey/soge'),false);
 assert.equal(matchesPreview(view('https://example.com/@jeffrey/soge'),'jeffrey/soge'),false);
 assert.equal(matchesPreview(view('https://aesthetic.computer/other','jeffrey/soge'),'jeffrey/soge'),false);
});
