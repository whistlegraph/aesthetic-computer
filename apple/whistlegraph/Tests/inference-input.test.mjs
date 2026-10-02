import test from 'node:test';
import assert from 'node:assert/strict';
import {inferenceRequest,wantsSoundEvidence} from '../Resources/Web/inference-input.mjs';
const request=transcript=>'Legacy audio-heavy instructions\nINPUT DATA:\n'+JSON.stringify({transcript,words:[{text:transcript,atMs:0}],sound:{frames:[{rms:.2,pitchHz:440}],recordingID:'private-recording-reference'}});
test('ordinary requests contain only spoken words, including recovered legacy input',()=>{
 for(const text of ['The most beautiful mountain in the universe','Cookies for my love','Make it sing','Scrumptious']) {
  assert.equal(inferenceRequest(request(text)),text);
  assert.equal(wantsSoundEvidence(text),false);
 }
});
test('explicit sound references and nonverbal input retain evidence without recording IDs',()=>{
 for(const text of ['', 'Make it bounce like this', 'Use my whistle']) {
  const result=inferenceRequest(request(text));
  assert.match(result,/pitchHz/);assert.doesNotMatch(result,/private-recording-reference|recordingID|Legacy audio-heavy/);
 }
});
test('typed requests pass through unchanged',()=>assert.equal(inferenceRequest('Add a moon'),'Add a moon'));
