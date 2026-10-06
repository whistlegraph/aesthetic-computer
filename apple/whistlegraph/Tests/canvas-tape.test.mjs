import {test} from 'node:test';
import assert from 'node:assert/strict';
import {createCanvasTapeRecorder} from '../../../system/public/aesthetic.computer/lib/canvas-tape.mjs';

function fixture({supported=['video/mp4'], fail=false}={}) {
  const stopped=[], tracks=[];
  const video={stop:()=>stopped.push('video')},audio={stop:()=>stopped.push('audio')};
  const stream={getVideoTracks:()=>[video],addTrack:t=>tracks.push(t)};
  const canvas={captureStream:()=>stream};
  class Recorder extends EventTarget {
    static isTypeSupported(type){return supported.includes(type);}
    constructor(stream,options){super();if(fail)throw Error('encoder failed');this.mimeType=options.mimeType;}
  }
  return {Recorder,canvas,audio,stopped,tracks};
}

test('finishing a tape releases its canvas track, preserving shared app audio',()=>{
  const f=fixture();globalThis.MediaRecorder=f.Recorder;
  const tape=createCanvasTapeRecorder(f.canvas,{audioTracks:[f.audio]});
  assert.equal(tape.mimeType,'video/mp4');assert.deepEqual(f.tracks,[f.audio]);
  tape.dispatchEvent(new Event('stop'));assert.deepEqual(f.stopped,['video']);
});
test('MP4-only export fails before capture when only WebM is supported',()=>{
  const f=fixture({supported:['video/webm']});globalThis.MediaRecorder=f.Recorder;
  f.canvas.captureStream=()=>{throw Error('must not capture');};
  assert.throws(()=>createCanvasTapeRecorder(f.canvas,{mp4Only:true}),/cannot encode an MP4/);
});
test('an encoder failure releases the acquired video track',()=>{
  const f=fixture({fail:true});globalThis.MediaRecorder=f.Recorder;
  assert.throws(()=>createCanvasTapeRecorder(f.canvas),/encoder failed/);assert.deepEqual(f.stopped,['video']);
});
