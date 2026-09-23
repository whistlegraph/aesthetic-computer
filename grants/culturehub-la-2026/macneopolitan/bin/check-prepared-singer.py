#!/usr/bin/env python3
"""Verify silent preparation, exact float WAV export, dynamics and stale-cache rejection."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[4];source=root/'slab/menuband/Sources/MenuBand'
prefix=(source/'MenuBandSinger.swift').read_text().split('final class MenuBandSinger {')[0].replace('import CSinger','')
stub=r'''
final class MenuBandSinger {
 var calls = 0
 func render(_ line:SungLine, into format:AVAudioFormat, completion:@escaping(SungRender?)->Void) {
  calls += 1
  let b = AVAudioPCMBuffer(pcmFormat:format,frameCapacity:4410)!;b.frameLength=4410
  for i in 0..<4410 { b.floatChannelData![0][i] = Float(sin(Double(i)*0.08)*0.5) }
  completion(SungRender(buffer:b,spanOffset:0,notesUsed:1,noteCount:1,peak:0.5,
   articulation:SingerArticulation(cues:[],energy:[],duration:0.1)))
 }
}
let id = "test-"+UUID().uuidString
let directory = SingerPreparedPerformance.root.appendingPathComponent(id)
defer { try? FileManager.default.removeItem(at:directory) }
let info=["prepareId":id,"lyrics":"doo","notes":"60:1","bpm":"88","singVoice":"Noelle (Enhanced)","singLineGains":"0.3","singNoteGains":"0.6"]
let store = SingerPreparedPerformance(), singer = MenuBandSinger()
let format = AVAudioFormat(commonFormat:.pcmFormatFloat32,sampleRate:44100,channels:1,interleaved:false)!
store.prepare(id,info:info,singer:singer,format:format)
assert(singer.calls == 1)
let kept = store.take(id,info:info)![0]
assert(abs(kept.buffer.floatChannelData![0][20]) < 0.091)
let wav = try AVAudioFile(forReading:directory.appendingPathComponent("phrase-00.wav"))
let reread = AVAudioPCMBuffer(pcmFormat:wav.processingFormat,frameCapacity:AVAudioFrameCount(wav.length))!
try wav.read(into:reread)
assert(reread.frameLength == kept.buffer.frameLength)
for i in 0..<Int(reread.frameLength) { assert(reread.floatChannelData![0][i] == kept.buffer.floatChannelData![0][i]) }
var changed = info; changed["notes"] = "61:1"
assert(store.take(id,info:changed) == nil)
var timed = info; timed["preparedId"]=id;timed["startEpoch"]="123"
assert(store.take(id,info:timed) != nil)
store.prepare(id,info:info,singer:singer,format:format);assert(singer.calls == 1)
store.cancelAll();assert(store.take(id,info:info) == nil)
assert(!SingerPreparedPerformance.validID("../bad"))
print("PASS: silent cache, exact float samples, single dynamics application, payload identity, cancellation.")
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d)/'main.swift';binary=Path(d)/'check'
 p.write_text((source/'SingerArticulation.swift').read_text()+'\n'+(source/'SingerPerformance.swift').read_text()+'\n'+prefix+'\n'+(source/'SingerPreparedPerformance.swift').read_text()+'\n'+stub)
 subprocess.run(['swiftc','-O',str(p),'-o',str(binary)],check=True)
 subprocess.run([str(binary)],check=True)
