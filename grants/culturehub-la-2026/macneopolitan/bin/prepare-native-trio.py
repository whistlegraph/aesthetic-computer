#!/usr/bin/env python3
"""Stage the new native piece and exact Center WAV. Never sends prepare/play."""
import concurrent.futures,hashlib,json,struct,time,urllib.request
from pathlib import Path
import os
ROOT=Path(__file__).resolve().parents[1];OUT=Path(os.environ.get('TRIO_OUT','/Users/jas/Shelf/culturehub-one-big-voice'))   # TRIO_OUT=… picks the song's shelf folder
ALLOW=['culturehub-rehearsal','culturehub-concert','trio-fleet']+[p for p in os.environ.get('TRIO_ALLOW_PIECES','').split(',') if p]   # pieces a seat may be showing before we load
plan=json.loads((OUT/'plan.json').read_text());assets=json.loads((OUT/'prepared.json').read_text())
assert assets['arrangementHash']==plan['arrangementHash']
raw=Path(assets['centerMix']['file']).read_bytes();rate=assets['centerMix']['sampleRate']
assert hashlib.sha256(raw).hexdigest()==assets['centerMix']['sha256']
wave=b'RIFF'+struct.pack('<I',36+len(raw))+b'WAVEfmt '+struct.pack('<IHHIIHH',16,3,1,rate,rate*4,4,32)+b'data'+struct.pack('<I',len(raw))+raw
wavehash=hashlib.sha256(wave).hexdigest();wavename='/pieces/trio-voices-'+wavehash[:16]+'.wav';(OUT/'center-voices.wav').write_bytes(wave)
code=(ROOT/'fleet/native-trio.mjs').read_bytes()
def get(url):
 with urllib.request.urlopen(url,timeout=20) as r:return r.read()
def put(url,data):
 if not isinstance(data,bytes):data=json.dumps(data).encode()
 with urllib.request.urlopen(urllib.request.Request(url,data=data,method='PUT'),timeout=30) as r:return r.read()
def load(node):
 url=f"http://{node['host']}:{node['port']}";previous=json.loads(get(url+'/status'))
 try:old_instance=json.loads(get(url+'/pieces/trio-fleet-status.json'))['instance']
 except:old_instance=None
 if previous['piece'] not in ALLOW:raise RuntimeError('Unexpected active piece '+previous['piece']+' (TRIO_ALLOW_PIECES to permit)')
 cfg={'schema':'trio-native-v1','receiverId':node['id'],'arrangementHash':plan['arrangementHash'],'bpm':plan['bpm'],'duration':plan['duration'],
 'color':[[143,209,63],[90,87,211],[242,167,185]][node['seat']%3],
 'events':[e for e in plan['events'] if e['receiver']==node['id'] and e['layer']!='dmx']}
 if node['seat']==5:
  chunks=[]
  for i,start in enumerate(range(0,len(wave),4*1024*1024)):
   name='/pieces/trio-'+wavehash[:16]+'-'+str(i)+'.part';chunk=wave[start:start+4*1024*1024]
   put(url+name,chunk);assert hashlib.sha256(get(url+name)).digest()==hashlib.sha256(chunk).digest();chunks.append(name)
  cfg['center']={'file':wavename,'parts':chunks,'sha256':wavehash,'rawSha256':assets['centerMix']['sha256'],'duration':assets['centerMix']['frames']/rate,'bytes':len(wave),'gainBakedIn':assets['centerMix']['bakedGain']}
 put(url+'/pieces/trio-fleet-config.json',cfg);put(url+'/pieces/trio-fleet-command.json',{'id':'boot-idle-'+str(time.time_ns()),'action':'idle'})
 if os.environ.get('TRIO_KEEP_PIECE'):   # TRIO_KEEP_PIECE=1 leaves the seat's staged trio-fleet.mjs alone (someone else's module); config + Center still load
  print(node['id'],'keeping the staged piece code',flush=True)
 else:
  put(url+'/pieces/trio-fleet.mjs',code)
  assert hashlib.sha256(get(url+'/pieces/trio-fleet.mjs')).digest()==hashlib.sha256(code).digest()
 put(url+'/jump/trio-fleet',b'')
 deadline=time.monotonic()+25
 while time.monotonic()<deadline:
  try:
   s=json.loads(get(url+'/pieces/trio-fleet-status.json'))
   if s.get('instance')==old_instance:time.sleep(.2);continue
   if s.get('error'):raise RuntimeError(node['id']+': '+s['error'])
   if s.get('arrangementHash')==plan['arrangementHash'] and s['phase']=='ready' and s['centerReady']:
    assert s['mono'] and s['monoOutput']=='left' and not s['microphoneHot']
    print(node['id'],'loaded silently; Center ready',s['centerReady'],flush=True)
    return {**node,'url':url,'previousPiece':previous['piece'],'status':s,'observedAt':time.time(),'assetReadbackVerified':node['seat']==5}
  except urllib.error.HTTPError:pass
  time.sleep(.2)
 raise RuntimeError(node['id']+' readiness timeout')
with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
 results=list(pool.map(load,plan['nodes']))
(OUT/'native-loaded.json').write_text(json.dumps(results,indent=2))
print('Native staging complete; no audio cue.')
