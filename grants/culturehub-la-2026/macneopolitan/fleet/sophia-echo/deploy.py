from pathlib import Path
import json,urllib.request,hashlib,concurrent.futures,time,sys
root=Path(sys.argv[1]);built=root/'built';plan=json.loads((built/'plan.json').read_text());manifest=json.loads((built/'manifest.json').read_text())
code=(root/'native-source.mjs').read_text().replace("if(cfg?.center)api.system.dmxSend", "if(cfg?.heldCenter)api.system.dmxSend").replace("if(!cfg?.center||now-lightAt<.05)","if(!cfg?.heldCenter||now-lightAt<.05)")
# Deck playback is used on all six; local serial light belongs to held-center only.
(root/'native-echo.mjs').write_text(code)
colors={'neo':[242,167,185],'blueberry':[143,209,63],'frisbee':[90,87,211]}
def load(node):
 seat=node['seat'];m=manifest['manifest'][seat];base=f"http://{node['host']}:{node['port']}"
 def get(p):return urllib.request.urlopen(base+p,timeout=30).read()
 def put(p,d):return urllib.request.urlopen(urllib.request.Request(base+p,data=d if isinstance(d,bytes) else json.dumps(d).encode(),method='PUT'),timeout=30).read()
 before=json.loads(get('/status'));assert before['piece']=='trio-fleet',before
 state=json.loads(get('/pieces/trio-fleet-status.json'));assert state['phase'] in ['ready','finished','error'],state
 wave=(built/m['file']).read_bytes();parts=[]
 for i,start in enumerate(range(0,len(wave),4*1024*1024)):
  name='/pieces/trio-'+m['sha256'][:16]+'-'+str(i)+'.part';chunk=wave[start:start+4*1024*1024];put(name,chunk);assert hashlib.sha256(get(name)).digest()==hashlib.sha256(chunk).digest();parts.append(name)
 cfg={'schema':'trio-native-v1','receiverId':node['id'],'arrangementHash':plan['arrangementHash'],'bpm':plan['bpm'],'duration':plan['duration'],'color':[[143,209,63],[90,87,211],[242,167,185]][seat%3],'events':[e for e in plan['events'] if e.get('receiver')==node['id'] and e['layer']!='dmx'],'heldCenter':seat==5,'center':{'file':'/pieces/trio-voices-'+m['sha256'][:16]+'.wav','parts':parts,'sha256':m['sha256'],'rawSha256':m['rawSha256'],'duration':m['frames']/m['sampleRate'],'bytes':len(wave),'gainBakedIn':.35},'lightCues':[]}
 if seat==5:
  for r in manifest['routes']:
   if r['seat']==5 and r['duration']>0:cfg['lightCues'].append({'t':r['offsetSeconds'],'dur':r['duration'],'rgb':[round(c/255*88*r['gain']/.35) for c in colors[r['member']]]})
 put('/pieces/trio-fleet-config.json',cfg);put('/pieces/trio-fleet-command.json',{'id':'echo-idle-'+str(time.time_ns()),'action':'idle'});put('/pieces/trio-fleet.mjs',code.encode());put('/jump/trio-fleet',b'')
 end=time.monotonic()+30
 while time.monotonic()<end:
  s=json.loads(get('/pieces/trio-fleet-status.json'))
  if s.get('instance')!=state.get('instance') and s.get('arrangementHash')==plan['arrangementHash'] and s['phase']=='ready' and s['centerReady']:
   assert s['center']['rawSha256']==m['rawSha256'];print(node['id'],'echo stem verified, ready',flush=True);return {**node,'url':base,'status':s,'observedAt':time.time(),'assetReadbackVerified':True}
  time.sleep(.2)
 raise RuntimeError((node['id'],'not ready'))
with concurrent.futures.ThreadPoolExecutor(max_workers=6) as ex:results=list(ex.map(load,plan['nodes']))
(built/'native-loaded.json').write_text(json.dumps(results,indent=2))
