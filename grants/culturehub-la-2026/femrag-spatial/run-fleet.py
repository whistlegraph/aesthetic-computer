#!/usr/bin/env python3
"""Femrag conductor. Silent preflight by default; --run announces and cues.
Run only by the agreed rig owner (frisbee:todo). No singer-cache dependency.
"""
import concurrent.futures as cf,json,os,subprocess,time,uuid,urllib.request,threading,signal,sys
from pathlib import Path
OUT=Path(os.environ.get('FEMRAG_OUT','/Users/jas/Shelf/femrag-spatial'))
SUB=os.environ.get('TRIO_SUB','http://127.0.0.1:8791').rstrip('/')
DMX=os.environ.get('TRIO_DMX','http://127.0.0.1:8790').rstrip('/')
VIS=os.environ.get('FEMRAG_VIS','http://192.168.1.234:8796').rstrip('/')
RTT_MAX=.04
plan=json.loads((OUT/'plan.json').read_text());nodes=json.loads((OUT/'native-loaded.json').read_text())
runid='femrag-'+uuid.uuid4().hex[:10];done=threading.Event();errors=[];threads=[]
locks={n['id']:threading.Lock() for n in nodes}
record={'runId':runid,'arrangementHash':plan['arrangementHash'],'samples':[],'timing':'Native simulation-frame deck starts; not acoustically calibrated'}
def request(url,data=None,method=None):
 headers={'Content-Type':'application/json'}
 if url.startswith(VIS):headers['Origin']=VIS
 raw=None if data is None else json.dumps(data).encode()
 with urllib.request.urlopen(urllib.request.Request(url,raw,headers,method=method),timeout=2) as r:
  b=r.read()
  try:return json.loads(b)
  except:return b.decode()
def parallel(fn,items):
 with cf.ThreadPoolExecutor(max_workers=6) as p:return list(p.map(fn,items))
def save(): (OUT/(runid+'.json')).write_text(json.dumps(record,indent=2))
def status(n):return request(n['url']+'/pieces/trio-fleet-status.json')
def command(n,action,**kw):
 with locks[n['id']]:
  cid=uuid.uuid4().hex;request(n['url']+'/pieces/trio-fleet-command.json',dict(id=cid,action=action,**kw),'PUT');return cid
def ack(n,phase):
 end=time.monotonic()+3
 while time.monotonic()<end:
  s=status(n)
  if s.get('error'):raise RuntimeError((n['id'],s['error']))
  if s['phase']==phase and s['runId']==runid:return s
  time.sleep(.03)
 raise RuntimeError((n['id'],'Missing '+phase+' ACK'))
def nativecheck(n):
 s=status(n);time.sleep(.08);s2=status(n)
 assert s2['audioTime']>s['audioTime'] and s2['instance']==s['instance'],n['id']+' stale'
 assert s2['phase']=='ready' and not s2['error'] and s2['arrangementHash']==plan['arrangementHash'] and s2['receiverId']==n['id'],s2
 assert s2['centerReady'] and s2['center']['loaded'] and n['assetReadbackVerified'],s2
 assert s2['mono'] and s2['monoOutput']=='left' and not s2['microphoneHot'],s2
 assert abs(s2['center']['duration']-plan['duration'])<.1,s2
 samples=[]
 for _ in range(48):
  a=time.monotonic();cid=command(n,'clock');end=a+2
  while time.monotonic()<end:
   c=request(n['url']+'/pieces/trio-fleet-clock.json')
   if isinstance(c,dict) and c.get('id')==cid:break
   time.sleep(.005)
  else:raise RuntimeError(n['id']+' clock timeout')
  b=time.monotonic();assert c['instance']==s2['instance'];samples.append({'offset':c['audioTime']-(a+b)/2,'rtt':b-a})
 best=min(samples,key=lambda x:x['rtt']);assert best['rtt']<RTT_MAX,(n['id'],'Clock round trip exceeds40ms',best)
 return n['id'],{'clock':best,'receipt':s2}
def subcheck():
 rs=request(SUB+'/api/receivers');r=next((r for r in rs if r['ip'].endswith('192.168.1.67') and r['online'] and r['armed']),None)
 assert r and r['audioState']=='running' and r['fullscreen'] and r['route']=='both',('SUB not ready',rs)
 assert r['scoreHash']==plan['arrangementHash'] and r.get('stemReady') and r.get('stemHash')==plan['arrangementHash'],r
 assert abs(r['duration']-plan['duration'])<.1,r
 return r
def dmxcheck():
 s=request(DMX+'/state');assert s['supportsCancel'] and time.time()-s['bridgeSeen']<4 and s['queueDepth']==0,s
 return s
def cancel_dmx():
 request(DMX+'/cancel',{});cid=request(DMX+'/state')['cancelId'];end=time.monotonic()+4
 while time.monotonic()<end:
  s=request(DMX+'/state')
  if s.get('lastResult',{}).get('id')==cid and s['lastResult']['result']=='ok' and s['queueDepth']==0:return s['lastResult']
  time.sleep(.05)
 raise RuntimeError('DMX blackout not acknowledged')
def visuals(playing,elapsed):return request(VIS+'/api/transport',{'playing':playing,'elapsed':max(0,min(plan['duration'],elapsed))})
def heartbeat():
 while not done.is_set():
  try:
   parallel(lambda n:command(n,'keepalive',runId=runid),nodes)
   request(SUB+'/api/trio/keepalive',{'runId':runid})
   t=time.monotonic()-downbeat;visuals(0<=t<plan['duration'],t)
  except Exception as e:errors.append(str(e));return
  done.wait(.25)
def lights():
 checked=0;queue=0
 for e in (e for e in plan['events'] if e['layer']=='dmx'):
  if done.wait(max(0,downbeat+e['t']-time.monotonic())):return
  try:
   now=time.monotonic()
   if now-checked>.5:queue=request(DMX+'/state')['queueDepth'];checked=now
   if now-downbeat-e['t']>.2 or queue>=8:
    record['skippedLateOrQueuedLights']=record.get('skippedLateOrQueuedLights',0)+1;continue
   request(DMX+'/command',dict(e['command'],eventId=runid+'-'+str(e['id'])))
   queue+=1
  except Exception as e:errors.append('DMX '+str(e));return
try:
 native=dict(parallel(nativecheck,nodes));sub=subcheck()
 assert abs(sub['level']-.25)<.001,('Initial SUB level must be25%',sub['level'])
 for n in nodes:
  v=request(n['url']+'/pieces/composition-volume-status.json');assert abs(v['percent']-25)<.2,v
 record['checks']={'native':native,'sub':sub,'dmx':dmxcheck(),'visualFeed':request(VIS+'/api/performance')};save()
 print('READY: six decoded stems, sample SUB, room and center DMX, visual feed.',flush=True)
except Exception as e:record['error']=str(e);save();raise
if '--run' not in sys.argv:sys.exit(0)
# TTS is on the conducting Mac only, before scheduling the musical downbeat.
env=dict(os.environ,MB_NAME='computer.aestheticcomputer.menuband.say',MB_KV='text=Femrag plus plus, in the round.;voice=Zoe (Premium);startEpoch='+str(time.time()+.5))
subprocess.run(['/tmp/mbpost'],env=env,check=True,timeout=5)
epoch=time.time()+15;downbeat=time.monotonic()+(epoch-time.time());record['startEpoch']=epoch
signal.signal(signal.SIGTERM,lambda *_:(_ for _ in ()).throw(KeyboardInterrupt()))
def cleanup():
 done.set()
 for t in threads:t.join(timeout=3)
 result={}
 for n in nodes:
  try:command(n,'stop');result[n['id']]='stop sent'
  except Exception as e:result[n['id']]=str(e)
 for label,fn in [('sub',lambda:request(SUB+'/api/trio/stop',{})),('dmx',cancel_dmx),('visuals',lambda:visuals(False,0))]:
  try:result[label]=fn()
  except Exception as e:result[label]=str(e)
 record['cleanup']=result;save()
try:
 def prepare(n):
  command(n,'prepare',arrangementHash=plan['arrangementHash'],startAt=downbeat+native[n['id']]['clock']['offset'],runId=runid);return n['id'],ack(n,'prepared')
 record['prepared']=dict(parallel(prepare,nodes));record['subPrepared']=request(SUB+'/api/trio/prepare',{'hash':plan['arrangementHash'],'runId':runid,'startEpoch':epoch})
 t=threading.Thread(target=heartbeat,daemon=True);threads.append(t);t.start()
 def play(n):command(n,'play',runId=runid);return n['id'],ack(n,'countdown')
 record['countdown']=dict(parallel(play,nodes));record['subCountdown']=request(SUB+'/api/trio/play',{'runId':runid})
 assert epoch-time.time()>2 and not errors
 t=threading.Thread(target=lights,daemon=True);threads.append(t);t.start();save();print('CUED: downbeat in',round(epoch-time.time(),1),'seconds',flush=True)
 last=-1
 while time.monotonic()<downbeat+plan['duration']+1:
  if errors:raise RuntimeError(errors)
  elapsed=time.monotonic()-downbeat
  if elapsed>=0 and int(elapsed)//2!=last:
   last=int(elapsed)//2;ss=parallel(status,nodes)
   for s in ss:assert not s['error'],s
   sub=subcheck();dmx=request(DMX+'/state');assert time.time()-dmx['bridgeSeen']<4,'DMX offline'
   record['samples'].append({'t':elapsed,'native':ss,'sub':sub,'dmx':dmx.get('lastResult')});save()
   if last%5==0:print('Playing',round(elapsed),'seconds; six stems and SUB online.',flush=True)
  time.sleep(.08)
 record['completed']=True;print('Femrag performance completed.',flush=True)
except BaseException as e:record['error']=repr(e);raise
finally:cleanup()
