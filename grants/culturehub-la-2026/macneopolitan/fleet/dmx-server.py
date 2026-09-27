from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from pathlib import Path
import json,time,threading,shutil,uuid,socket
from collections import deque
from urllib.parse import urlparse,parse_qs
QUEUE=deque()
COMMANDS={}
ROOT=Path(__file__).parent
STATE={'address':None,'color':'off','level':0,'updated':time.time(),'fixtures':[]}
if (ROOT/'state.json').exists():
 try: STATE.update(json.loads((ROOT/'state.json').read_text()))
 except Exception: pass
LOCK=threading.Lock()
READY=threading.Condition(LOCK)
class Handler(BaseHTTPRequestHandler):
 protocol_version="HTTP/1.1"
 def setup(self):
  super().setup();self.connection.setsockopt(socket.IPPROTO_TCP,socket.TCP_NODELAY,1)
 def log_message(self,*args): pass
 def do_GET(self):
  path=self.path.split('?')[0]
  if path=='/next':
   with LOCK:
    STATE['bridgeSeen']=time.time()
    wait=min(1,max(0,float(parse_qs(urlparse(self.path).query).get('wait',['0'])[0])))
    if not QUEUE and wait: READY.wait_for(lambda:bool(QUEUE),timeout=wait)
    data=json.dumps(QUEUE.popleft() if QUEUE else {}).encode()
   mime='application/json'
  elif path=='/state':
   with LOCK:
    STATE['queueDepth']=len(QUEUE);STATE['supportsCancel']=True
    STATE['cameraAge']=time.time()-(ROOT/'latest.jpg').stat().st_mtime if (ROOT/'latest.jpg').exists() else 999
    data=json.dumps(STATE).encode()
   mime='application/json'
  elif path in ('/','/index.html','/latest.jpg','/Map-Lights.ps1','/bridge.ps1','/Upgrade-Concert.ps1'):
   p=ROOT/('index.html' if path=='/' else path[1:])
   if not p.exists(): self.send_error(404);return
   data=p.read_bytes();mime='image/jpeg' if p.suffix=='.jpg' else 'text/html' if p.suffix=='.html' else 'text/plain'
  else: self.send_error(404);return
  self.send_response(200);self.send_header('Content-Type',mime);self.send_header('Cache-Control','no-store');self.send_header('Content-Length',str(len(data)));self.end_headers();self.wfile.write(data)
 def do_POST(self):
  if self.path not in ('/event','/fixtures','/command','/ack','/cancel'): self.send_error(404);return
  n=int(self.headers.get('Content-Length','0'))
  if n>16000: self.send_error(413);return
  try: d=json.loads(self.rfile.read(n))
  except Exception: self.send_error(400);return
  with LOCK:
   if self.path=='/cancel':
    if self.client_address[0] not in ('127.0.0.1','::1'): self.send_error(403);return
    QUEUE.clear()
    d={'id':uuid.uuid4().hex[:8],'address':1,'logicalAddress':'all','all':True,'addresses':[1,11,21,31,41],'color':'red','channel':0,'level':0,'duration':.2,'receivedAt':time.time()}
    COMMANDS[d['id']]=dict(d);QUEUE.append(d);READY.notify_all();STATE['queued']=d;STATE['cancelId']=d['id']
   elif self.path=='/command':
    if self.client_address[0] not in ('127.0.0.1','::1') or d.get('address') not in (1,11,21,31,41,'all') or d.get('color') not in ('red','green','blue','orange','white','violet','rgb') or not 0<=d.get('level',96)<=128 or not 0<d.get('duration',8)<=3600:
     self.send_error(400);return
    if d.get('color')=='rgb' and (not isinstance(d.get('rgb'),list) or len(d['rgb'])!=3 or any(not isinstance(v,int) or not 0<=v<=128 for v in d['rgb'])):
     self.send_error(400);return
    if 'envelope' in d and (not isinstance(d['envelope'],dict) or any(not isinstance(d['envelope'].get(k,0),(int,float)) or not 0<=d['envelope'].get(k,0)<=3600 for k in ('attack','decay'))):
     self.send_error(400);return
    d['receivedAt']=time.time();d['id']=uuid.uuid4().hex[:8];d['logicalAddress']=d['address'];d['all']=d['address']=='all';d['address']=1 if d['all'] else d['address'];d['addresses']=sorted(set(f.get('address',1) for f in STATE.get('fixtures',[])));d['channel']=('red','green','blue','orange','white','violet','rgb').index(d['color']);d.setdefault('level',96);d.setdefault('duration',8);COMMANDS[d['id']]=dict(d);QUEUE.append(d);READY.notify_all();STATE['queued']=d
   elif self.path=='/ack': STATE['lastResult']=d
   elif self.path=='/fixtures': STATE['fixtures']=d
   else:
    if d.get('id') in COMMANDS:
     command=COMMANDS[d['id']];d['address']=command['logicalAddress']
     if d.get('color')!='off':
      STATE['latencySamples']=(STATE.get('latencySamples',[])+[{'id':d['id'],'ms':round((time.time()-command['receivedAt'])*1000,2)}])[-100:]
    STATE['eventId']=d.get('id')
    STATE.update({k:d[k] for k in ('address','color','level','rgb') if k in d}); STATE['updated']=time.time()
   (ROOT/'state.json').write_text(json.dumps(STATE,indent=2))
  self.send_response(200);self.send_header('Content-Length','2');self.end_headers();self.wfile.write(b'ok')
def evidence():
 while True:
  time.sleep(1)
  with LOCK: s=dict(STATE)
  if s.get('color')!='off' and time.time()-s.get('updated',0)>1 and (ROOT/'latest.jpg').exists():
   name=f"evidence-{s.get('address')}-{s.get('color')}-{time.time_ns()}"
   shutil.copyfile(ROOT/'latest.jpg',ROOT/(name+'.jpg'));(ROOT/(name+'.json')).write_text(json.dumps(s))
threading.Thread(target=evidence,daemon=True).start()
ThreadingHTTPServer(('0.0.0.0',8790),Handler).serve_forever()
