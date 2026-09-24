#!/usr/bin/env python3
"""measure-sync.py — while a Trio piece plays, sample every seat's audio clock
against this machine's wall clock for N seconds and report: each seat's rate
(1.000 = locked; >1 rushing), its jitter, and the seat-to-seat spread of
score position. Read-only: only GETs the seats' status files.

    python3 bin/measure-sync.py [seconds=20] [--fleet ~/.ac-os/culturehub/fleet.json]
"""
import json,os,sys,time,statistics,urllib.request,concurrent.futures as cf
secs=float(next((a for a in sys.argv[1:] if not a.startswith('--')),20))
fleet=json.load(open(os.path.expanduser(next((sys.argv[i+1] for i,a in enumerate(sys.argv) if a=='--fleet'),'~/.ac-os/culturehub/fleet.json'))))
seats=[(h,p,l) for h,p,l,*_ in fleet]
def get(seat):
    h,p,l=seat
    try:
        with urllib.request.urlopen('http://%s:%s/pieces/trio-fleet-status.json'%(h,p),timeout=1.5) as r:j=json.loads(r.read())
        return l,j
    except Exception:return l,None
rows=[];t_end=time.time()+secs
while time.time()<t_end:
    t0=time.time()
    with cf.ThreadPoolExecutor(6) as ex:res=list(ex.map(get,seats))
    t1=time.time();rows.append(((t0+t1)/2,{l:j for l,j in res if j}))
    time.sleep(max(0,.5-(t1-t0)))
print('samples',len(rows))
labels=[l for _,_,l in seats]
for l in labels:
    pts=[(w,v[l]['audioTime'],(v[l]['audioTime']-v[l]['origin']) if v[l].get('origin') else None,v[l].get('phase'),v[l].get('maxFrameGap')) for w,v in rows if l in v and v[l].get('audioTime') is not None]
    if len(pts)<5:print('%-13s no data'%l);continue
    w0,a0=pts[0][0],pts[0][1];w1,a1=pts[-1][0],pts[-1][1];rate=(a1-a0)/(w1-w0)
    resid=[(a-a0)-(w-w0) for w,a,*_ in pts];jit=(max(resid)-min(resid))/2
    print('%-13s rate %.4f  jitter ±%.3f s  phase %s  maxFrameGap %.2f'%(l,rate,jit,pts[-1][3],pts[-1][4] or 0))
spread=[]
for w,v in rows:
    pos=[v[l]['audioTime']-v[l]['origin'] for l in v if v[l].get('origin') and v[l].get('phase')=='playing']
    if len(pos)>=2:spread.append(max(pos)-min(pos))
if spread:print('seat-to-seat spread of score position (playing seats): median %.3f s  max %.3f s  (includes ~%d ms of poll latency)'%(statistics.median(spread),max(spread),40))
else:print('no seats reported playing during the window')
