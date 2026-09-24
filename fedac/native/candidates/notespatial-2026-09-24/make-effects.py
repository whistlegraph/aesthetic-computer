import json,pathlib,bisect,gzip
o=pathlib.Path(__file__).parent;score=json.loads(gzip.decompress((o/'fixtures/baseline-score.nsscore.gz').read_bytes()));events=[e for l in score['lanes'] for e in l['events']];edges=sorted([(e['t'],1) for e in events]+[(e['t']+e['dur'],-1) for e in events]);accepted=0;rejected=0
windows=[(m['t0']+2,m['t1']-2) for m in score['movements'] if any(n in m['name'] for n in ['Lullaby','Return'])]
def room(t,end):
 live=0
 for at,d in edges:
  if at>=end:break
  live+=d
  if at>=t and live>=32:return False
 # Also account for voices already active at the candidate start.
 live=sum(d for at,d in edges if at<=t)
 return live<32
for lane in score['lanes']:
 if not (lane.get('center') or lane['name'].startswith(('answer ','echo '))):continue
 taps=[]
 for e in list(lane['events']):
  if not e.get('note') or e.get('hz',0)<180 or not any(a<=e['t']<z for a,z in windows):continue
  for delay,g in [(.24,.18),(.48,.07)]:
   t=e['t']+delay;dur=min(.4,e['dur']*.75)
   if t+dur>score['dur'] or not room(t,t+dur):rejected+=1;continue
   tap={**e,'t':round(t,6),'dur':dur,'g':e['g']*g,'attack':.012,'decay':min(.18,dur*.6),'echoTap':True}
   taps.append(tap);bisect.insort(edges,(tap['t'],1));bisect.insort(edges,(tap['t']+dur,-1));accepted+=1

 lane['events']=sorted(lane['events']+taps,key=lambda e:e['t'])
# Same existing native Notepat flange, global within one laptop; section envelopes.
N=1025;seatfx=score.setdefault('seatFx',{})
for seat in range(6):
 k=str(seat);mine=seatfx.setdefault(k,{}) if isinstance(seatfx,dict) else seatfx[seat]
 src=mine.get('fxWobble',score.get('fxWobble',[0]))
 def old(t):
  u=t/score['dur']*(len(src)-1);i=int(u);return src[i]+(src[min(i+1,len(src)-1)]-src[i])*(u-i)
 arr=[]
 for i in range(N):
  t=i/(N-1)*score['dur'];v=old(t)
  for m in score['movements']:
   selected=('Chase' in m['name'] and seat<5) or ('Lullaby' in m['name'] and seat==5)
   if selected and m['t0']<=t<m['t1']:
    fade=max(0,min(1,(t-m['t0'])/3,(m['t1']-t)/3));v=max(v,(.24 if seat==5 else .3)*fade)
  arr.append(round(min(.35,v),5))
 mine['fxWobble']=arr
score['name']+=' · selected echo + flange'
score['effectRevision']={'echoTaps':accepted,'echoRejectedAtVoiceBudget':rejected,'tapDelays':[.24,.48],'tapRelativeGains':[.18,.07],'flangeMax':.35,'subLanesUnchanged':True}
(o/'notespatial-echo-flange.nsscore').write_text(json.dumps(score,separators=(',',':')))
print(score['effectRevision'])
