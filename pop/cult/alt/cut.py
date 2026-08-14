exec(open('alt/analyze.py').read())

CAND = {
 '7071087615948148010': [
   ('hehe', 'he he he', 3.00, 4.76),
   ('haha', 'ha ha ha', 4.76, 6.50),
   ('cult', 'cult', 9.00, 9.67),
   ('three-of-us', 'the three of us are in a cult', 6.50, 9.67),
   ('guys', 'guys', 1.70, 2.00),
 ],
 '7071405403367755054': [
   ('hehe', 'hee hee hee', 0.00, 2.29),
   ('haha', 'ha ha ha', 2.29, 3.99),
   ('cult', 'cult', 6.27, 7.00),
   ('three-of-us', 'the three of us are in our cult', 3.99, 7.00),
 ],
 '7055506210086456622': [
   ('dashes', 'dash dash dash', 0.20, 2.63),
   ('dots', 'dot dot dot', 2.63, 4.55),
   ('cult-long', 'cult (held)', 7.33, 9.60),
   ('three-of-us', 'the three of us are in a cult', 4.55, 9.60),
 ],
 '7101863800650861866': [
   ('dashes', 'dash dash dash', 0.13, 2.36),
   ('dots', 'dot dot dot', 2.36, 4.24),
   ('cult', 'cult', 6.58, 7.22),
 ],
 '7119595740988460330': [
   ('dashes', 'dash dash dash', 0.18, 1.85),
   ('dots', 'dot dot dot', 1.85, 4.73),
   ('cult', 'cult', 6.94, 7.80),
   ('three-of-us', 'the three of us are in a cult', 4.73, 7.80),
 ],
 '7124494165806828842': [
   ('dashes', 'dash dash dash', 0.50, 3.01),
   ('dots', 'dot dot dot', 3.01, 4.74),
   ('cult-long', 'cult (held)', 7.00, 8.43),
   ('three-of-us', 'the three of us are in a cult', 4.74, 8.43),
 ],
 '7055148270699597103': [
   ('dashes', 'dash dash dash', 2.60, 4.50),
   ('dots', 'dot dot', 4.62, 6.16),
   ('cult', 'cult', 8.42, 9.08),
   ('uh', 'uh', 7.69, 8.26),
 ],
 '7143784658172464426': [
   ('cult', 'cult', 10.19, 10.52),
   ('noway', 'no way guys', 10.80, 11.49),
   ('filmmakers', "we're filmmakers now", 11.73, 13.19),
   ('spotify', 'spotify', 20.91, 21.60),
 ],
 '7144151707113639214': [
   ('count', 'one two three', 0.01, 1.20),
   ('duhs', 'duh duh duh duh', 2.00, 2.38),
   ('dashes', 'dash dash dash', 2.38, 5.05),
   ('dots', 'dot dot dot dot', 5.05, 7.46),
   ('cult', 'cult', 10.95, 11.98),
 ],
 '7156050516676726062': [
   ('dashes', 'dash dash dash', 0.45, 3.00),
   ('dots', 'dot dot', 3.00, 5.00),
   ('three-of-us', 'the three of us are in a', 5.00, 7.40),
 ],
 '7144849836880219434': [
   ('sing-full', '(singing / no words)', 0.13, 13.00),
 ],
}

report={}
rows=[]
for id, cands in CAND.items():
    y=load(id)
    dur=len(y)/SR
    words=json.load(open(f'alt/txt/{id}.json'))['transcription']
    segs=[]
    for name, words_txt, t0, t1 in cands:
        s,t=refine(y,t0,t1)
        # never let refine blow past the requested window by much
        s=max(s, t0-0.30); t=min(t, t1+0.35)
        f0,vfrac=f0_of(y,s,t)
        f0hi,vhi=f0_of(y,s,t,fmin=500,fmax=3000)
        out=f'alt-{id[:4]}-{name}'
        wdur=write(out,y,s,t)
        rms=noisefloor(y,s,t)
        segs.append(dict(file=f'alt/samples/{out}.wav', words=words_txt,
            start=round(s,3), end=round(t,3), dur=round(wdur,3),
            median_f0_hz=round(f0,1) if f0 else None, voiced_frac=round(vfrac,2),
            hi_f0_hz=round(f0hi,1) if f0hi else None, hi_voiced_frac=round(vhi,2),
            rms=round(rms,4)))
        rows.append((id,name,words_txt,round(wdur,3),f0,vfrac,f0hi,vhi))
    report[id]=dict(duration=round(dur,2), speech_found=True,
        transcript=' '.join(w['text'] for w in words).strip(),
        words=[dict(t=w['text'], start=w['offsets']['from']/1000, end=w['offsets']['to']/1000) for w in words],
        segments=segs)

json.dump(report, open('alt/harvest.json','w'), indent=2)
for r in rows:
    print(f'{r[0][:4]} {r[1]:14s} {r[3]:6.2f}s f0={r[4] if r[4] else 0:7.1f} v={r[5]:.2f}  hi={r[6] if r[6] else 0:7.1f} hv={r[7]:.2f}  "{r[2]}"')
