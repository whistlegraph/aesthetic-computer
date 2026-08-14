exec(open('alt/bursts.py').read())

CUTS = {
 '7055148270699597103': [
   ('dash1','dash',2.06,2.49), ('dash2','dash',2.58,2.98), ('dash3','dash',3.09,3.88),
   ('dot1','dot',4.08,4.32), ('dot2','dot',4.57,4.87),
   ('threeofus','the three of us are in',6.05,7.81),
   ('cult','cult',7.94,8.28),
 ],
 '7055506210086456622': [
   ('dash1','dash',0.79,1.23), ('dash2','dash',1.411,1.637), ('dash3','dash',2.016,2.221),
   ('dot1','dot',2.98,3.26), ('dot2','dot',3.61,3.87), ('dot3','dot',4.15,4.37),
   ('threeofus','the three of us are in',5.01,6.72),
   ('uh','uh / a',6.82,7.28), ('cult','cult',7.37,7.69),
 ],
 '7071087615948148010': [
   ('intro','lets do it around the watery hole guys',0.00,1.93),
   ('hehehaha','he he he ha ha ha',3.17,5.29),
   ('ha1','ha ha',5.39,5.81), ('ha2','ha ha',5.91,6.31),
   ('threeofus','the three of us',6.75,7.74),
   ('inacult','are in a cult',7.864,9.035),
   ('cult','cult',9.104,9.456),
 ],
 '7071405403367755054': [
   ('hehe','hee hee hee',0.23,1.87), ('haha','ha ha ha',2.21,3.68),
   ('threeofus','the three of us',4.265,4.916),
   ('inour','are in our',4.974,6.30),
   ('cult','cult',6.340,6.649),
 ],
 '7101863800650861866': [
   ('dash1','dash',0.13,0.58), ('dash2','dash',0.70,1.16), ('dash3','dash',1.26,1.56),
   ('dot1','dot',2.36,2.55), ('dot2','dot',2.91,3.20), ('dot3','dot',3.46,3.79),
   ('threeofus','the three of us are in',4.51,5.97),
   ('uh','uh / a',6.07,6.41), ('cult','cult',6.59,6.83),
 ],
 '7119595740988460330': [
   ('dashes','dash dash dash',0.19,1.86),
   ('dot1','dot',2.62,3.01), ('dot2','dot',3.30,3.66), ('dot3','dot',3.90,4.17),
   ('threeofus','three of us',5.12,5.85), ('arein','are in',5.99,6.78),
   ('uh','uh / a',6.94,7.44), ('cult','cult',7.54,8.00),
 ],
 '7124494165806828842': [
   ('dash1','dash',1.529,1.761), ('dash2','dash',2.05,2.33),
   ('dot1','dot',3.01,3.26), ('dot2','dot',3.50,3.77), ('dot3','dot',3.98,4.24),
   ('threeofus','the three of us are in',5.01,6.38),
   ('uh','uh / a',6.49,6.81), ('cult','cult',6.99,7.36),
 ],
 '7143784658172464426': [
   ('dash1','dash',1.198,1.867), ('dash2','dash',2.185,2.758),
   ('dot1','dot',3.22,3.96), ('dot2','dot',4.09,4.62),
   ('cult','cult',11.864,12.072),
   ('noway','no way guys',12.240,13.061),
   ('filmmakers',"we're filmmakers now",13.56,14.62),
   ('ever','ever',19.12,19.63),
   ('spotify','staying on spotify too guys / spotify',19.87,21.93),
 ],
 '7144151707113639214': [
   ('duhduhduh','duh duh duh',1.18,1.80),
   ('dash1','dash',2.27,2.75), ('dash2','dash',2.95,3.38), ('dash3','dash',3.59,4.28),
   ('dot1','dot',5.05,5.36), ('dot2','dot',5.72,5.94), ('dot3','dot',6.41,6.62),
   ('threeofus','three of us (sung)',7.80,8.675),
   ('cult','cult',10.593,11.038),
 ],
 '7144849836880219434': [
   ('sungdash','da da da dash dash (sung)',0.11,2.56),
   ('duh','duh',4.89,5.21), ('dot','dot',5.57,5.93),
   ('threeofus','three of us (sung)',7.188,8.073),
   ('cult','cult (sung)',9.970,10.701),
 ],
 '7156050516676726062': [
   ('dash1','dash',0.51,0.68), ('dash2','dash',1.12,1.61),
   ('dot1','dot',2.68,2.88), ('dot2','dot',3.36,3.66),
   ('threeofus','the three of us are in a',4.94,6.49),
 ],
}

report={}
for id, cuts in CUTS.items():
    y=load(id); dur=len(y)/SR
    words=json.load(open(f'alt/txt/{id}.json'))['transcription']
    segs=[]
    for name, label, t0, t1 in cuts:
        s,t=refine(y,t0,t1,pad=0.06,thresh_ratio=0.10)
        s=max(s,t0-0.06); t=min(t,t1+0.08)
        f0,vfrac=f0_of(y,s,t,fmin=70,fmax=500)
        out=f'alt-{id[:5]}-{name}'
        wdur=write(out,y,s,t)
        check=transcribe(librosa.load(f'alt/samples/{out}.wav',sr=SR,mono=True)[0],0,wdur,pad=0)
        segs.append(dict(file=os.path.abspath(f'alt/samples/{out}.wav'), words=label,
            start=round(s,3), end=round(t,3), dur=round(wdur,3),
            median_f0_hz=round(f0,1) if f0 else None, voiced_frac=round(vfrac,2),
            whisper_recheck=check))
        print(f'{out:32s} {wdur:5.2f}s f0={f0 if f0 else 0:6.1f} v={vfrac:.2f} "{label}" | recheck: {check}')
    report[id]=dict(source_mp4=os.path.abspath(f'alt/raw/{id}.mp4'),
        wav=os.path.abspath(f'alt/wav/{id}.wav'),
        duration=round(dur,2), speech_found=True,
        transcript=' '.join(w['text'] for w in words).strip(),
        transcription_method='whisper.cpp (whisper-cli) ggml-small.en, word-level -ml 1, plus per-burst re-transcription for alignment',
        whistle_detected=False,
        whistle_note='pyin scan 500-3000 Hz found no sustained tonal run >=0.15 s in this clip; the melody here is chanted/spoken, not whistled',
        word_timestamps=[dict(t=w['text'], start=w['offsets']['from']/1000, end=w['offsets']['to']/1000) for w in words],
        bursts=json.load(open('alt/bursts.json'))[id],
        samples=segs)
json.dump(report, open('alt/harvest.json','w'), indent=2)
print('WROTE alt/harvest.json')
