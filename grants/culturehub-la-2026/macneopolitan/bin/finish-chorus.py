#!/usr/bin/env python3
"""Wannadash-derived gentle vocal mastering and measurements of the rendered stems.

Usage: finish-chorus.py /path/chorus-dry
Keeps the dry render; writes chorus-master.wav/mp4 and measured-frequency.png/json.
"""
import argparse
import hashlib
import json
import subprocess
import warnings
from pathlib import Path
import numpy as np
import soundfile as sf
from scipy.signal import butter, sosfilt, resample_poly, spectrogram
with warnings.catch_warnings():
    warnings.simplefilter('ignore')
    import pyworld as pw
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

MEMBERS=['neo','blueberry','frisbee']
COLORS=['#8FD13F','#5A57D3','#F2A7B9']

def run(args):
    return subprocess.run(args,check=True,capture_output=True)

def loudness(path):
    r=run(['ffmpeg','-hide_banner','-i',str(path),'-af','loudnorm=I=-12:TP=-2:LRA=8:print_format=json','-f','null','-'])
    text=r.stderr.decode(); start=text.rfind('{'); j=json.loads(text[start:text.find('}',start)+1])
    return {k:float(j[k]) for k in ['input_i','input_tp','input_lra']}

def finish(prefix, name="chorus", focus_label="“I run warm” — three simultaneous vocal parts"):
    folder=prefix.parent; audio=Path(str(prefix)+'-audio')
    out=folder/'master-stems'; out.mkdir(exist_ok=True)
    dry_report=json.loads(Path(str(prefix)+'-pitch.json').read_text())
    duration=json.loads(prefix.with_suffix('.json').read_text())['total_seconds']
    stems={}
    for m in MEMBERS:
        source=audio/f'{m}-vocal.wav'; dest=out/f'{m}.wav'
        # Wannadash release: slow RMS compression, gentle EQ, dry direct voice.
        filters=f'highpass=f={35 if m=="blueberry" else 65},equalizer=f=3000:t=q:w=0.8:g=-1.5,lowpass=f=11000,acompressor=threshold=0.12:ratio=1.5:attack=30:release=180:knee=4:makeup=1:detection=rms:mix=0.5'
        run(['ffmpeg','-y','-v','error','-i',str(source),'-af',filters,'-c:a','pcm_f32le',str(dest)])
        stems[m],sr=sf.read(dest)
    n=round(duration*sr)
    stems={m:y[:n] for m,y in stems.items()}
    vocals=np.zeros((n,2))
    for m,pan in zip(MEMBERS,[-.22,0,.22]):
        y=stems[m]
        vocals+=y[:,None]*np.array([np.sqrt((1-pan)/2),np.sqrt((1+pan)/2)])
    # Small, dark early reflections. Low bass stays centered and dry.
    room=sosfilt(butter(2,[180,5000],btype='bandpass',fs=sr,output='sos'),sum(stems.values()))
    for delay,amount,ch in [(.031,.06,0),(.047,.06,1),(.071,.035,0),(.089,.035,1),(.131,.015,0),(.149,.015,1)]:
        d=round(delay*sr); vocals[d:,ch]+=room[:-d]*amount
    inst,_=sf.read(audio/'instruments.wav'); mixed=vocals+inst[:n,None]/np.sqrt(2)
    pre=folder/f'{name}-premaster.wav'; sf.write(pre,mixed,sr,subtype='FLOAT')
    before=loudness(pre)
    gain_db=-12.0-before['input_i']
    master=folder/f'{name}-master.wav'
    # One measured static gain, oversampled limiter, native-rate 24-bit output.
    chain=f'volume={gain_db:.6f}dB,aresample={sr*4},alimiter=limit=0.76:attack=4:release=120:level=false:latency=true,aresample={sr}'
    run(['ffmpeg','-y','-v','error','-i',str(pre),'-af',chain,'-c:a','pcm_s24le',str(master)])
    sf.write(folder/f'{name}-acappella-premaster.wav',vocals,sr,subtype='FLOAT')
    run(['ffmpeg','-y','-v','error','-i',str(folder/f'{name}-acappella-premaster.wav'),'-af',chain,'-c:a','pcm_s24le',str(folder/f'{name}-acappella.wav')])
    if prefix.with_suffix('.mp4').exists():
        run(['ffmpeg','-y','-v','error','-i',str(prefix.with_suffix('.mp4')),'-i',str(master),'-map','0:v:0','-map','1:a:0','-c:v','copy','-c:a','aac','-b:a','256k','-shortest',str(folder/f'{name}-master.mp4')])
    y,fs=sf.read(master)
    gain=10**(gain_db/20)
    before_limit=mixed*gain
    # Difference also includes oversampling filters; do not call it exact GR.
    report={'measurement_scope':'F0 measured from processed vocal stems before the shared master limiter; the spectrogram is measured from the actual final master. One mixed waveform has multiple fundamentals.',
            'master_sha256':hashlib.sha256(master.read_bytes()).hexdigest(),
            'master_loudness':loudness(master),'decoded_aac_loudness':loudness(folder/f'{name}-master.mp4') if (folder/f'{name}-master.mp4').exists() else None,'premaster_loudness':before,'static_gain_db':gain_db,
            'sample_peak_dbfs':float(20*np.log10(np.max(np.abs(y)))),
            'samples_above_limiter_ceiling_before_limiting_pct':float(100*np.mean(np.abs(before_limit)>.76)),
            'members':{},'warm_notes':[]}
    step=.01; grid=np.arange(0,n/sr,step); tracks=[]; targets=[]
    for m in MEMBERS:
        x=resample_poly(stems[m],1,2).astype(np.float64)
        f,t=pw.harvest(x,sr//2,f0_floor=50,f0_ceil=700,frame_period=10)
        f=pw.stonemask(x,f,t,sr//2)
        bins=np.minimum(np.round(grid/step).astype(int),len(f)-1); f=f[bins]
        rms=np.sqrt(np.maximum(0,np.convolve(x*x,np.ones(441)/441,mode='same')))
        levels=np.interp(grid,np.arange(len(rms))/(sr/2),rms)
        f[levels<max(.0003,np.max(levels)*.02)]=0
        target=np.full(len(grid),np.nan)
        notes=[a for a in dry_report['notes'] if a['kind']=='vocal' and a['member']==m]
        for note in notes:
            a,b=note['start'],note['end']; span=b-a
            target[(grid>=a+.2*span)&(grid<b-.2*span)]=note['written_midi']
            if note['text']=='I run warm and I am carried':
                values=f[(grid>=a+.2*span)&(grid<b-.2*span)&(f>0)]
                measured=float(np.median(values)) if len(values) else None
                report['warm_notes'].append({'member':m,'start':a,'target_hz':float(440*2**((note['written_midi']-69)/12)),'measured_hz':measured})
        valid=(f>0)&np.isfinite(target)
        errors=1200*np.log2(f[valid]/(440*2**((target[valid]-69)/12)))
        report['members'][m]={'measured_f0_5_95_percentile_hz':np.percentile(f[valid],[5,95]).tolist(),
                             'voiced_note_interior_coverage_pct':float(100*sum(valid)/sum(np.isfinite(target))),
                             'measured_interior_frames_within_50_cents_pct':float(100*np.mean(np.abs(errors)<=50))}
        tracks.append(f); targets.append(target)
    tracks=np.array(tracks); targets=np.array(targets)
    masks=(tracks>0)&np.isfinite(targets)
    pair_results=[]
    for a,b in [(0,1),(0,2),(1,2)]:
        valid=masks[a]&masks[b]
        err=1200*np.log2(tracks[a,valid]/tracks[b,valid])-100*(targets[a,valid]-targets[b,valid])
        pair_results.append({'pair':[MEMBERS[a],MEMBERS[b]],'measured_overlap_seconds':float(sum(valid)*step),
                             'simultaneous_interval_frames_within_50_cents_pct':float(100*np.mean(np.abs(err)<=50))})
    report['simultaneous_harmony']=pair_results
    report['all_three_measured_interior_seconds']=float(np.sum(np.all(masks,axis=0))*step)
    np.savez_compressed(folder/'measured-frequency-tracks.npz',seconds=grid,hz=tracks,target_midi=targets)
    (folder/'measured-frequency.json').write_text(json.dumps(report,indent=2)+'\n')
    plot_frequency(folder,y,fs,grid,tracks,duration,focus_label)
    print(json.dumps(report,indent=2),flush=True)

def plot_frequency(folder,y,fs,grid,tracks,duration,focus_label="“I run warm” — three simultaneous vocal parts"):
    # A common frequency axis makes the bass register and harmony readable.
    fig,axes=plt.subplots(2,1,figsize=(15,8),layout='constrained')
    for ax,(start,stop) in zip(axes,[(0,duration),(5.5,10.5)]):
        mono=y.mean(axis=1); lo,hi=round(start*fs),round(stop*fs)
        freqs,times,power=spectrogram(mono[lo:hi],fs,nperseg=4096,noverlap=3072)
        keep=(freqs>=50)&(freqs<=600)
        ax.pcolormesh(times+start,freqs[keep],10*np.log10(power[keep]+1e-12),vmin=-75,vmax=-30,cmap='Greys',alpha=.55,shading='auto',rasterized=True)
        for i,m in enumerate(MEMBERS):
            mask=(grid>=start)&(grid<=stop)&(tracks[i]>0)
            ax.scatter(grid[mask],tracks[i,mask],s=5,color=COLORS[i],label=m,zorder=3)
        ax.set(ylim=(50,600),xlim=(start,stop),ylabel='Frequency (Hz)')
        ax.set_yscale('log'); ax.set_yticks([55,73.4,110,146.8,220,293.7,440]);ax.set_yticklabels(['55','73','110','147','220','294','440'])
        from matplotlib.ticker import NullLocator
        ax.yaxis.set_minor_locator(NullLocator())
        ax.grid(alpha=.15);ax.legend(loc='upper right',ncol=3)
    axes[0].set_title('Final master spectrum + measured vocal fundamentals',fontsize=17)
    axes[1].set_title(focus_label,fontsize=15)
    axes[1].set_xlabel('Seconds from the first note')
    fig.savefig(folder/'measured-frequency.png',dpi=160);plt.close(fig)

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('prefix',type=Path)
    parser.add_argument('--name',default='chorus')
    parser.add_argument('--focus-label',default='“I run warm” — three simultaneous vocal parts')
    args=parser.parse_args();finish(args.prefix,args.name,args.focus_label)
