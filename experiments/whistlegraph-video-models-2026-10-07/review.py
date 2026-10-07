import json, subprocess
from pathlib import Path
D=Path(__file__).resolve().parent
FF=str(D.parents[1]/'toolchain/shims/ffmpeg')
FP=str(D.parents[1]/'toolchain/shims/ffprobe')
font='/System/Library/Fonts/Supplemental/Arial.ttf'
names=[('seedance-2.5','Seedance 2.5'),('h3-max','H3 Max'),('wan-3.0','Wan 3.0')]
probes={}
for name,label in names:
 p=D/(name+'.mp4')
 if not p.exists():continue
 data=json.loads(subprocess.check_output([FP,'-v','error','-show_entries','format=duration:stream=codec_type,width,height,r_frame_rate,sample_rate','-of','json',str(p)]))
 probes[name]=data
 dur=float(data['format']['duration'])
 sheet=D/(name+'-frames.jpg')
 if not sheet.exists():
  filt=f"fps={24/dur},scale=240:-2,tile=6x4"
  subprocess.run([FF,'-hide_banner','-loglevel','error','-i',str(p),'-vf',filt,'-frames:v','1','-threads','2',str(sheet)],check=True)
 clip=D/(name+'-review.mp4')
 if not clip.exists():
  title=D/(name+'-label.png')
  subprocess.run(['magick','-background','black','-fill','white','-font',font,'-pointsize','28','-size','540x56','-gravity','West','label:  '+label,str(title)],check=True)
  filt="[0:v]scale=540:960:force_original_aspect_ratio=decrease,pad=540:1016:(ow-iw)/2:56:black,setsar=1,fps=30[v];[v][1:v]overlay=0:0[out]"
  subprocess.run([FF,'-hide_banner','-loglevel','error','-i',str(p),'-i',str(title),'-filter_complex',filt,'-map','[out]','-map','0:a','-af','aresample=48000','-c:v','libx264','-threads','2','-preset','fast','-crf','20','-c:a','aac','-b:a','128k','-ac','2','-movflags','+faststart',str(clip)],check=True)
(D/'media-check.json').write_text(json.dumps(probes,indent=2)+'\n')
if len(probes)==3 and not (D/'comparison.mp4').exists():
 (D/'concat.txt').write_text(''.join(f"file '{name}-review.mp4'\n" for name,label in names))
 subprocess.run([FF,'-hide_banner','-loglevel','error','-n','-f','concat','-safe','0','-i',str(D/'concat.txt'),'-c','copy','-movflags','+faststart',str(D/'comparison.mp4')],check=True)
print(json.dumps(probes,indent=2))
