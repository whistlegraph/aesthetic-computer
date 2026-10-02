// Blend the loop boundary; retain the original FAL masters beside the delivery files.
import {existsSync, copyFileSync, renameSync, readFileSync, writeFileSync} from 'node:fs';
import {dirname, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';
const here=dirname(fileURLToPath(import.meta.url));
const out=resolve(here,'../../podcast/out/pals/turnarounds');
for(const slug of ['psycho-dripped','psycho-dripped-pink']) {
 const target=resolve(out,`${slug}.mp4`), source=resolve(out,`${slug}.source.mp4`);
 const temporary=resolve(out,`${slug}.loop.mp4`);
 if(!existsSync(source)) copyFileSync(target,source);
 // Last half-second fades into the original first half-second. Starting the
 // output at t=.5 makes the new end continue directly into its new beginning.
 execFileSync('ffmpeg',['-y','-hide_banner','-loglevel','error','-threads','2','-i',source,
   '-filter_complex_threads','2','-filter_complex',
   '[0:v]trim=end=6,setpts=PTS-STARTPTS,split[a][b];[a]trim=start=0.5,setpts=PTS-STARTPTS[main];[b]trim=end=0.5,setpts=PTS-STARTPTS[head];[main][head]xfade=transition=fade:duration=0.5:offset=5[v]',
   '-map','[v]','-an','-c:v','libx264','-threads','2','-preset','slow','-crf','16','-pix_fmt','yuv420p','-movflags','+faststart',temporary],{stdio:'inherit'});
 renameSync(temporary,target);
 const probe=JSON.parse(execFileSync('ffprobe',['-v','error','-show_entries','stream=width,height,r_frame_rate','-show_entries','format=duration,size','-of','json',target]));
 const recipePath=resolve(here,`${slug}.motion.json`), recipe=JSON.parse(readFileSync(recipePath));
 recipe.delivery={...probe.streams[0],duration:Number(probe.format.duration),bytes:Number(probe.format.size),loopTreatment:'0.5-second cyclic crossfade; original FAL output preserved in .source.mp4',webp:{width:512,fps:15},apng:{width:400,fps:12}};
 writeFileSync(recipePath,JSON.stringify(recipe,null,2)+'\n');
 console.log(`${slug}: loop prepared (${probe.format.duration}s)`);
}
