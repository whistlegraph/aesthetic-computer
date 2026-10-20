#!/bin/zsh
# mix2.sh — deep house mix: vox-grid + deep/ stems → mix2/master.wav
# per-stem LUFS targets override as name=value, e.g. zsh mix2.sh kick=-17 vox=-15
set -e
mkdir -p mix2 # run from the work dir, like the other scripts
lufs() { ffmpeg -i "$1" -af ebur128 -f null - 2>&1 | grep -E '^\s+I:' | awk '{print $2}'; }
typeset -A T; T=(vox -16 kick -15.5 sub -21.5 pad -28 keys -40 ep -24 bass -25 gtr -28 str -28 arp -26.5 shaker -29 rim -31 clap -26 fx -28)
for k in "$@"; do T[${k%%=*}]=${k#*=}; done
END=$(node -e 'const s=require("./deep/song.json");console.log((s.total).toFixed(2))')
# vocal: house vocal-lead chain (acdsp wants 16-bit), short plate, de-ess
ffmpeg -v error -y -i vox-grid.wav -af "aresample=48000,pan=stereo|c0=c0|c1=c0" -c:a pcm_s16le mix2/vox-in.wav
node -e 'import("/Users/jas/aesthetic-computer/pop/lib/master.mjs").then(({processWav,presets})=>{const r=processWav("mix2/vox-in.wav","mix2/vox-chain.wav",presets.vocalLead());if(!r.ok){console.error(r.stderr);process.exit(1)}})'
ffmpeg -v error -y -i mix2/vox-chain.wav -af "highpass=f=95,aecho=0.8:0.6:61|97:0.09|0.05,deesser=i=0.3" -c:a pcm_s16le mix2/vox.wav
args=(); fc=""; i=0
for s in vox kick sub pad keys ep bass gtr str arp shaker rim clap fx; do
  f=deep/$s.wav; [[ $s == vox ]] && f=mix2/vox.wav
  l=$(lufs $f); g=$(echo "${T[$s]} - ($l)" | bc)
  args+=(-i $f); fc+="[${i}:a]aresample=48000,volume=${g}dB[$s];"; i=$((i+1))
done
fc+="[kick]asplit=3[kd][kk1][kk2];[kk1]asplit=6[kv][ke][kb][kg][ks][ka];"
# the kick pumps the vocal a little (about 2-3 dB) and the instruments more
fc+="[vox]apad=whole_dur=${END}[vp];[vp][kv]sidechaincompress=threshold=0.06:ratio=2.2:attack=4:release=180[vpump];[vpump]asplit[v][vkey];"
fc+="[ep][ke]sidechaincompress=threshold=0.04:ratio=5:attack=4:release=200[epp];[bass][kb]sidechaincompress=threshold=0.05:ratio=5:attack=3:release=150[bp];"
fc+="[gtr][kg]sidechaincompress=threshold=0.04:ratio=4:attack=4:release=180[gp];[str][ks]sidechaincompress=threshold=0.04:ratio=5:attack=6:release=240[sp];"
fc+="[bp]highpass=f=60,lowpass=f=3500[bpf];[epp]highpass=f=150[epf];[gp]highpass=f=180[gpf];[sp]highpass=f=120,lowpass=f=7000[spf];[arp][ka]sidechaincompress=threshold=0.05:ratio=3:attack=5:release=180,highpass=f=200[apf];"
fc+="[pad][keys][epf][gpf][spf][apf]amix=inputs=6:normalize=0[bed];[bed][vkey]sidechaincompress=threshold=0.08:ratio=2:attack=25:release=300[bedd];"
fc+="[kd][clap][rim]amix=inputs=3:normalize=0,acompressor=threshold=0.2:ratio=3:attack=8:release=80:makeup=1.3,asoftclip=type=tanh:threshold=0.85[drums];"
fc+="[sub]lowpass=f=1800[subf];[kk2]anullsink;"
fc+="[v][drums][subf][bpf][bedd][shaker][fx]amix=inputs=7:duration=longest:normalize=0,highpass=f=25,acompressor=threshold=0.12:ratio=2:attack=20:release=150:makeup=1.4,volume=-6dB,atrim=end=${END},afade=t=out:st=$(echo "$END - 2.2" | bc):d=2.2[m]"
ffmpeg -v error -y "${args[@]}" -filter_complex "$fc" -map "[m]" -c:a pcm_s16le mix2/mix-pre.wav
node -e 'import("/Users/jas/aesthetic-computer/pop/lib/master.mjs").then(({processWav,presets})=>{const r=processWav("mix2/mix-pre.wav","mix2/mix-bus.wav",presets.danceMasterBus({in_db:-3,out_db:3,iron:0.35}));if(!r.ok){console.error(r.stderr);process.exit(1)}})'
master() { ffmpeg -v error -y -i mix2/mix-bus.wav -af "volume=${1}dB,aresample=192000,alimiter=limit=0.63:attack=1.5:release=70:level=false,aresample=48000" -c:a pcm_s24le mix2/master.wav; }
ml=$(lufs mix2/mix-bus.wav); g=$(echo "-10 - ($ml)" | bc); master $g
l2=$(lufs mix2/master.wav); g=$(echo "$g + (-10 - ($l2))" | bc); master $g
pk=$(ffmpeg -i mix2/mix-bus.wav -af astats=measure_perchannel=0:measure_overall=Peak_level -f null - 2>&1 | grep "Peak level" | awk '{print $NF}')
echo "bus $ml LUFS, peak $pk → gain ${g}dB (limiter ≈ $(echo "$pk + $g + 4.0" | bc) dB on the loudest hit)"
