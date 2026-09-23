#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DUBS="$ROOT/voice-takes/dubs"
OUT="$ROOT/out/dub-mix"
MASTER="$ROOT/out/wattajetta-stone-club-distrokid-master.wav"
STEMS="$ROOT/out/wattajetta-stone-club-stems"
TAKE1="$DUBS/2026-08-08T08-04-36Z.caf"
TAKE2="$DUBS/2026-08-08T08-08-27Z.caf"
THIRD="$OUT/aesthetivox-take-2-third-up.wav"
PITCHED="$OUT/aesthetivox-take-2-fifth-up.wav"
DOWN="$OUT/aesthetivox-take-2-octave-down.wav"
VOCALS="$OUT/wattajetta-aesthetivox.wav"
MIX="$OUT/wattajetta-stone-club-aesthetivox-mix.wav"
MP3="$OUT/wattajetta-stone-club-aesthetivox-mix.mp3"

mkdir -p "$OUT"

# A restrained parallel fifth supplies melodic color while the natural takes
# remain the intelligible center. Raw recordings are never modified.
rubberband -q -p 3 "$TAKE2" "$THIRD"
rubberband -q -p 7 "$TAKE2" "$PITCHED"
rubberband -q -p -12 "$TAKE2" "$DOWN"

ffmpeg -y -hide_banner -loglevel warning \
  -i "$TAKE1" -i "$TAKE2" -i "$THIRD" -i "$PITCHED" -i "$DOWN" \
  -filter_complex \
  "[0:a]atrim=start=0.025,asetpts=PTS-STARTPTS,highpass=f=78,lowpass=f=14500,equalizer=f=280:t=q:w=1.1:g=-3,equalizer=f=3400:t=q:w=1.0:g=2.5,deesser=i=0.22:m=0.45:f=0.55,acompressor=threshold=0.07:ratio=3.2:attack=6:release=95:makeup=1.8,volume='if(lt(t,13.913),0.16,if(lt(t,31.304),0.92,if(lt(t,45.217),1.06,if(lt(t,62.609),0.92,if(lt(t,76.522),1.06,if(lt(t,85.217),0.56,if(lt(t,93.913),0.78,if(lt(t,114.783),1.12,0.42))))))))':eval=frame[lead]; \
   [1:a]atrim=start=0.025,asetpts=PTS-STARTPTS,highpass=f=95,lowpass=f=12000,equalizer=f=320:t=q:w=1.0:g=-4,acompressor=threshold=0.08:ratio=4:attack=4:release=80:makeup=1.6,adelay=18,volume='if(lt(t,31.304),0.10,if(lt(t,45.217),0.46,if(lt(t,62.609),0.12,if(lt(t,76.522),0.48,if(lt(t,85.217),0.22,if(lt(t,93.913),0.32,if(lt(t,114.783),0.54,0.18)))))))':eval=frame[double]; \
   [3:a]highpass=f=150,lowpass=f=9000,adelay=34,volume='if(between(t,31.304,45.217)+between(t,62.609,76.522)+between(t,93.913,114.783),0.11,0)':eval=frame[harmony]; \
   [2:a]atrim=start=31.304:end=32.174,asetpts=PTS-STARTPTS,afade=t=in:d=0.012,afade=t=out:st=0.82:d=0.05,adelay=34782,volume=0.26[chop1]; \
   [3:a]atrim=start=62.609:end=63.478,asetpts=PTS-STARTPTS,afade=t=in:d=0.012,afade=t=out:st=0.82:d=0.05,adelay=66087,volume=0.22[chop2]; \
   [4:a]atrim=start=93.913:end=94.783,asetpts=PTS-STARTPTS,afade=t=in:d=0.012,afade=t=out:st=0.82:d=0.05,adelay=95652,volume=0.20[chop3]; \
   [2:a]atrim=start=93.913:end=94.348,asetpts=PTS-STARTPTS,afade=t=in:d=0.008,afade=t=out:st=0.39:d=0.04,asplit=3[s1][s2][s3]; \
   [s1]adelay=99130[st1];[s2]adelay=99565[st2];[s3]adelay=100000[st3]; \
   [lead][double][harmony][chop1][chop2][chop3][st1][st2][st3]amix=inputs=9:duration=longest:normalize=0,aecho=0.8:0.22:95|190:0.07|0.035,loudnorm=I=-20:TP=-5:LRA=12,alimiter=limit=0.92:attack=4:release=70[v]" \
  -map "[v]" -ar 48000 -ac 2 -c:a pcm_f32le "$VOCALS"

# The vocal keys only the competing musical stems. Kick, samples, and Empty
# Trash FX retain their narrative/transient shape.
ffmpeg -y -hide_banner -loglevel warning \
  -i "$STEMS/01-kick.wav" \
  -i "$STEMS/02-water-engine.wav" \
  -i "$STEMS/03-stone-bells-uke.wav" \
  -i "$STEMS/04-disco-bass-squares.wav" \
  -i "$STEMS/05-guitar.wav" \
  -i "$STEMS/06-samples-fx.wav" \
  -i "$STEMS/07-empty-trash-fx.wav" \
  -i "$VOCALS" \
  -filter_complex \
  "[7:a]apad,asplit=5[kw][kb][kd][kg][vox]; \
   [1:a][kw]sidechaincompress=threshold=0.020:ratio=2.4:attack=7:release=145[water]; \
   [2:a][kb]sidechaincompress=threshold=0.018:ratio=3.0:attack=5:release=180[bells]; \
   [3:a][kd]sidechaincompress=threshold=0.022:ratio=2.2:attack=8:release=125[disco]; \
   [4:a][kg]sidechaincompress=threshold=0.018:ratio=3.2:attack=6:release=155[guitar]; \
   [0:a][water][bells][disco][guitar][5:a][6:a]amix=inputs=7:duration=longest:normalize=0,acompressor=threshold=0.125:ratio=1.8:attack=16:release=140:makeup=1.08,equalizer=f=54:t=q:w=0.8:g=1.6,volume=0.944[bed]; \
   [bed][vox]amix=inputs=2:duration=first:normalize=0,alimiter=limit=0.89:attack=5:release=80[mix]" \
  -map "[mix]" -ar 48000 -c:a pcm_s24le "$MIX"

ffmpeg -y -hide_banner -loglevel warning -i "$MIX" \
  -c:a libmp3lame -b:a 320k "$MP3"

printf '%s\n' "$VOCALS" "$MIX" "$MP3"
