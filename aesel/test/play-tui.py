"""Check /play in the real TUI: a built cartridge runs in its core beside the transcript, and stops."""
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT=Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('bench_tui',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

with tempfile.TemporaryDirectory(prefix='aesel-play-',dir='/tmp') as tmp:
    ffmpeg=next(p for p in ('/opt/homebrew/bin/ffmpeg','/usr/local/bin/ffmpeg','/usr/bin/ffmpeg',shutil.which('ffmpeg')) if p and os.path.exists(p))
    root=Path(tmp);node=shutil.which(os.environ.get('AESEL_TEST_RUNTIME','node'))
    subprocess.run([node,'--input-type=module','-e',
        "import {assemble} from '%s';import {readFileSync,writeFileSync} from 'node:fs';"
        "writeFileSync('%s',assemble(readFileSync('%s','utf8')).bytes)"
        % (ROOT/'media/nes/asm.mjs',root/'game.nes',ROOT/'media/nes/starter.asm')],check=True)
    # A stand-in `open`, so no app comes up on the machine running the test:
    # it has QuickTime, lacks VLC, and writes down what it was asked to open.
    fake=root/'bin';fake.mkdir();calls=root/'open.log'
    (fake/'open').write_text('#!/bin/sh\ncase "$*" in *VLC*|*IINA*) exit 1;; esac\necho "$*" >> "%s"\n' % calls);(fake/'open').chmod(0o755)
    (fake/'osascript').write_text('#!/bin/sh\necho "osascript $*" >> "%s"\n' % calls);(fake/'osascript').chmod(0o755)
    subprocess.run([ffmpeg,'-v','error','-f','lavfi','-i','testsrc2=size=64x48:rate=20:duration=1','-pix_fmt','yuv420p',str(root/'clip.mp4')],check=True)
    env={key:os.environ[key] for key in ('PATH','TMPDIR','LANG') if key in os.environ}
    env['PATH']=str(fake)+os.pathsep+env['PATH']
    env.update(HOME=tmp,TERM='xterm-256color',COLORTERM='truecolor',AESEL_THEME='own',SLAB_TERMINAL_TTY='ttys999',
        SLAB_HOME=str(root/'slab'),AESEL_TEST_LOG=str(root/'events.jsonl'),AESEL_HISTORY_DIR=str(root/'history'),
        AESEL_TRANSCRIPTS=str(root/'transcripts'),AESEL_CONFIG_DIR=str(root/'.config/aesel'))
    command=[node,'--import',str(ROOT/'test/harness-tui-fixture.mjs'),str(ROOT/'src'/os.environ.get('AESEL_TEST_ENTRY','tui.mjs')),
        '--cwd',tmp,'--pro','--backend','codex','--no-autopublish']
    terminal=bench.Terminal(command,env,tmp,100)
    try:
        terminal.until(lambda text:'@tester' in text)
        terminal.send('/play notes.txt\r');terminal.until(lambda text:'No core plays notes.txt' in text)
        terminal.send('/play game.nes\r')
        terminal.until(lambda text:'Playing game.nes in the NES core' in text and '▀'*20 in text)
        # The game has the keys: letters do not reach the prompt until Esc.
        terminal.send('qqq');terminal.send('\x1b');terminal.until(lambda text:'Keys go to the prompt' in text)
        assert 'qqq' not in terminal.pump(.2)
        terminal.send('/play\r')
        terminal.until(lambda text:'Stopped playing' in text and '▀'*20 not in text)
        opened=lambda:calls.read_text().splitlines() if calls.exists() else []
        clip='/clip.mp4'
        # A film loops here in the terminal; an app is by name, and the choice is kept.
        terminal.send('/play clip.mp4 vlc\r');terminal.until(lambda text:'VLC is not installed' in text)
        terminal.send('/play clip.mp4\r');terminal.until(lambda text:'Playing clip.mp4 in the terminal' in text and '\u2580'*20 in text)
        terminal.send('/play\r');terminal.until(lambda text:text.count('Stopped playing')==2 and '\u2580'*20 not in text)
        terminal.send('/play clip.mp4 quicktime\r');terminal.until(lambda text:'Playing clip.mp4 in QuickTime Player' in text)
        terminal.until(lambda text:any(line.startswith('osascript ') and 'set looping of film to true' in line and 'activate' in line and line.endswith(clip) for line in opened()))
        subprocess.run([ffmpeg,'-v','error','-y','-f','lavfi','-i','testsrc2=size=64x48:rate=20:duration=2','-pix_fmt','yuv420p',str(root/'clip.mp4')],check=True)
        terminal.until(lambda text:any(line.startswith('osascript ') and 'activate' not in line and line.endswith(clip) for line in opened()))
        terminal.send('/play\r');terminal.until(lambda text:text.count('Stopped playing')==3)
        terminal.send('/quit\r');terminal.until(lambda text:terminal.child.poll() is not None,10)
        assert terminal.child.returncode==0
        print('PASS: /play runs a cartridge in its core beside the transcript, gives the game the keys until Esc, loops a film in the terminal, opens one looping in an outside player and again on a write, refuses a missing player and a file nothing plays, and stops')
    finally:
        terminal.close()
