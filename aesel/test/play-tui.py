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
    root=Path(tmp);node=shutil.which(os.environ.get('AESEL_TEST_RUNTIME','node'))
    subprocess.run([node,'--input-type=module','-e',
        "import {assemble} from '%s';import {readFileSync,writeFileSync} from 'node:fs';"
        "writeFileSync('%s',assemble(readFileSync('%s','utf8')).bytes)"
        % (ROOT/'media/nes/asm.mjs',root/'game.nes',ROOT/'media/nes/starter.asm')],check=True)
    # A stand-in `open`, so no app comes up on the machine running the test:
    # it has QuickTime, lacks VLC, and writes down what it was asked to open.
    fake=root/'bin';fake.mkdir();calls=root/'open.log'
    (fake/'open').write_text('#!/bin/sh\ncase "$*" in *VLC*|*IINA*) exit 1;; esac\necho "$*" >> "%s"\n' % calls);(fake/'open').chmod(0o755)
    (root/'clip.mp4').write_bytes(b'not really a film')
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
        terminal.send('/play\r')
        terminal.until(lambda text:'Stopped playing' in text and '▀'*20 not in text)
        opened=lambda:calls.read_text().splitlines() if calls.exists() else []
        clip='/clip.mp4'
        terminal.send('/play clip.mp4 vlc\r');terminal.until(lambda text:'VLC is not installed' in text)
        terminal.send('/play clip.mp4\r');terminal.until(lambda text:'Playing clip.mp4 in QuickTime Player' in text)
        terminal.until(lambda text:any(line.startswith('-a QuickTime Player ') and line.endswith(clip) for line in opened()))
        (root/'clip.mp4').write_bytes(b'a second render of it')
        terminal.until(lambda text:any(line.startswith('-g -a QuickTime Player ') and line.endswith(clip) for line in opened()))
        terminal.send('/play\r');terminal.until(lambda text:text.count('Stopped playing')==2)
        terminal.send('/quit\r');terminal.until(lambda text:terminal.child.poll() is not None,10)
        assert terminal.child.returncode==0
        print('PASS: /play runs a cartridge in its core beside the transcript, opens a video in an outside player and reopens it on a write, refuses a missing player and a file nothing plays, and stops')
    finally:
        terminal.close()
