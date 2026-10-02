"""Check input bursts, editing, paste and submission in the real animated TUI."""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import tempfile

ROOT=Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('bench_tui',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

with tempfile.TemporaryDirectory(prefix='aesel-input-',dir='/tmp') as tmp:
    root=Path(tmp);log=root/'events.jsonl'
    env={key:os.environ[key] for key in ('PATH','TMPDIR','LANG') if key in os.environ}
    env.update(HOME=tmp,TERM='xterm-256color',AESEL_THEME='own',SLAB_TERMINAL_TTY='ttys999',
        SLAB_HOME=str(root/'slab'),AESEL_TEST_LOG=str(log),AESEL_HISTORY_DIR=str(root/'history'),
        AESEL_TRANSCRIPTS=str(root/'transcripts'),AESEL_CONFIG_DIR=str(root/'.config/aesel'))
    command=[shutil.which(os.environ.get('AESEL_TEST_RUNTIME','node')),'--import',
        str(ROOT/'test/input-tui-fixture.mjs'),str(ROOT/'src'/os.environ.get('AESEL_TEST_ENTRY','tui.mjs')),
        '--cwd',tmp,'--pro','--backend','codex','--no-autopublish']
    terminal=bench.Terminal(command,env,tmp,100)
    def events():
        return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []
    try:
        terminal.until(lambda text:'@tester' in text and any(e['event']=='connected' for e in events()))
        # One terminal write may be split by the OS. Bound paints per actual
        # stdin chunk, not per write, and allow a scheduled background frame.
        start=len(events());burst='Burst'+''.join(str(i%10) for i in range(64))+'END'
        terminal.send(burst);terminal.until(lambda text:burst in text)
        rows=events()[start:]
        chunks=[e['chunk'] for e in rows if e['event']=='input']
        assert chunks
        for chunk in chunks:
            paints=sum(e['event']=='paint' and e['chunk']==chunk for e in rows)
            assert paints<=2, f'{paints} paints for one input chunk'
        # Cursor commands in the same chunk must retain their order.
        terminal.send('\x1b[D\x1b[D\x7fZ\x1b[C\x1b[C')
        terminal.until(lambda text:burst[:-3]+'ZND' in text)
        terminal.send('\x15');terminal.until(lambda text:'Burst' not in text)
        terminal.send('\x1b[200~  pasted ☃ text  \x1b[201~')
        terminal.until(lambda text:'pasted ☃ text' in text)
        terminal.send('\r')
        terminal.until(lambda text:any(e['event']=='prompt' for e in events()))
        assert [e['text'] for e in events() if e['event']=='prompt']==['pasted ☃ text']
        terminal.until(lambda text:'Settings request handled.' in text)
        terminal.send('/quit\r');terminal.until(lambda text:terminal.child.poll() is not None,10)
        assert terminal.child.returncode==0
        print('PASS: input bursts coalesce paints; cursor edits and Unicode paste retain order; submission trims surrounding spaces')
    finally:
        terminal.close()
