import importlib.util,json,os,shutil,sys,tempfile
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[1]
sys.path.insert(0,str(ROOT/'test'))
spec=importlib.util.spec_from_file_location('bench',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)
with tempfile.TemporaryDirectory(prefix='native-features-') as tmp:
    root=Path(tmp);log=root/'events.jsonl'
    env=dict(os.environ,HOME=tmp,TERM='xterm-256color',NO_COLOR='1',SLAB_HOME=str(root/'slab'),
        AESEL_TEST_LOG=str(log),AESEL_CONFIG_DIR=str(root/'.config/aesel'),AESEL_HISTORY_DIR=str(root/'history'),AESEL_TRANSCRIPTS=str(root/'transcripts'))
    command=[str(HERE/'.build/aesel-native'),'--',shutil.which('bun'),'--preload',str(HERE/'full-feature-fixture.mjs'),
        str(ROOT/'src/launch.mjs'),'--cwd',tmp,'--pro','--backend','codex','--no-autopublish']
    term=bench.Terminal(command,env,tmp,100)
    def events():return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []
    try:
        term.until(lambda s:'Aesel starting' in s)
        term.send('discard this\x15opening prompt\rqueued draft')
        term.until(lambda s:any(e.get('event')=='native-prompt' for e in events()) and 'queued draft' in s)
        assert [e['text'] for e in events() if e['event']=='native-prompt']==['opening prompt']
        term.send('\x15/ask on\r');term.until(lambda s:'Asking before each action' in s)
        term.send('approval test\r');term.until(lambda s:'fixture-action' in s and 'Deny' in s)
        assert not any(e['event']=='native-approval' for e in events())
        term.send('n');term.until(lambda s:'Approval answered' in s)
        assert next(e for e in events() if e['event']=='native-approval')['result']=={'decision':'decline'}
        assert '**Approval answered**' not in term.screen.text
        term.send('/settings\r');term.until(lambda s:'Provider' in s or 'Engine' in s)
        term.send('\x1b');term.send('/quit\r');term.until(lambda s:term.child.poll() is not None,10)
        assert term.child.returncode==0
        print('PASS: opening edits replay once, next draft survives, approval waits for explicit denial, Markdown and Settings render')
    finally:term.close()
