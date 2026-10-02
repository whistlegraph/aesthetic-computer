"""Exercise the real TUI's recovery and input queue in an isolated PTY."""
import os, pty, subprocess, tempfile, json, time, select, shutil, fcntl, termios, struct
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory(prefix='aesel-network-', dir='/tmp') as tmp:
    root=Path(tmp); log=root/'events.jsonl'
    master,slave=pty.openpty()
    fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',28,100,0,0))
    env=dict(os.environ,TERM='xterm-256color',NO_COLOR='1',SLAB_TERMINAL_TTY='ttys999',
             SLAB_HOME=str(root/'slab'),AESEL_TEST_LOG=str(log),AESEL_HISTORY_DIR=str(root/'history'),
             AESEL_TRANSCRIPTS=str(root/'transcripts'),AESEL_CONFIG_DIR=str(root/'.config/aesel'),
             NODE_OPTIONS='--import='+str(ROOT/'test/network-tui-fixture.mjs'))
    for key in ['AESEL_DESKTOP','AESEL_DESKTOP_SESSION','AESEL_SESSION_ID','HOME']:
        env.pop(key,None)
    child=subprocess.Popen([shutil.which(os.environ.get('AESEL_TEST_RUNTIME','node')), '--import', str(ROOT/'test/network-tui-fixture.mjs'),str(ROOT/'src'/os.environ.get('AESEL_TEST_ENTRY','tui.mjs')),'--cwd',tmp,'--pro','--backend','codex','--no-autopublish'],stdin=slave,stdout=slave,stderr=slave,env=env)
    os.close(slave);output=bytearray()
    def rows():
        return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []
    def wait_for(predicate,timeout=12):
        until=time.monotonic()+timeout
        while time.monotonic()<until:
            if select.select([master],[],[],.025)[0]:
                try: output.extend(os.read(master,65536))
                except OSError: break
            if predicate(): return
        raise AssertionError(output.decode(errors='replace')[-7000:])
    def send(text): os.write(master,(text+'\r').encode())
    def done(mode): return any(r.get('event')=='done' and r.get('mode')==mode for r in rows())
    def prompts(): return [r for r in rows() if r['event']=='prompt']
    def continues(): return [r for r in prompts() if r['text'].startswith('The connection interrupted this turn.')]
    try:
        wait_for(lambda:any(r['event']=='connect' for r in rows()))
        for mode in ['before start','terminal failure','bridge dies','provider stalls']:
            count=len(continues());send(mode);wait_for(lambda:done(mode))
            assert len(continues())==count+1, (mode,prompts())
        assert all(r.get('resume')=='fixture' for r in rows() if r['event']=='connect' and r['serial']>1)
        count=len(continues());send('provider retries');wait_for(lambda:done('provider retries'))
        assert len(continues())==count, 'Do not overlap the provider retry'
        send('permanent');wait_for(lambda:any(r['event']=='permanent' for r in rows()))
        send('after permanent');wait_for(lambda:done('after permanent'))
        assert len(continues())==count, 'Do not retry authentication errors'
        send('exhaust');send('queued after exhaust')
        wait_for(lambda:b'Recovery paused' in output or b'Connection recovery paused' in output)
        assert len(continues())==count+3, 'Bound recovery attempts'
        assert not any(r['text']=='queued after exhaust' for r in prompts()), 'Preserve queue behind failed request'
        send('/retry');wait_for(lambda:done('queued after exhaust'))
        assert sum(r['text']=='queued after exhaust' for r in prompts())==1
        count=len(continues());start=len(output);send('cancel retry')
        wait_for(lambda:b'Retry 1/3' in output[start:])
        os.write(master,b'\x03');wait_for(lambda:b'Recovery stopped' in output[start:])
        assert len(continues())==count
        os.write(master,'unfinished draft'.encode())
        send('') # This submitted message must remain queued while recovery is stopped.
        send('/retry');wait_for(lambda:done('unfinished draft'))
        start=len(output);send('quit during retry');wait_for(lambda:b'Retry 1/3' in output[start:])
        send('retained on quit');send('/quit');wait_for(lambda:child.poll() is not None)
        saved=[json.loads((root/'.aesel/session.json').read_text())]
        assert len(saved)==1 and saved[0]['recovery']['text']=='quit during retry', saved
        assert saved[0]['ui']['queued']==['retained on quit']

        assert child.returncode==0
        print('PASS: same-thread recovery after start, stream, bridge and stalled-provider failures; provider retry stays singular; bounded retries; auth errors stop; Ctrl-C and /retry preserve queued input')
    finally:
        if child.poll() is None:child.kill();child.wait(timeout=5)
        os.close(master)
