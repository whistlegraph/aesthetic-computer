"""Exercise the real C terminal through a PTY, with no provider or account."""
import fcntl, importlib.util, json, os, shutil, signal, struct, sys, tempfile, termios, time
from pathlib import Path

ROOT=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT.parents[1]/'test'))
spec=importlib.util.spec_from_file_location('bench',ROOT.parents[1]/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

def case(mode,work):
    with tempfile.TemporaryDirectory(prefix='aesel-c-test-') as tmp:
        root=Path(tmp);log=root/'events.jsonl'
        env=dict(os.environ,C_TUI_TEST_MODE=mode,C_TUI_TEST_LOG=str(log),LANG='en_US.UTF-8')
        term=bench.Terminal([os.environ.get('C_TUI_BINARY',str(ROOT/'.build/aesel-c')),'--runtime',shutil.which('python3'),'--bridge',str(ROOT/'fake-bridge.py')],env,root,100)
        try:
            term.until(lambda s:'C prototype' in s)
            work(term,log)
        finally:term.close()

def normal(term,log):
    term.until(lambda s:'Ready fixture' in s)
    term.send('hé🟣');term.until(lambda s:'hé🟣' in s)
    term.send(b'\x7f');term.until(lambda s:'hé' in s and '🟣' not in s)
    term.send('\x15')
    term.send('\x1b[200~paste\ntext\x1b[201~')
    term.until(lambda s:'paste text' in s)
    assert not log.exists(),'pasted newline must not submit'
    term.send('\r');term.until(lambda s:'SABLE 🟣' in s and 'café' in s)
    assert json.loads(log.read_text().splitlines()[0])['text']=='paste text'
    term.send('hold\r');term.until(lambda s:'Holding turn' in s)
    term.send('draft stays');term.until(lambda s:'draft stays' in s)
    term.send('\x03');term.until(lambda s:'Stopped fixture' in s and 'draft stays' in s)
    fcntl.ioctl(term.master,termios.TIOCSWINSZ,struct.pack('HHHH',20,64,0,0))
    term.pump(.2);term.send('\x15/quit\r')
    term.until(lambda s:term.child.poll() is not None)
    assert term.child.returncode==0
    assert b'\x1b[?1049l' in term.raw and b'\x1b[?25h' in term.raw

def blocked(term,log):
    started=time.monotonic();term.send('responsive despite blocked bridge')
    term.until(lambda s:'responsive despite blocked bridge' in s,2)
    assert time.monotonic()-started<1,'provider must not stall editing'
    term.child.send_signal(signal.SIGTERM)
    term.until(lambda s:term.child.poll() is not None,3)
    assert term.child.returncode==0
    assert b'\x1b[?1049l' in term.raw

def disconnected(term,log):
    term.until(lambda s:'Ready fixture' in s);term.send('disconnect\rkept draft')
    term.until(lambda s:'Bridge disconnected' in s and 'kept draft' in s)
    term.send('\r');term.until(lambda s:'draft kept' in s.lower())
    assert 'kept draft' in term.screen.text

def overflow(term,log):
    term.until(lambda s:'Ready fixture' in s);term.send('overflow\r')
    term.until(lambda s:'reply truncated' in s,5)
    term.send('still types');term.until(lambda s:'still types' in s)

for mode,work in [('normal',normal),('blocked',blocked),('normal',disconnected),('normal',overflow),
                  ('oversize',lambda t,l:t.until(lambda s:'offline' in s)),
                  ('partial',lambda t,l:t.until(lambda s:'Bridge disconnected' in s))]:
    case(mode,work);print('PASS:',work.__name__,mode)
