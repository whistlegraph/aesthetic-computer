"""Exercise real TUI settings handoff with offline engines and no user stores."""
import os, pty, subprocess, tempfile, json, time, select, shutil, fcntl, termios, struct
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory(prefix='aesel-harness-') as tmp:
 root=Path(tmp);piece=root/'piece.mjs';source='export function paint({wipe}) { wipe(0); }\n';piece.write_text(source)
 master,slave=pty.openpty();fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',28,100,0,0))
 log=root/'events.jsonl';env=dict(os.environ,TERM='xterm-256color',NO_COLOR='1',SLAB_HOME=str(root/'slab'),EASEL_TEST_LOG=str(log),EASEL_HISTORY_DIR=str(root/'history'),EASEL_DESKTOP='1',EASEL_DESKTOP_SESSION=str(root/'session.json'))
 child=subprocess.Popen([shutil.which('node'),'--import',str(ROOT/'test/harness-tui-fixture.mjs'),str(ROOT/'src/tui.mjs'),'--cwd',tmp,'--piece',str(piece),'--backend','claude','--no-autopublish'],stdin=slave,stdout=slave,stderr=slave,env=env);os.close(slave)
 output=bytearray()
 def wait_for(predicate,timeout=12):
  until=time.monotonic()+timeout
  while time.monotonic()<until:
   if select.select([master],[],[],.05)[0]:
    try:output.extend(os.read(master,65536))
    except OSError:break
   if predicate():return
  raise AssertionError(output.decode(errors='replace')[-7000:])
 def rows():
  return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []
 def send(value):os.write(master,(value+'\r').encode())
 try:
  wait_for(lambda:any(r['event']=='connected' for r in rows()))
  context=rows()[0]['context'];assert '3+3' in context and 'aesel_settings' in context
  send('switch provider');wait_for(lambda:len([r for r in rows() if r['event']=='connected'])==2)
  queued=next(r['result'] for r in rows() if r['event']=='tool');assert queued['status']=='queued' and queued['provider']=='claude'
  send('read settings');wait_for(lambda:any(r['event']=='read' for r in rows()));assert rows()[-1]['result']['provider']=='codex'
  send('fail switch');wait_for(lambda:b'Fixture provider unavailable' in output)
  old=len(rows());send('read settings');wait_for(lambda:len(rows())>old);assert rows()[-1]['result']['provider']=='codex'
  send('open settings');wait_for(lambda:b'easel-settings:open' in output)
  assert piece.read_text()==source
  send('/quit');wait_for(lambda:child.poll() is not None);assert child.returncode==0
  print('PASS real TUI: prompt routing, queued provider handoff, failed-switch rollback, direct Settings open, artwork unchanged')
 finally:
  if child.poll() is None:child.kill();child.wait(timeout=5)
  os.close(master)
