"""Interactive ownership fixture; only local pipes and a temporary event log."""
import json,os,sys,time
log=os.environ['NATIVE_TEST_LOG']
def phase(value):os.write(3,('AESEL/1 '+value+'\n').encode())
def note(value):
    with open(log,'a') as f:f.write(json.dumps(value)+'\n')
def output(text):sys.stdout.write(text);sys.stdout.flush()
time.sleep(.2)
phase('gate');output('\x1b[2J\x1b[HGate: press y\r\n')
answer=os.read(0,100)
note({'gate':answer.decode()})
phase('boot');time.sleep(.05)
phase('ready');output('\x1b[2J\x1b[HFull core ready\r\n')
draft=os.read(0,65536);note({'draft':draft.decode()})
output('draft:'+draft.decode()+'\r\n')
while True:
    data=os.read(0,4096)
    if not data:break
    if data==b'\x04':break
    note({'input':list(data)});os.write(1,data)
