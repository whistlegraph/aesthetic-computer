"""Offline test peer for native framing and terminal behavior."""
import json, os, struct, sys, time

def emit(kind,text=''):
    body=text.encode(); sys.stdout.buffer.write(kind.encode()+struct.pack('!I',len(body))+body);sys.stdout.buffer.flush()

mode=os.environ.get('C_TUI_TEST_MODE','normal')
if mode=='blocked':time.sleep(60)
elif mode=='oversize':
    sys.stdout.buffer.write(b'D'+struct.pack('!I',65537));sys.stdout.buffer.flush();time.sleep(60)
elif mode=='partial':
    sys.stdout.buffer.write(b'D\x00\x00');sys.stdout.buffer.flush()
else:
    emit('S','Ready fixture');emit('H','fixture-thread')
    while True:
        header=sys.stdin.buffer.read(5)
        if len(header)<5:break
        size=struct.unpack('!I',header[1:])[0]
        text=sys.stdin.buffer.read(size).decode()
        log=os.environ.get('C_TUI_TEST_LOG')
        if log:
            with open(log,'a') as out:out.write(json.dumps({'kind':chr(header[0]),'text':text})+'\n')
        if header[:1]==b'P':
            if text=='disconnect':break
            emit('B','1')
            if text=='hold':emit('S','Holding turn');continue
            if text=='overflow':emit('D','x'*60000)
            if text=='overflow':
                for _ in range(10):emit('D','x'*60000)
            # Deliberately split a UTF-8 packet across writes.
            body='SABLE 🟣 café'.encode();packet=b'D'+struct.pack('!I',len(body))+body
            for byte in packet:sys.stdout.buffer.write(bytes([byte]));sys.stdout.buffer.flush()
            emit('S','Ready fixture');emit('B','0')
        elif header[:1]==b'I':emit('S','Stopped fixture');emit('B','0')
        elif header[:1]==b'R':emit('D','Retry fixture');emit('B','0')
