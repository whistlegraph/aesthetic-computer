"""Small VT screen for observing ASCII benchmark markers, without shell echo."""
import codecs
import os
import re
import unicodedata


class Screen:
    def __init__(self, columns=100, rows=30):
        self.columns, self.rows = columns, rows
        self.lines = [[' '] * columns for _ in range(rows)]
        self.x = self.y = 0
        self.saved = (0, 0)
        self.pending = ''
        self.decoder = codecs.getincrementaldecoder('utf-8')('replace')
        self.replies = []

    @property
    def text(self):
        return '\n'.join(''.join(row) for row in self.lines)

    def feed(self, data):
        self.pending += self.decoder.decode(data)
        while self.pending:
            if self.pending.startswith('\x1b'):
                if len(self.pending) < 2:
                    break
                if self.pending[1] == '[':
                    match = re.match(r'\x1b\[([0-?]*)([ -/]*)([@-~])', self.pending)
                    if not match:
                        break
                    self.csi(match[1], match[3])
                    self.pending = self.pending[match.end():]
                    continue
                if self.pending[1] in ']P_^':
                    match = re.search(r'\x07|\x1b\\', self.pending[2:])
                    if not match:
                        break
                    end = 2 + match.end()
                    osc = self.pending[2:2+match.start()]
                    if osc in ('10;?', '11;?'):
                        color = 'ffff/ffff/ffff' if osc.startswith('10') else '0000/0000/0000'
                        self.replies.append(f'\x1b]{osc[:2]};rgb:{color}\x1b\\'.encode())
                    self.pending = self.pending[end:]
                    continue
                if self.pending[1] == '7': self.saved = (self.x, self.y)
                if self.pending[1] == '8': self.x, self.y = self.saved
                self.pending = self.pending[2:]
                continue
            ch, self.pending = self.pending[0], self.pending[1:]
            if ch == '\r': self.x = 0
            elif ch == '\n': self.down()
            elif ch == '\b': self.x = max(0, self.x-1)
            elif ch == '\t': self.x = min(self.columns-1, (self.x//8+1)*8)
            elif ord(ch) >= 32 and not unicodedata.combining(ch):
                if self.x >= self.columns: self.x = 0; self.down()
                self.lines[self.y][self.x] = ch
                width = 2 if unicodedata.east_asian_width(ch) in ('W','F') else 1
                self.x += width

    def down(self):
        self.y += 1
        if self.y >= self.rows:
            self.lines.pop(0); self.lines.append([' ']*self.columns); self.y = self.rows-1

    def csi(self, params, op):
        private = params.startswith('?')
        values = [int(p) if p.isdigit() else 0 for p in params.lstrip('?=>').split(';')]
        n = values[0] or 1
        if op in ('H','f'): self.y = min(self.rows-1,n-1); self.x = min(self.columns-1,(values[1] or 1)-1 if len(values)>1 else 0)
        elif op == 'G': self.x = min(self.columns-1,n-1)
        elif op == 'd': self.y = min(self.rows-1,n-1)
        elif op == 'A': self.y = max(0,self.y-n)
        elif op in ('B','e'): self.y = min(self.rows-1,self.y+n)
        elif op in ('C','a'): self.x = min(self.columns-1,self.x+n)
        elif op == 'D': self.x = max(0,self.x-n)
        elif op == 'E': self.y = min(self.rows-1,self.y+n); self.x = 0
        elif op == 'F': self.y = max(0,self.y-n); self.x = 0
        elif op == 's': self.saved = (self.x,self.y)
        elif op == 'u': self.x,self.y = self.saved
        elif op == 'J':
            if values[0] in (2,3): self.lines = [[' ']*self.columns for _ in range(self.rows)]
            elif values[0] == 0:
                self.lines[self.y][self.x:] = [' ']*(self.columns-self.x)
                for y in range(self.y+1,self.rows): self.lines[y] = [' ']*self.columns
        elif op == 'K':
            a,b = (0,self.columns) if values[0]==2 else (0,self.x+1) if values[0]==1 else (self.x,self.columns)
            self.lines[self.y][a:b] = [' ']*(b-a)
        elif op == 'P': self.lines[self.y] = (self.lines[self.y][:self.x]+self.lines[self.y][self.x+n:]+[' ']*n)[:self.columns]
        elif op == 'X': self.lines[self.y][self.x:min(self.columns,self.x+n)] = [' ']*min(n,self.columns-self.x)
        elif op == '@': self.lines[self.y] = (self.lines[self.y][:self.x]+[' ']*n+self.lines[self.y][self.x:])[:self.columns]
        elif op == 'n' and n == 6: self.replies.append(f'\x1b[{self.y+1};{min(self.x+1,self.columns)}R'.encode())
        elif op == 'c': self.replies.append(b'\x1b[?1;2c')
        elif op == 't' and n == 18: self.replies.append(f'\x1b[8;{self.rows};{self.columns}t'.encode())
