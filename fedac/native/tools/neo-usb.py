#!/usr/bin/env python3
"""Read T8140 AC OS diagnostics through the connected Mac's DebugUSB port."""
import argparse
from pathlib import Path
import re
import struct
import time
import zlib

MAX_REPLY = 2 * 1024 * 1024
HEADER = re.compile(rb"ACOS1 ([A-Z]+) ([0-9]+) ([0-9a-f]{8})\n")


def read_reply(port, deadline):
    line = bytearray()
    while time.monotonic() < deadline:
        c = port.read(1)
        if not c:
            continue
        line += c
        if c != b"\n":
            if len(line) > 512:
                line.clear()
            continue
        match = HEADER.fullmatch(bytes(line))
        line.clear()
        if not match:
            continue
        kind, size, checksum = match.groups()
        size = int(size)
        if size > MAX_REPLY:
            raise ValueError(f"Reply exceeds limit: {size}")
        data = bytearray()
        while len(data) < size and time.monotonic() < deadline:
            data.extend(port.read(min(16384, size - len(data))))
        if len(data) != size:
            raise TimeoutError(f"Incomplete reply: {len(data)}/{size}")
        if zlib.crc32(data) != int(checksum, 16):
            raise ValueError("Reply CRC mismatch")
        footer = bytearray()
        while len(footer) < 11 and time.monotonic() < deadline:
            footer.extend(port.read(11 - len(footer)))
        if bytes(footer) != b"\nACOS1 END\n":
            raise ValueError("Missing reply footer")
        return kind.decode(), bytes(data)
    raise TimeoutError("No AC OS DebugUSB response")


def ppm_to_png(ppm):
    match = re.match(rb"P6\n([0-9]+) ([0-9]+)\n255\n", ppm)
    if not match:
        raise ValueError("Unsupported framebuffer format")
    width, height = map(int, match.groups())
    pixels = ppm[match.end():]
    if not width or not height or len(pixels) != width * height * 3:
        raise ValueError("Invalid framebuffer dimensions")
    def chunk(kind, data):
        return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data))
    rows = b''.join(b'\0' + pixels[y*width*3:(y+1)*width*3] for y in range(height))
    return (b'\x89PNG\r\n\x1a\n' +
            chunk(b'IHDR', struct.pack('>IIBBBBB', width, height, 8, 2, 0, 0, 0)) +
            chunk(b'IDAT', zlib.compress(rows)) + chunk(b'IEND', b''))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['ping', 'status', 'log', 'dmesg', 'frame', 'checks', 'put', 'restart'])
    parser.add_argument('--port', default='/dev/cu.kis-00120000-ch-0')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--timeout', type=float, default=60)
    parser.add_argument('--src', type=Path, help='put: local file to send')
    parser.add_argument('--dest', help='put: target path (/pieces/*.mjs|.lisp or /usr/lib/*.mjs only)')
    args = parser.parse_args()
    import serial
    wanted = 'PONG' if args.command == 'ping' else args.command.upper()
    payload = b''
    header = f'\nACOS {args.command.upper()}\n'
    if args.command == 'put':
        if not args.src or not args.dest:
            parser.error('put needs --src and --dest')
        payload = args.src.read_bytes()
        header = f'\nACOS PUT {args.dest} {len(payload)} {zlib.crc32(payload):08x}\n'
    with serial.Serial(args.port, 115200, timeout=0.2, write_timeout=30) as port:
        port.reset_input_buffer()
        port.write(header.encode())
        for off in range(0, len(payload), 4096):  # the target drains a 2 KiB FIFO
            port.write(payload[off:off + 4096])
            port.flush()
        deadline = time.monotonic() + args.timeout
        while True:
            kind, data = read_reply(port, deadline)
            if kind == 'ERROR':
                raise SystemExit(data.decode(errors='replace'))
            if kind == wanted:
                break
    if args.command == 'frame':
        output = args.output or Path('frisbee-frame.png')
        output.write_bytes(ppm_to_png(data))
        print(output.resolve())
    else:
        if args.output:
            args.output.write_bytes(data)
        print(data.decode(errors='replace'), end='\n' if not data.endswith(b'\n') else '')


if __name__ == '__main__':
    main()
