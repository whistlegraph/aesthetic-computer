#!/usr/bin/env python3
import re, gzip, base64, sys

bundle_file = sys.argv[1] if len(sys.argv) > 1 else '$39j-@fifi-2025.11.27.7.52.32.961.lisp.html'

with open(bundle_file, 'r') as f:
    content = f.read()

match = re.search(r"data:application/gzip;base64,([^']+)", content)
b64_data = match.group(1)
compressed = base64.b64decode(b64_data)
decompressed = gzip.decompress(compressed).decode('utf-8')

# Try different patterns for load-bitmap handler
patterns = [
    'if("load-bitmap"===',
    '"load-bitmap"===g',
    'load-bitmap"===',
    'acEMBEDDED_PAINTING_BITMAPS&&',
]

for pattern in patterns:
    idx = decompressed.find(pattern)
    if idx != -1:
        print(f'Found pattern: {pattern}')
        print('='*50)
        # Show 200 chars before and 1500 after
        start = max(0, idx - 100)
        print(decompressed[start:idx+1500])
        print('='*50)
        break
else:
    print('No pattern found')
    
# Also save the full decompressed content for inspection
with open('decompressed_bundle.js', 'w') as f:
    f.write(decompressed)
print(f'\\nSaved full decompressed content to decompressed_bundle.js ({len(decompressed)} chars)')
