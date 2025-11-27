#!/usr/bin/env python3
import sys, re, base64, gzip

filename = sys.argv[1] if len(sys.argv) > 1 else '$39j-@fifi-2025.11.27.7.52.32.961.lisp.html'

with open(f'keep-bundles/{filename}', 'r') as f:
    html = f.read()

# Find the base64 content
match = re.search(r"fetch\('data:application/octet-stream;base64,([^']+)'\)", html)
if match:
    b64 = match.group(1)
    data = base64.b64decode(b64)
    decompressed = gzip.decompress(data).decode('utf-8')
    
    # Search for acEMBEDDED
    if 'acEMBEDDED_PAINTING' in decompressed:
        print('✅ Found acEMBEDDED_PAINTING in bundle!')
    else:
        print('❌ acEMBEDDED_PAINTING NOT found in bundle')
    
    if 'acPAINTING_CODE_MAP' in decompressed:
        print('✅ Found acPAINTING_CODE_MAP in bundle!')
    else:
        print('❌ acPAINTING_CODE_MAP NOT found in bundle')
    
    # Check for the intercept code in bios
    if 'Check for embedded painting' in decompressed:
        print('✅ Found embedded painting intercept code in bundle!')
    else:
        print('❌ Embedded painting intercept code NOT found in bundle')
else:
    print('❌ No base64 data found')
