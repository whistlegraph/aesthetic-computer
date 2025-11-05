#!/usr/bin/env fish
# Test the Tezos setup

echo "🧪 Testing Tezos environment..."
echo ""

# Test Python imports
echo "📦 Testing Python packages..."
python -c "
import sys
try:
    import pytezos
    print('✅ pytezos:', pytezos.__version__)
except ImportError as e:
    print('❌ pytezos:', e)
    sys.exit(1)

try:
    import requests
    print('✅ requests:', requests.__version__)
except ImportError as e:
    print('❌ requests:', e)
    sys.exit(1)

try:
    from dotenv import load_dotenv
    print('✅ python-dotenv: installed')
except ImportError as e:
    print('❌ python-dotenv:', e)
    sys.exit(1)
"

echo ""
echo "🔧 Testing CLI tools..."

# Test octez-client
if command -v octez-client &> /dev/null
    echo "✅ octez-client:"(octez-client --version | head -n 1)
else
    echo "❌ octez-client not found"
end

echo ""
echo "📁 Checking files..."
test -f requirements.txt && echo "✅ requirements.txt" || echo "❌ requirements.txt missing"
test -f .gitignore && echo "✅ .gitignore" || echo "❌ .gitignore missing"
test -f create_kidlisp_wallet.py && echo "✅ create_kidlisp_wallet.py" || echo "❌ create_kidlisp_wallet.py missing"
test -f balance.py && echo "✅ balance.py" || echo "❌ balance.py missing"

echo ""
echo "🎉 All tests complete!"
