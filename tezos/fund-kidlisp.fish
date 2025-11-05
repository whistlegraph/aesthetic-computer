#!/usr/bin/env fish
# Open Ghostnet faucet to fund KidLisp wallet
# KidLisp Address: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC

echo "🚰 Opening Ghostnet faucet for KidLisp wallet..."
echo ""
echo "📋 KidLisp Address: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC"
echo ""
echo "Instructions:"
echo "1. Paste the address above into the faucet form"
echo "2. Complete the captcha"
echo "3. Request test XTZ"
echo "4. Wait ~1-2 minutes for confirmation"
echo "5. Run: python3 balance.py kidlisp"
echo ""

"$BROWSER" "https://faucet.ghostnet.teztnets.com/"
