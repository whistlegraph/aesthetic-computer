#!/usr/bin/env fish
# Cleanup unused Netlify environment variables
# Run this to remove unused vars and free up space

echo "🧹 Cleaning up unused Netlify environment variables..."
echo ""

set unused_vars ART_DOMAIN WAND_DOMAIN AWS_LAMBDA_JS_RUNTIME CLOUDFLARE_BROWSER_EDIT SOTCE_AUTH0_DOMAIN

for var in $unused_vars
    echo "🗑️  Removing $var..."
    netlify env:unset $var --context production
    netlify env:unset $var --context deploy-preview
    netlify env:unset $var --context branch-deploy
end

echo ""
echo "✅ Cleanup complete!"
echo ""
echo "📊 Checking new size..."
netlify env:list --json | node -e '
const env = JSON.parse(require("fs").readFileSync(0, "utf-8"));
let total = 0;
Object.entries(env).forEach(([key, value]) => {
  total += key.length + value.length + 2;
});
console.log(`💾 New total: ${total} bytes (${(total/1024).toFixed(2)} KB)`);
console.log(`⚠️  AWS Lambda limit: 4096 bytes (4 KB)`);
if (total < 4096) {
  console.log(`✅ Under limit! ${4096 - total} bytes of headroom.`);
} else {
  console.log(`❌ Still over by ${total - 4096} bytes.`);
}
'
