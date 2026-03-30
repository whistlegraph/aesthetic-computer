#!/usr/bin/env fish
# DNS Cutover — Update all Netlify-pointing records to the new lith droplet IP
#
# Usage: fish scripts/dns-cutover.fish <NEW_IP>
#
# This updates 36 DNS records across 8 Cloudflare zones.
# Records are changed from aesthetic-computer.netlify.app / 75.2.60.5 → NEW_IP

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

if test (count $argv) -lt 1
    echo "Usage: fish scripts/dns-cutover.fish <NEW_IP>"
    exit 1
end

set NEW_IP $argv[1]

# Load Cloudflare credentials
set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../aesthetic-computer-vault"

if test -f "$VAULT_DIR/cloudflare.env"
    for line in (cat "$VAULT_DIR/cloudflare.env" | grep -v '^#' | grep -v '^$' | grep '=')
        set -l parts (string split '=' $line)
        if test (count $parts) -ge 2
            set -gx $parts[1] (string join '=' $parts[2..-1])
        end
    end
else
    # Fallback to feed.env
    set FEED_ENV "$SCRIPT_DIR/../../.devcontainer/envs/feed.env"
    if test -f $FEED_ENV
        set -gx CF_EMAIL (grep CLOUDFLARE_EMAIL $FEED_ENV | head -1 | cut -d= -f2 | tr -d '"')
        set -gx CF_KEY (grep CLOUDFLARE_API_KEY $FEED_ENV | head -1 | cut -d= -f2 | tr -d '"')
    end
end

if test -z "$CF_EMAIL" -o -z "$CF_KEY"
    echo -e "$RED x Cloudflare credentials not found$NC"
    exit 1
end

function cf_headers
    echo -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" -H "Content-Type: application/json"
end

# Helper: update or create an A record
function update_a_record -a zone_id name
    echo -e "$GREEN  -> $name → $NEW_IP$NC"

    # Find existing record
    set record_id (curl -s "https://api.cloudflare.com/client/v4/zones/$zone_id/dns_records?type=A&name=$name" \
        -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" \
        | python3 -c "import json,sys; r=json.load(sys.stdin)['result']; print(r[0]['id'] if r else '')")

    if test -n "$record_id"
        # Update existing A record
        curl -s -X PATCH "https://api.cloudflare.com/client/v4/zones/$zone_id/dns_records/$record_id" \
            -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" -H "Content-Type: application/json" \
            --data "{\"type\":\"A\",\"content\":\"$NEW_IP\",\"proxied\":true}" | python3 -c "import json,sys; print('    OK' if json.load(sys.stdin)['success'] else '    FAIL')"
    else
        # Check for CNAME and delete it first
        set cname_id (curl -s "https://api.cloudflare.com/client/v4/zones/$zone_id/dns_records?type=CNAME&name=$name" \
            -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" \
            | python3 -c "import json,sys; r=json.load(sys.stdin)['result']; print(r[0]['id'] if r else '')")

        if test -n "$cname_id"
            curl -s -X DELETE "https://api.cloudflare.com/client/v4/zones/$zone_id/dns_records/$cname_id" \
                -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" > /dev/null
        end

        # Create new A record
        curl -s -X POST "https://api.cloudflare.com/client/v4/zones/$zone_id/dns_records" \
            -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" -H "Content-Type: application/json" \
            --data "{\"type\":\"A\",\"name\":\"$name\",\"content\":\"$NEW_IP\",\"proxied\":true}" | python3 -c "import json,sys; print('    OK' if json.load(sys.stdin)['success'] else '    FAIL')"
    end
end

echo -e "$YELLOW=== DNS Cutover: 36 records → $NEW_IP ===$NC"
echo ""

# --- aesthetic.computer (da794a6ae8f17b80424907f81ed0db7c) ---
set Z da794a6ae8f17b80424907f81ed0db7c
echo -e "$YELLOW[aesthetic.computer]$NC"
update_a_record $Z "aesthetic.computer"
update_a_record $Z "api.aesthetic.computer"
update_a_record $Z "bills.aesthetic.computer"
update_a_record $Z "give.aesthetic.computer"
update_a_record $Z "keeps.aesthetic.computer"
update_a_record $Z "l5.aesthetic.computer"
update_a_record $Z "news.aesthetic.computer"
update_a_record $Z "p5.aesthetic.computer"
update_a_record $Z "pals.aesthetic.computer"
update_a_record $Z "papers.aesthetic.computer"
update_a_record $Z "processing.aesthetic.computer"
update_a_record $Z "sitemap.aesthetic.computer"
update_a_record $Z "www.aesthetic.computer"

# --- false.work (0fa28e0097b24e187f41fea0ec036c0d) ---
set Z 0fa28e0097b24e187f41fea0ec036c0d
echo -e "$YELLOW[false.work]$NC"
update_a_record $Z "builds.false.work"

# --- jas.life (79e214366285134e1fc7952db8aff75e) ---
set Z 79e214366285134e1fc7952db8aff75e
echo -e "$YELLOW[jas.life]$NC"
update_a_record $Z "jas.life"

# --- justanothersystem.org (a3366b124c7ca95fe902a54f868dcc51) ---
set Z a3366b124c7ca95fe902a54f868dcc51
echo -e "$YELLOW[justanothersystem.org]$NC"
update_a_record $Z "justanothersystem.org"
update_a_record $Z "www.justanothersystem.org"

# --- kidlisp.com (bac7b811ac7b4df664b696fafa9e6207) ---
set Z bac7b811ac7b4df664b696fafa9e6207
echo -e "$YELLOW[kidlisp.com]$NC"
update_a_record $Z "kidlisp.com"
update_a_record $Z "www.kidlisp.com"
update_a_record $Z "buy.kidlisp.com"
update_a_record $Z "calm.kidlisp.com"
update_a_record $Z "device.kidlisp.com"
update_a_record $Z "keep.kidlisp.com"
update_a_record $Z "keeps.kidlisp.com"
update_a_record $Z "learn.kidlisp.com"
update_a_record $Z "pj.kidlisp.com"
update_a_record $Z "top.kidlisp.com"

# --- notepat.com (8d289a1e56563dbcc9bc88747428c8ee) ---
set Z 8d289a1e56563dbcc9bc88747428c8ee
echo -e "$YELLOW[notepat.com]$NC"
update_a_record $Z "notepat.com"
update_a_record $Z "www.notepat.com"

# --- prompt.ac (1f93ca86e2d9de0def0acb0b8c4e722b) ---
set Z 1f93ca86e2d9de0def0acb0b8c4e722b
echo -e "$YELLOW[prompt.ac]$NC"
update_a_record $Z "prompt.ac"
update_a_record $Z "api.prompt.ac"
update_a_record $Z "l5.prompt.ac"
update_a_record $Z "p5.prompt.ac"
update_a_record $Z "papers.prompt.ac"
update_a_record $Z "processing.prompt.ac"
update_a_record $Z "sitemap.prompt.ac"

# --- sotce.net (1f56f8b5fd7b3db92d31bad0714a518f) ---
set Z 1f56f8b5fd7b3db92d31bad0714a518f
echo -e "$YELLOW[sotce.net]$NC"
update_a_record $Z "sotce.net"
update_a_record $Z "www.sotce.net"

echo ""
echo -e "$GREEN=== Done. 36 records updated. ===$NC"
echo -e "$YELLOW   Cloudflare propagation: ~30 seconds (proxied) or ~5 minutes (DNS-only)$NC"
