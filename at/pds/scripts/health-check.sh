#!/usr/bin/env bash
# PDS Health Check Script
# Monitors the health and status of your PDS instance

set -euo pipefail

# Configuration
PDS_URL="${PDS_URL:-https://pds.aesthetic.computer}"
ALERT_EMAIL="${ALERT_EMAIL:-me@jas.life}"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo "╔════════════════════════════════════════╗"
echo "║   PDS Health Check                     ║"
echo "╚════════════════════════════════════════╝"
echo ""

# Check HTTP health endpoint
check_http_health() {
    echo -n "HTTP Health Check... "
    
    RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" "$PDS_URL/xrpc/_health" || echo "000")
    
    if [ "$RESPONSE" = "200" ]; then
        echo -e "${GREEN}✓ OK${NC}"
        
        # Get version
        VERSION=$(curl -s "$PDS_URL/xrpc/_health" | jq -r '.version // "unknown"')
        echo "  Version: $VERSION"
        return 0
    else
        echo -e "${RED}✗ FAILED${NC} (HTTP $RESPONSE)"
        return 1
    fi
}

# Check WebSocket endpoint
check_websocket() {
    echo -n "WebSocket Check... "
    
    if command -v wsdump &> /dev/null; then
        timeout 5 wsdump "wss://${PDS_URL#https://}/xrpc/com.atproto.sync.subscribeRepos?cursor=0" &> /dev/null && {
            echo -e "${GREEN}✓ OK${NC}"
            return 0
        } || {
            echo -e "${RED}✗ FAILED${NC}"
            return 1
        }
    else
        echo -e "${YELLOW}⊘ SKIPPED${NC} (wsdump not installed)"
        return 0
    fi
}

# Check SSL certificate
check_ssl() {
    echo -n "SSL Certificate... "
    
    HOSTNAME="${PDS_URL#https://}"
    EXPIRY=$(echo | openssl s_client -servername "$HOSTNAME" -connect "$HOSTNAME:443" 2>/dev/null | \
             openssl x509 -noout -enddate 2>/dev/null | cut -d= -f2)
    
    if [ -n "$EXPIRY" ]; then
        EXPIRY_EPOCH=$(date -d "$EXPIRY" +%s 2>/dev/null || date -j -f "%b %d %H:%M:%S %Y %Z" "$EXPIRY" +%s)
        NOW_EPOCH=$(date +%s)
        DAYS_LEFT=$(( ($EXPIRY_EPOCH - $NOW_EPOCH) / 86400 ))
        
        if [ $DAYS_LEFT -lt 7 ]; then
            echo -e "${RED}✗ EXPIRING SOON${NC} ($DAYS_LEFT days left)"
            return 1
        elif [ $DAYS_LEFT -lt 30 ]; then
            echo -e "${YELLOW}⚠ WARNING${NC} ($DAYS_LEFT days left)"
            return 0
        else
            echo -e "${GREEN}✓ OK${NC} ($DAYS_LEFT days left)"
            return 0
        fi
    else
        echo -e "${RED}✗ FAILED${NC} (cannot read certificate)"
        return 1
    fi
}

# Check DNS resolution
check_dns() {
    echo -n "DNS Resolution... "
    
    HOSTNAME="${PDS_URL#https://}"
    
    if host "$HOSTNAME" &> /dev/null; then
        IP=$(host "$HOSTNAME" | grep "has address" | head -1 | awk '{print $NF}')
        echo -e "${GREEN}✓ OK${NC} ($IP)"
        
        # Check wildcard
        echo -n "DNS Wildcard... "
        if host "test123.$HOSTNAME" &> /dev/null; then
            WILDCARD_IP=$(host "test123.$HOSTNAME" | grep "has address" | head -1 | awk '{print $NF}')
            if [ "$IP" = "$WILDCARD_IP" ]; then
                echo -e "${GREEN}✓ OK${NC}"
            else
                echo -e "${YELLOW}⚠ MISMATCH${NC} (wildcard: $WILDCARD_IP)"
            fi
        else
            echo -e "${RED}✗ FAILED${NC}"
            return 1
        fi
        
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        return 1
    fi
}

# Check response time
check_response_time() {
    echo -n "Response Time... "
    
    START=$(date +%s%N)
    curl -s -o /dev/null "$PDS_URL/xrpc/_health" || true
    END=$(date +%s%N)
    
    ELAPSED=$(( ($END - $START) / 1000000 ))
    
    if [ $ELAPSED -lt 200 ]; then
        echo -e "${GREEN}✓ EXCELLENT${NC} (${ELAPSED}ms)"
    elif [ $ELAPSED -lt 500 ]; then
        echo -e "${GREEN}✓ GOOD${NC} (${ELAPSED}ms)"
    elif [ $ELAPSED -lt 1000 ]; then
        echo -e "${YELLOW}⚠ SLOW${NC} (${ELAPSED}ms)"
    else
        echo -e "${RED}✗ VERY SLOW${NC} (${ELAPSED}ms)"
    fi
}

# Main checks
FAILED=0

check_http_health || FAILED=$((FAILED + 1))
check_websocket || FAILED=$((FAILED + 1))
check_ssl || FAILED=$((FAILED + 1))
check_dns || FAILED=$((FAILED + 1))
check_response_time

echo ""
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All checks passed!${NC}"
    exit 0
else
    echo -e "${RED}$FAILED check(s) failed${NC}"
    
    # Send alert if email configured
    if [ -n "$ALERT_EMAIL" ]; then
        echo "PDS health check failed at $PDS_URL" | mail -s "PDS Health Alert" "$ALERT_EMAIL" 2>/dev/null || true
    fi
    
    exit 1
fi
