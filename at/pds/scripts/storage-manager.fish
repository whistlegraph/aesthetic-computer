#!/usr/bin/env fish
# Storage Management Helper for DigitalOcean Spaces
# Provides utilities for managing blob storage

set -e

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR (cd "$SCRIPT_DIR/../../../../aesthetic-computer-vault" && pwd)

# Load configuration
if test -f "$VAULT_DIR/at/deploy.env"
    source "$VAULT_DIR/at/deploy.env"
else
    echo "✗ Vault config not found"
    exit 1
end

# Configure s3cmd
function setup_s3cmd
    echo "Setting up s3cmd configuration..."
    
    printf "[default]
access_key = %s
secret_key = %s
host_base = %s
host_bucket = %%(bucket)s.%s
use_https = True
" "$SPACES_KEY" "$SPACES_SECRET" (echo $SPACES_ENDPOINT | sed 's|https://||') (echo $SPACES_ENDPOINT | sed 's|https://||') > ~/.s3cfg
    
    chmod 600 ~/.s3cfg
    echo "✓ s3cmd configured"
end

# Check storage usage
function check_usage
    echo "╔════════════════════════════════════════╗"
    echo "║   Storage Usage Report                 ║"
    echo "╚════════════════════════════════════════╝"
    echo ""
    
    # Check if bucket exists
    if not s3cmd ls | grep -q "$SPACES_BUCKET"
        echo "Bucket '$SPACES_BUCKET' not found"
        return 1
    end
    
    # Get size
    set SIZE (s3cmd du s3://$SPACES_BUCKET | awk '{print $1}')
    set SIZE_GB (math "$SIZE / 1024 / 1024 / 1024")
    
    # Get object count
    set COUNT (s3cmd ls -r s3://$SPACES_BUCKET | wc -l)
    
    # Calculate cost
    if test $SIZE_GB -gt 250
        set COST (math "($SIZE_GB - 250) * 0.02 + 5")
    else
        set COST 5
    end
    
    echo "Bucket: $SPACES_BUCKET"
    echo "Region: $SPACES_REGION"
    echo ""
    echo "Storage Used: "(printf "%.2f" $SIZE_GB)" GB"
    echo "Objects: $COUNT"
    echo "Estimated Cost: \$"(printf "%.2f" $COST)"/month"
    echo ""
    
    # Show breakdown
    echo "Top directories by size:"
    s3cmd du -r s3://$SPACES_BUCKET | sort -n -r | head -10
end

# List recent uploads
function list_recent
    set LIMIT 20
    if test -n "$argv[1]"
        set LIMIT $argv[1]
    end
    
    echo "Recent uploads (last $LIMIT):"
    echo ""
    
    s3cmd ls -r s3://$SPACES_BUCKET | tail -$LIMIT
end

# Test upload/download
function test_storage
    echo "Testing storage connectivity..."
    echo ""
    
    # Create test file
    echo "test data $(date)" > /tmp/pds-storage-test.txt
    
    # Upload
    echo -n "Upload test... "
    if s3cmd put /tmp/pds-storage-test.txt s3://$SPACES_BUCKET/test/ >/dev/null 2>&1
        echo "✓"
    else
        echo "✗ FAILED"
        return 1
    end
    
    # Download
    echo -n "Download test... "
    if s3cmd get s3://$SPACES_BUCKET/test/pds-storage-test.txt /tmp/pds-storage-test-dl.txt >/dev/null 2>&1
        echo "✓"
    else
        echo "✗ FAILED"
        return 1
    end
    
    # Verify
    echo -n "Verify test... "
    if diff /tmp/pds-storage-test.txt /tmp/pds-storage-test-dl.txt >/dev/null 2>&1
        echo "✓"
    else
        echo "✗ FAILED"
        return 1
    end
    
    # Cleanup
    s3cmd del s3://$SPACES_BUCKET/test/pds-storage-test.txt >/dev/null 2>&1
    rm /tmp/pds-storage-test*.txt
    
    echo ""
    echo "✓ All storage tests passed"
end

# Backup blobs
function backup_blobs
    set BACKUP_BUCKET $BACKUP_BUCKET
    set TIMESTAMP (date +%Y%m%d-%H%M%S)
    
    if test -z "$BACKUP_BUCKET"
        echo "✗ BACKUP_BUCKET not configured"
        return 1
    end
    
    echo "Backing up blobs..."
    echo "  Source: s3://$SPACES_BUCKET"
    echo "  Destination: s3://$BACKUP_BUCKET/backups/$TIMESTAMP/"
    echo ""
    
    s3cmd sync s3://$SPACES_BUCKET/ s3://$BACKUP_BUCKET/backups/$TIMESTAMP/ --progress
    
    echo ""
    echo "✓ Backup complete"
end

# Clean old test files
function clean_test_files
    echo "Cleaning test files..."
    
    s3cmd ls -r s3://$SPACES_BUCKET | grep -i test | awk '{print $4}' | while read file
        echo "Delete: $file"
        s3cmd del "$file"
    end
    
    echo "✓ Cleanup complete"
end

# Show CDN info
function show_cdn_info
    echo "╔════════════════════════════════════════╗"
    echo "║   CDN Information                      ║"
    echo "╚════════════════════════════════════════╝"
    echo ""
    
    set CDN_ENDPOINT (echo $SPACES_ENDPOINT | sed 's/digitaloceanspaces/cdn.digitaloceanspaces/')
    
    echo "Origin: $SPACES_ENDPOINT"
    echo "CDN: $CDN_ENDPOINT"
    echo ""
    echo "CDN URL format:"
    echo "  $CDN_ENDPOINT/$SPACES_BUCKET/path/to/file"
    echo ""
    echo "✓ CDN is included with Spaces at no extra cost"
end

# Main command dispatcher
function main
    if test (count $argv) -eq 0
        echo "Usage: fish storage-manager.fish <command>"
        echo ""
        echo "Commands:"
        echo "  setup        - Configure s3cmd"
        echo "  usage        - Show storage usage and cost"
        echo "  recent [N]   - List N recent uploads (default: 20)"
        echo "  test         - Test storage connectivity"
        echo "  backup       - Backup blobs to backup bucket"
        echo "  clean        - Remove test files"
        echo "  cdn          - Show CDN information"
        echo ""
        return 0
    end
    
    switch $argv[1]
        case setup
            setup_s3cmd
        case usage
            check_usage
        case recent
            list_recent $argv[2..-1]
        case test
            test_storage
        case backup
            backup_blobs
        case clean
            clean_test_files
        case cdn
            show_cdn_info
        case '*'
            echo "Unknown command: $argv[1]"
            echo "Run without arguments to see usage"
            return 1
    end
end

# Run main
main $argv
