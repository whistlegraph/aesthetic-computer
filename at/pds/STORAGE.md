# Blob Storage Configuration Guide

This guide covers configuring blob storage for your PDS to store images, videos, and other media files.

## Overview

PDS stores two types of data:

1. **Structured Data** (SQLite): Posts, profiles, relationships
2. **Blobs** (Object Storage): Images, videos, avatars, banners

This document focuses on blob storage configuration.

## Storage Options Comparison

### DigitalOcean Spaces ✅ Recommended

**Pros:**
- S3-compatible API (easy integration)
- Simple pricing: $5/month for 250GB storage + bandwidth
- Includes CDN at no extra cost
- Easy to set up from DO dashboard
- Good performance
- Scales to petabytes

**Cons:**
- Limited to DO regions
- Less features than AWS S3

**Best For:**
- Small to medium deployments (1-100 users)
- Cost-conscious hosting
- Simple setup requirements

**Pricing:**
```
Base: $5/month (250GB storage + 1TB bandwidth)
Additional storage: $0.02/GB/month
Additional bandwidth: $0.01/GB (after 1TB)
```

### Google Cloud Storage

**Pros:**
- Highly scalable
- Advanced features (lifecycle, versioning, analytics)
- Global network
- Integrated with GCP services
- Better for large scale

**Cons:**
- More complex pricing
- Requires GCP account setup
- More configuration needed
- Higher cost at small scale

**Best For:**
- Large deployments (100+ users)
- Need advanced features
- Already using GCP

**Pricing:**
```
Standard storage: $0.020/GB/month
Network egress: $0.12/GB (NA/EU)
Operations: $0.05 per 10,000 writes
```

### Local Disk (Development Only)

**Pros:**
- Simple setup
- No external dependencies
- No extra cost

**Cons:**
- No redundancy
- No CDN
- Limited by disk size
- Not suitable for production
- Difficult to scale

**Best For:**
- Development and testing only
- Never for production

## DigitalOcean Spaces Setup

### 1. Create a Space

**Via DigitalOcean Dashboard:**

1. Go to Spaces: https://cloud.digitalocean.com/spaces
2. Click "Create a Space"
3. Choose region (closest to users):
   - `nyc3` - New York (US East)
   - `sfo3` - San Francisco (US West)
   - `fra1` - Frankfurt (Europe)
   - `sgp1` - Singapore (Asia)
4. Name: `aesthetic-pds-blobs`
5. Enable CDN: ✅ Yes (included in price)
6. File Listing: 🚫 Restricted (important for privacy)
7. Click "Create Space"

**Via CLI (doctl):**

```bash
# Install doctl first
brew install doctl  # or appropriate package manager

# Authenticate
doctl auth init

# Create space (CDN enabled by default)
doctl spaces create aesthetic-pds-blobs --region nyc3
```

### 2. Generate Access Keys

**Via Dashboard:**

1. Go to API → Spaces Keys
2. Click "Generate New Key"
3. Name: `aesthetic-pds-production`
4. Save the key and secret (shown only once!)

**Store in vault:**

```bash
# Add to aesthetic-computer-vault/at/.env
echo "SPACES_KEY=DO00XXXXXXXXXX" >> aesthetic-computer-vault/at/.env
echo "SPACES_SECRET=xxxxxxxxxxxxxxxxxxxx" >> aesthetic-computer-vault/at/.env
```

### 3. Configure PDS

Add to `/pds/pds.env` on your server:

```env
# Blob Storage - DigitalOcean Spaces
PDS_BLOBSTORE_DISK_LOCATION=s3

# S3-compatible endpoint
PDS_BLOBSTORE_S3_ENDPOINT=https://nyc3.digitaloceanspaces.com

# Space name (bucket)
PDS_BLOBSTORE_S3_BUCKET=aesthetic-pds-blobs

# Access credentials
PDS_BLOBSTORE_S3_ACCESS_KEY_ID=DO00XXXXXXXXXX
PDS_BLOBSTORE_S3_SECRET_ACCESS_KEY=xxxxxxxxxxxxxxxxxxxx

# Region
PDS_BLOBSTORE_S3_REGION=nyc3

# Force path style (required for Spaces)
PDS_BLOBSTORE_S3_FORCE_PATH_STYLE=true
```

### 4. Set CORS Policy (If Needed)

For direct browser uploads:

```json
[
  {
    "AllowedOrigins": ["https://pds.aesthetic.computer"],
    "AllowedMethods": ["PUT", "POST", "GET", "HEAD"],
    "AllowedHeaders": ["*"],
    "MaxAgeSeconds": 3600
  }
]
```

Apply via doctl:

```bash
# Save CORS policy to file
cat > cors.json << 'EOF'
[
  {
    "AllowedOrigins": ["https://pds.aesthetic.computer"],
    "AllowedMethods": ["PUT", "POST", "GET", "HEAD"],
    "AllowedHeaders": ["*"],
    "MaxAgeSeconds": 3600
  }
]
EOF

# Apply to space
doctl spaces cors set aesthetic-pds-blobs --config cors.json
```

### 5. Enable CDN (Included)

Spaces CDN is automatically enabled. Verify:

```bash
# Get Space info
doctl spaces list

# CDN endpoint will be shown
# Format: aesthetic-pds-blobs.nyc3.cdn.digitaloceanspaces.com
```

### 6. Test Upload

```bash
# Install s3cmd
apt install s3cmd

# Configure
cat > ~/.s3cfg << EOF
[default]
access_key = DO00XXXXXXXXXX
secret_key = xxxxxxxxxxxxxxxxxxxx
host_base = nyc3.digitaloceanspaces.com
host_bucket = %(bucket)s.nyc3.digitaloceanspaces.com
use_https = True
EOF

# Test upload
echo "test" > test.txt
s3cmd put test.txt s3://aesthetic-pds-blobs/test.txt

# Test download
s3cmd get s3://aesthetic-pds-blobs/test.txt

# Delete test file
s3cmd del s3://aesthetic-pds-blobs/test.txt
```

## Google Cloud Storage Setup

### 1. Create GCS Bucket

```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash

# Initialize and authenticate
gcloud init

# Create bucket
gcloud storage buckets create gs://aesthetic-pds-blobs \
  --location=us-east1 \
  --uniform-bucket-level-access \
  --public-access-prevention

# Enable versioning (optional)
gcloud storage buckets update gs://aesthetic-pds-blobs \
  --versioning
```

### 2. Create Service Account

```bash
# Create service account
gcloud iam service-accounts create pds-blob-storage \
  --display-name="PDS Blob Storage"

# Grant permissions
gcloud storage buckets add-iam-policy-binding gs://aesthetic-pds-blobs \
  --member="serviceAccount:pds-blob-storage@aesthetic-computer.iam.gserviceaccount.com" \
  --role="roles/storage.objectAdmin"

# Create key
gcloud iam service-accounts keys create pds-gcs-key.json \
  --iam-account=pds-blob-storage@aesthetic-computer.iam.gserviceaccount.com
```

### 3. Configure PDS

```env
# In /pds/pds.env
PDS_BLOBSTORE_DISK_LOCATION=gcs
PDS_BLOBSTORE_GCS_BUCKET=aesthetic-pds-blobs
PDS_BLOBSTORE_GCS_PROJECT_ID=aesthetic-computer
GOOGLE_APPLICATION_CREDENTIALS=/pds/gcs-service-account.json
```

Copy the service account key to server:

```bash
scp pds-gcs-key.json root@pds.aesthetic.computer:/pds/gcs-service-account.json
```

### 4. Set CORS Policy

```bash
# Create CORS config
cat > cors.json << 'EOF'
[
  {
    "origin": ["https://pds.aesthetic.computer"],
    "method": ["GET", "HEAD", "PUT", "POST"],
    "responseHeader": ["Content-Type"],
    "maxAgeSeconds": 3600
  }
]
EOF

# Apply CORS
gcloud storage buckets update gs://aesthetic-pds-blobs \
  --cors-file=cors.json
```

### 5. Enable CDN (Optional)

```bash
# Create backend bucket
gcloud compute backend-buckets create aesthetic-pds-blobs-backend \
  --gcs-bucket-name=aesthetic-pds-blobs \
  --enable-cdn

# Set cache TTL
gcloud compute backend-buckets update aesthetic-pds-blobs-backend \
  --cache-mode=CACHE_ALL_STATIC \
  --default-ttl=3600
```

## Storage Management Scripts

### Monitor Storage Usage

**For DigitalOcean Spaces:**

```bash
#!/bin/bash
# scripts/check-storage-usage.sh

SPACE_NAME="aesthetic-pds-blobs"

# Get total size
SIZE=$(s3cmd du s3://$SPACE_NAME | awk '{print $1}')
SIZE_GB=$(echo "scale=2; $SIZE/1024/1024/1024" | bc)

echo "Space: $SPACE_NAME"
echo "Size: ${SIZE_GB}GB"
echo "Cost: \$$(echo "scale=2; if($SIZE_GB > 250) ($SIZE_GB - 250) * 0.02 + 5 else 5" | bc)/month"

# Get object count
COUNT=$(s3cmd ls -r s3://$SPACE_NAME | wc -l)
echo "Objects: $COUNT"
```

### Backup Blobs

```bash
#!/bin/bash
# scripts/backup-blobs.sh

SOURCE="s3://aesthetic-pds-blobs"
DEST="s3://aesthetic-pds-backups/blobs-$(date +%Y%m%d)"

# Sync to backup bucket
s3cmd sync $SOURCE $DEST

echo "Backed up blobs to $DEST"
```

### Clean Old Test Files

```bash
#!/bin/bash
# scripts/clean-test-blobs.sh

# Delete files older than 30 days with "test" in name
s3cmd ls -r s3://aesthetic-pds-blobs | \
  grep "test" | \
  awk '{if (NF==4) print $4}' | \
  while read file; do
    s3cmd del "$file"
  done
```

## Lifecycle Policies

### DigitalOcean Spaces

Spaces supports lifecycle rules for automatic cleanup:

```xml
<!-- lifecycle.xml -->
<LifecycleConfiguration>
  <Rule>
    <ID>delete-old-drafts</ID>
    <Status>Enabled</Status>
    <Prefix>drafts/</Prefix>
    <Expiration>
      <Days>30</Days>
    </Expiration>
  </Rule>
  <Rule>
    <ID>transition-old-videos</ID>
    <Status>Enabled</Status>
    <Prefix>videos/</Prefix>
    <Expiration>
      <Days>365</Days>
    </Expiration>
  </Rule>
</LifecycleConfiguration>
```

Apply:
```bash
s3cmd setlifecycle lifecycle.xml s3://aesthetic-pds-blobs
```

### Google Cloud Storage

```bash
# Create lifecycle config
cat > lifecycle.json << 'EOF'
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "Delete"},
        "condition": {
          "age": 365,
          "matchesPrefix": ["videos/"]
        }
      }
    ]
  }
}
EOF

# Apply lifecycle
gcloud storage buckets update gs://aesthetic-pds-blobs \
  --lifecycle-file=lifecycle.json
```

## Security Best Practices

### 1. Restrict Access

**DO Spaces:**
- Set "File Listing" to "Restricted"
- Use signed URLs for private content
- Rotate access keys annually

**GCS:**
- Use uniform bucket-level access
- Enable public access prevention
- Use service accounts, not user accounts

### 2. Encrypt at Rest

Both DO Spaces and GCS encrypt at rest by default:
- ✅ DO Spaces: AES-256 encryption (automatic)
- ✅ GCS: AES-256 encryption (automatic)

For additional security:
```env
# Client-side encryption
PDS_BLOBSTORE_ENCRYPTION_KEY=<32-byte-key>
```

### 3. Access Logging

**Enable access logs:**

```bash
# DO Spaces
s3cmd logging s3://aesthetic-pds-blobs s3://aesthetic-pds-logs

# GCS
gcloud storage buckets update gs://aesthetic-pds-blobs \
  --log-bucket=aesthetic-pds-logs \
  --log-object-prefix=access-logs/
```

### 4. Versioning

Protect against accidental deletion:

```bash
# DO Spaces
s3cmd setversioning enable s3://aesthetic-pds-blobs

# GCS
gcloud storage buckets update gs://aesthetic-pds-blobs \
  --versioning
```

## Cost Optimization

### 1. Compress Images

```env
# In PDS config
PDS_BLOBSTORE_COMPRESSION_ENABLED=true
PDS_BLOBSTORE_MAX_SIZE_BYTES=5242880  # 5MB
```

### 2. Use CDN

- Reduces origin requests
- Lowers bandwidth costs
- Improves performance

### 3. Set Cache Headers

```env
# Cache static assets
PDS_BLOBSTORE_CACHE_CONTROL="public, max-age=31536000"
```

### 4. Monitor Usage

Set up alerts:

```bash
# Alert if storage > 200GB
if [ $SIZE_GB -gt 200 ]; then
  echo "Storage exceeds 200GB: ${SIZE_GB}GB" | mail -s "PDS Storage Alert" admin@aesthetic.computer
fi
```

## Troubleshooting

### Connection Errors

```bash
# Test connectivity
curl -I https://nyc3.digitaloceanspaces.com

# Test authentication
s3cmd ls s3://aesthetic-pds-blobs
```

### Upload Failures

Check PDS logs:
```bash
docker logs pds | grep -i blob
```

Common issues:
- Wrong credentials
- Bucket doesn't exist
- CORS misconfigured
- Network blocked

### Slow Performance

- Check CDN is enabled
- Verify region is close to users
- Monitor bandwidth usage
- Check server CPU/memory

## Migration Between Providers

### Spaces to GCS

```bash
# Install rclone
curl https://rclone.org/install.sh | bash

# Configure both remotes
rclone config

# Sync data
rclone sync spaces:aesthetic-pds-blobs gcs:aesthetic-pds-blobs --progress
```

### Verify Migration

```bash
# Check file counts
rclone size spaces:aesthetic-pds-blobs
rclone size gcs:aesthetic-pds-blobs

# Compare checksums
rclone check spaces:aesthetic-pds-blobs gcs:aesthetic-pds-blobs
```

## Next Steps

- Return to [README.md](./README.md) for overview
- Review [DEPLOYMENT.md](./DEPLOYMENT.md) for full deployment
- Review [MAINTENANCE.md](./MAINTENANCE.md) for ongoing management

---

**Last Updated**: October 9, 2025
**Recommended**: DigitalOcean Spaces for initial deployment
