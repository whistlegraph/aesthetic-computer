/**
 * Cloudflare DNS Management Module
 * 
 * Provides utilities for managing DNS records on Cloudflare.
 * Reads credentials from vault environment files.
 */

import { readFileSync } from 'fs';
import { resolve } from 'path';
import { homedir } from 'os';

const CLOUDFLARE_BASE_URL = 'https://api.cloudflare.com/client/v4';

/**
 * Load Cloudflare credentials from vault
 */
function loadCloudflareCredentials() {
  const vaultPaths = [
    resolve(homedir(), 'aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env'),
    resolve(homedir(), 'aesthetic-computer/aesthetic-computer-vault/grab/.env'),
  ];

  for (const vaultPath of vaultPaths) {
    try {
      const envContent = readFileSync(vaultPath, 'utf-8');
      const env = {};
      
      envContent.split('\n').forEach((line) => {
        const match = line.match(/^([^#=]+)=(.*)$/);
        if (match) {
          const key = match[1].trim();
          let value = match[2].trim();
          // Remove quotes if present
          if ((value.startsWith('"') && value.endsWith('"')) || 
              (value.startsWith("'") && value.endsWith("'"))) {
            value = value.slice(1, -1);
          }
          env[key] = value;
        }
      });

      if (env.CLOUDFLARE_EMAIL && env.CLOUDFLARE_API_KEY) {
        return {
          email: env.CLOUDFLARE_EMAIL,
          apiKey: env.CLOUDFLARE_API_KEY,
          accountId: env.CLOUDFLARE_ACCOUNT_ID,
        };
      }
    } catch (error) {
      // Try next path
      continue;
    }
  }

  throw new Error('Could not load Cloudflare credentials from vault');
}

/**
 * Create authenticated headers for Cloudflare API
 */
function createHeaders(credentials) {
  return {
    'X-Auth-Email': credentials.email,
    'X-Auth-Key': credentials.apiKey,
    'Content-Type': 'application/json',
  };
}

/**
 * Fetch zone ID for a domain
 */
async function getZoneId(domain, credentials) {
  const headers = createHeaders(credentials);
  const response = await fetch(`${CLOUDFLARE_BASE_URL}/zones?name=${domain}`, { headers });
  const data = await response.json();
  
  if (!data.success || !data.result || data.result.length === 0) {
    throw new Error(`Zone not found for domain: ${domain}`);
  }
  
  return data.result[0].id;
}

/**
 * Fetch existing DNS record
 */
async function getDNSRecord(zoneId, recordType, recordName, credentials) {
  const headers = createHeaders(credentials);
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records?type=${recordType}&name=${recordName}`,
    { headers }
  );
  const data = await response.json();
  
  return data.result?.[0];
}

/**
 * Create a new DNS record
 */
async function createDNSRecord(zoneId, recordData, credentials) {
  const headers = createHeaders(credentials);
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records`,
    { 
      method: 'POST', 
      headers, 
      body: JSON.stringify(recordData) 
    }
  );
  const data = await response.json();
  
  if (!data.success) {
    throw new Error(`Failed to create DNS record: ${JSON.stringify(data.errors)}`);
  }
  
  return data.result;
}

/**
 * Update an existing DNS record
 */
async function updateDNSRecord(zoneId, recordId, recordData, credentials) {
  const headers = createHeaders(credentials);
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records/${recordId}`,
    { 
      method: 'PUT', 
      headers, 
      body: JSON.stringify(recordData) 
    }
  );
  const data = await response.json();
  
  if (!data.success) {
    throw new Error(`Failed to update DNS record: ${JSON.stringify(data.errors)}`);
  }
  
  return data.result;
}

/**
 * Create or update a TXT record
 * 
 * @param {string} recordName - Full record name (e.g., "_lexicon.aesthetic.computer")
 * @param {string} content - TXT record content
 * @param {string} rootDomain - Root domain (e.g., "aesthetic.computer")
 * @param {boolean} dryRun - If true, only check what would be done
 * @returns {Promise<Object>} Result object with status and details
 */
export async function createOrUpdateTXTRecord(recordName, content, rootDomain, dryRun = false) {
  try {
    const credentials = loadCloudflareCredentials();
    const zoneId = await getZoneId(rootDomain, credentials);
    
    // Check for existing record
    const existingRecord = await getDNSRecord(zoneId, 'TXT', recordName, credentials);
    
    const recordData = {
      type: 'TXT',
      name: recordName,
      content: content,
      ttl: 3600, // 1 hour
    };
    
    if (existingRecord) {
      if (existingRecord.content === content) {
        return {
          action: 'none',
          recordName,
          content,
          message: `TXT record already exists with correct value`
        };
      }
      
      if (dryRun) {
        return {
          action: 'update',
          recordName,
          oldContent: existingRecord.content,
          newContent: content,
          message: `Would update TXT record: ${existingRecord.content} → ${content}`
        };
      }
      
      await updateDNSRecord(zoneId, existingRecord.id, recordData, credentials);
      return {
        action: 'updated',
        recordName,
        content,
        message: `Updated TXT record: ${recordName} → ${content}`
      };
    } else {
      if (dryRun) {
        return {
          action: 'create',
          recordName,
          content,
          message: `Would create TXT record: ${recordName} → ${content}`
        };
      }
      
      await createDNSRecord(zoneId, recordData, credentials);
      return {
        action: 'created',
        recordName,
        content,
        message: `Created TXT record: ${recordName} → ${content}`
      };
    }
  } catch (error) {
    return {
      action: 'error',
      recordName,
      error: error.message,
      message: `Error managing DNS record: ${error.message}`
    };
  }
}

/**
 * Verify a TXT record exists and has the correct value
 */
export async function verifyTXTRecord(recordName, expectedContent, rootDomain) {
  try {
    const credentials = loadCloudflareCredentials();
    const zoneId = await getZoneId(rootDomain, credentials);
    const record = await getDNSRecord(zoneId, 'TXT', recordName, credentials);
    
    if (!record) {
      return {
        exists: false,
        message: `TXT record not found: ${recordName}`
      };
    }
    
    const matches = record.content === expectedContent;
    return {
      exists: true,
      matches,
      actualContent: record.content,
      expectedContent,
      message: matches 
        ? `TXT record verified: ${recordName}`
        : `TXT record exists but content doesn't match`
    };
  } catch (error) {
    return {
      exists: false,
      error: error.message,
      message: `Error verifying DNS record: ${error.message}`
    };
  }
}

/**
 * Create or update a CNAME record (for Netlify subdomains)
 * 
 * @param {string} subdomain - Subdomain name (e.g., "bills" for bills.aesthetic.computer)
 * @param {string} target - CNAME target (default: Netlify's load balancer)
 * @param {string} rootDomain - Root domain (default: "aesthetic.computer")
 * @param {boolean} proxied - Whether to proxy through Cloudflare (default: true)
 * @param {boolean} dryRun - If true, only check what would be done
 * @returns {Promise<Object>} Result object with status and details
 */
export async function createOrUpdateCNAME(subdomain, target = 'aesthetic-computer.netlify.app', rootDomain = 'aesthetic.computer', proxied = true, dryRun = false) {
  const recordName = `${subdomain}.${rootDomain}`;
  
  try {
    const credentials = loadCloudflareCredentials();
    const zoneId = await getZoneId(rootDomain, credentials);
    
    // Check for existing record
    const existingRecord = await getDNSRecord(zoneId, 'CNAME', recordName, credentials);
    
    const recordData = {
      type: 'CNAME',
      name: recordName,
      content: target,
      ttl: 1, // Auto TTL when proxied
      proxied: proxied,
    };
    
    if (existingRecord) {
      if (existingRecord.content === target && existingRecord.proxied === proxied) {
        return {
          action: 'none',
          recordName,
          target,
          proxied,
          message: `CNAME record already exists with correct value`
        };
      }
      
      if (dryRun) {
        return {
          action: 'update',
          recordName,
          oldTarget: existingRecord.content,
          newTarget: target,
          proxied,
          message: `Would update CNAME: ${existingRecord.content} → ${target}`
        };
      }
      
      await updateDNSRecord(zoneId, existingRecord.id, recordData, credentials);
      return {
        action: 'updated',
        recordName,
        target,
        proxied,
        message: `Updated CNAME: ${recordName} → ${target}`
      };
    } else {
      if (dryRun) {
        return {
          action: 'create',
          recordName,
          target,
          proxied,
          message: `Would create CNAME: ${recordName} → ${target}`
        };
      }
      
      await createDNSRecord(zoneId, recordData, credentials);
      return {
        action: 'created',
        recordName,
        target,
        proxied,
        message: `Created CNAME: ${recordName} → ${target}`
      };
    }
  } catch (error) {
    return {
      action: 'error',
      recordName,
      error: error.message,
      message: `Error managing CNAME record: ${error.message}`
    };
  }
}

/**
 * List all DNS records for a domain
 */
export async function listDNSRecords(rootDomain = 'aesthetic.computer', type = null) {
  try {
    const credentials = loadCloudflareCredentials();
    const zoneId = await getZoneId(rootDomain, credentials);
    const headers = createHeaders(credentials);
    
    let url = `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records`;
    if (type) url += `?type=${type}`;
    
    const response = await fetch(url, { headers });
    const data = await response.json();
    
    if (!data.success) {
      throw new Error(`Failed to list DNS records: ${JSON.stringify(data.errors)}`);
    }
    
    return data.result;
  } catch (error) {
    throw new Error(`Error listing DNS records: ${error.message}`);
  }
}

// CLI support
if (import.meta.url === `file://${process.argv[1]}`) {
  const [,, command, ...args] = process.argv;
  
  async function main() {
    switch (command) {
      case 'add-subdomain':
      case 'add-cname': {
        const subdomain = args[0];
        const target = args[1] || 'aesthetic-computer.netlify.app';
        if (!subdomain) {
          console.error('Usage: node cloudflare-dns.mjs add-subdomain <subdomain> [target]');
          console.error('Example: node cloudflare-dns.mjs add-subdomain bills');
          process.exit(1);
        }
        console.log(`Adding CNAME: ${subdomain}.aesthetic.computer → ${target}`);
        const result = await createOrUpdateCNAME(subdomain, target);
        console.log(result.message);
        break;
      }
      
      case 'list': {
        const type = args[0]; // optional: CNAME, A, TXT, etc.
        console.log(`Listing DNS records${type ? ` (type: ${type})` : ''}...`);
        const records = await listDNSRecords('aesthetic.computer', type);
        records.forEach(r => {
          console.log(`  ${r.type.padEnd(6)} ${r.name.padEnd(40)} → ${r.content} ${r.proxied ? '(proxied)' : ''}`);
        });
        break;
      }
      
      default:
        console.log('Cloudflare DNS Management');
        console.log('');
        console.log('Commands:');
        console.log('  add-subdomain <name> [target]  Add a CNAME for a Netlify subdomain');
        console.log('  list [type]                    List all DNS records (optionally filter by type)');
        console.log('');
        console.log('Examples:');
        console.log('  node cloudflare-dns.mjs add-subdomain bills');
        console.log('  node cloudflare-dns.mjs list CNAME');
    }
  }
  
  main().catch(console.error);
}
