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
