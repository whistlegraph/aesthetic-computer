#!/usr/bin/env node

/**
 * Short Domain Availability Checker for QR Code Optimization
 * 
 * Goal: Find the shortest possible domain to minimize QR code size.
 * 
 * QR Code Version vs Character Capacity (Alphanumeric, Error Level L):
 * - Version 1 (21×21): 25 chars
 * - Version 2 (25×25): 47 chars  <- Current with prompt.ac
 * - Version 3 (29×29): 77 chars
 * 
 * URL format: https://DOMAIN/CODE
 * - "https://" = 8 chars
 * - "/" = 1 char  
 * - CODE = variable (let's assume 8-12 chars for typical codes)
 * 
 * To fit in Version 1 (21×21), we need: domain + 8 + 1 + code ≤ 25
 * With 10-char code: domain ≤ 6 chars (e.g., "q.ac" = 4 chars)
 */

// List of short, potentially cheap TLDs that work well for short domains
const SHORT_TLDS = [
  // 2-letter ccTLDs (often cheap)
  'ac', // Ascension Island - ~$26/yr
  'ai', // Anguilla - expensive ($70+)
  'cc', // Cocos Islands - ~$12/yr
  'co', // Colombia - ~$30/yr
  'cx', // Christmas Island - ~$25/yr
  'gg', // Guernsey - ~$70/yr
  'gs', // South Georgia - ~$40/yr
  'im', // Isle of Man - ~$20/yr
  'io', // British Indian Ocean - ~$40/yr (popular for tech)
  'is', // Iceland - ~$60/yr
  'la', // Laos - ~$40/yr
  'li', // Liechtenstein - ~$30/yr
  'ly', // Libya - ~$100/yr
  'me', // Montenegro - ~$15/yr
  'mn', // Mongolia - ~$50/yr
  'mu', // Mauritius - ~$90/yr
  'ms', // Montserrat - ~$35/yr
  'nu', // Niue - ~$25/yr
  'sh', // Saint Helena - ~$60/yr
  'so', // Somalia - ~$80/yr
  'tc', // Turks and Caicos - ~$100/yr
  'tl', // Timor-Leste - ~$100/yr
  'to', // Tonga - ~$70/yr
  'tv', // Tuvalu - ~$35/yr
  'vc', // St Vincent - ~$40/yr
  'vg', // British Virgin Islands - ~$40/yr
  'ws', // Samoa - ~$30/yr
  
  // New gTLDs (often cheap promos)
  'lol', // ~$30/yr
  'wtf', // ~$30/yr
  'xyz', // ~$1/yr first year!
  'fun', // ~$20/yr
  'one', // ~$10/yr
  'app', // ~$15/yr (requires HTTPS)
  'dev', // ~$15/yr (requires HTTPS)
  'page', // ~$10/yr
  'link', // ~$12/yr
  'click', // ~$10/yr
  'site', // ~$3/yr
  'online', // ~$5/yr
  'run', // ~$20/yr
  'zip', // ~$15/yr (but confusing with file extension)
];

// Single letter domains to check (most are taken but worth checking)
const SINGLE_LETTERS = 'abcdefghijklmnopqrstuvwxyz0123456789'.split('');

// Two letter combinations to check
const TWO_LETTER_PREFIXES = ['qr', 'ac', 'go', 'do', 'my', 'io', 'hi', 'ok', 'up', 'it'];

async function checkDomainWhois(domain) {
  // Using DNS lookup as a quick availability proxy
  // Note: This doesn't guarantee availability, just checks if it resolves
  try {
    const { promises: dns } = await import('dns');
    await dns.resolve(domain);
    return { domain, status: 'taken', note: 'Resolves to IP' };
  } catch (err) {
    if (err.code === 'ENOTFOUND' || err.code === 'ENODATA') {
      return { domain, status: 'possibly-available', note: 'No DNS records' };
    }
    return { domain, status: 'unknown', note: err.code };
  }
}

async function checkDomainRDAP(domain) {
  // Try RDAP lookup for more accurate availability check
  const tld = domain.split('.').pop();
  const rdapServers = {
    'ac': 'https://rdap.nic.ac/domain/',
    'io': 'https://rdap.nic.io/domain/',
    'co': 'https://rdap.nic.co/domain/',
    'me': 'https://rdap.nic.me/domain/',
    'xyz': 'https://rdap.centralnic.com/xyz/domain/',
    // Add more as needed
  };
  
  const rdapUrl = rdapServers[tld];
  if (!rdapUrl) return null;
  
  try {
    const response = await fetch(rdapUrl + domain, {
      headers: { 'Accept': 'application/rdap+json' }
    });
    if (response.status === 404) {
      return { domain, status: 'available', note: 'RDAP: Not found' };
    } else if (response.ok) {
      const data = await response.json();
      return { domain, status: 'taken', note: `RDAP: Registered` };
    }
    return null;
  } catch (err) {
    return null;
  }
}

function calculateQRVersion(url) {
  // Rough estimate of QR version needed for alphanumeric URL
  const len = url.length;
  if (len <= 25) return { version: 1, size: '21×21' };
  if (len <= 47) return { version: 2, size: '25×25' };
  if (len <= 77) return { version: 3, size: '29×29' };
  if (len <= 114) return { version: 4, size: '33×33' };
  return { version: '5+', size: '37×37+' };
}

async function main() {
  console.log('🔍 Short Domain Availability Checker for QR Optimization\n');
  console.log('=' .repeat(60));
  
  // Calculate current situation
  const currentDomain = 'prompt.ac';
  const exampleCode = 'TESTCODE';
  const currentUrl = `https://${currentDomain}/${exampleCode}`;
  const currentQR = calculateQRVersion(currentUrl);
  
  console.log(`\n📊 Current Setup:`);
  console.log(`   Domain: ${currentDomain}`);
  console.log(`   Example URL: ${currentUrl} (${currentUrl.length} chars)`);
  console.log(`   QR Version: ${currentQR.version} (${currentQR.size})`);
  
  console.log(`\n🎯 Goal: Find shorter domain to achieve Version 1 (21×21) QR code`);
  console.log(`   Need total URL ≤ 25 chars`);
  console.log(`   With "https://" (8) + "/" (1) + code (10) = 19 chars`);
  console.log(`   Domain must be ≤ 6 chars (e.g., "q.ac" = 4 chars)\n`);
  
  console.log('=' .repeat(60));
  console.log('\n🔎 Checking potential short domains...\n');
  
  // Priority domains to check (shortest that might work)
  const priorityDomains = [];
  
  // 1-letter.2-letter-tld domains (shortest possible)
  for (const letter of SINGLE_LETTERS.slice(0, 10)) { // Check first 10 letters
    for (const tld of ['ac', 'co', 'io', 'me', 'cc']) { // Cheapest short TLDs
      priorityDomains.push(`${letter}.${tld}`);
    }
  }
  
  // 2-letter.2-letter-tld domains
  for (const prefix of TWO_LETTER_PREFIXES) {
    for (const tld of ['ac', 'co', 'io', 'me']) {
      priorityDomains.push(`${prefix}.${tld}`);
    }
  }
  
  // Check domains
  const results = [];
  let checked = 0;
  
  for (const domain of priorityDomains.slice(0, 30)) { // Limit to 30 for speed
    const url = `https://${domain}/${exampleCode}`;
    const qr = calculateQRVersion(url);
    
    // Quick DNS check
    const dnsResult = await checkDomainWhois(domain);
    
    results.push({
      domain,
      urlLength: url.length,
      qrVersion: qr.version,
      qrSize: qr.size,
      status: dnsResult.status,
      note: dnsResult.note
    });
    
    checked++;
    process.stdout.write(`\r   Checked ${checked}/${Math.min(priorityDomains.length, 30)} domains...`);
  }
  
  console.log('\n');
  
  // Display results
  console.log('📋 Results (sorted by URL length):\n');
  console.log('Domain'.padEnd(12) + 'URL Len'.padEnd(10) + 'QR Ver'.padEnd(8) + 'QR Size'.padEnd(10) + 'Status');
  console.log('-'.repeat(60));
  
  results
    .sort((a, b) => a.urlLength - b.urlLength)
    .forEach(r => {
      const statusIcon = r.status === 'possibly-available' ? '🟢' : 
                         r.status === 'taken' ? '🔴' : '⚪';
      console.log(
        r.domain.padEnd(12) + 
        String(r.urlLength).padEnd(10) +
        String(r.qrVersion).padEnd(8) +
        r.qrSize.padEnd(10) +
        `${statusIcon} ${r.status}`
      );
    });
  
  // Find potential winners
  const available = results.filter(r => r.status === 'possibly-available' && r.qrVersion === 1);
  
  console.log('\n' + '=' .repeat(60));
  
  if (available.length > 0) {
    console.log('\n🎉 Potentially available domains that could achieve Version 1 QR:');
    available.forEach(r => console.log(`   ${r.domain} → ${r.qrSize} QR code!`));
  } else {
    console.log('\n⚠️  No available single-letter domains found (they\'re usually premium)');
  }
  
  console.log('\n💡 Recommendations:');
  console.log('   1. Single-letter .ac domains (a.ac, q.ac) are usually $1000+/yr premium');
  console.log('   2. Two-letter domains (qr.ac, ac.co) might be available for ~$30-50/yr');
  console.log('   3. Consider URL shortener services (bit.ly, t.co) as alternative');
  console.log('   4. Your current prompt.ac is actually quite good!');
  
  console.log('\n🔗 Check availability and pricing:');
  console.log('   - https://www.namecheap.com/domains/registration/');
  console.log('   - https://www.porkbun.com/products/domains');
  console.log('   - https://tld-list.com/ (price comparison)');
  console.log('   - https://spaceship.com/ (often cheapest)\n');
}

main().catch(console.error);
