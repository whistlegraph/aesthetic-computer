#!/usr/bin/env node

/**
 * Setup Script for Painting Tools
 * 
 * Helps configure environment for testing tools locally or against live.
 */

console.log(`
╔═══════════════════════════════════════════════════════════════╗
║                 🎨 Painting Tools Setup                       ║
╚═══════════════════════════════════════════════════════════════╝

This script will help you set up the environment for the painting tools.

You'll need:
  1. MongoDB connection (local or live)
  2. Digital Ocean Spaces credentials (for inspecting storage)
  3. API endpoint (local server or live)

`);

const env = process.env;

console.log('Current Configuration:\n');

// Check MongoDB
if (env.MONGODB_CONNECTION_STRING && env.MONGODB_NAME) {
  console.log('✅ MongoDB: Configured');
  console.log(`   Database: ${env.MONGODB_NAME}`);
  console.log(`   URI: ${env.MONGODB_CONNECTION_STRING.replace(/\/\/.*:.*@/, '//***:***@')}`);
} else {
  console.log('❌ MongoDB: Not configured');
  console.log('   Set: MONGODB_CONNECTION_STRING and MONGODB_NAME');
}
console.log();

// Check Digital Ocean
if (env.DO_SPACES_KEY && env.DO_SPACES_SECRET) {
  console.log('✅ Digital Ocean Spaces: Configured');
  console.log(`   Endpoint: ${env.DO_SPACES_ENDPOINT || 'nyc3.digitaloceanspaces.com'}`);
  console.log(`   Bucket: ${env.DO_SPACES_BUCKET || 'aesthetic-computer'}`);
} else {
  console.log('❌ Digital Ocean Spaces: Not configured');
  console.log('   Set: DO_SPACES_KEY, DO_SPACES_SECRET');
}
console.log();

// Check API
if (env.AC_API) {
  console.log('✅ API Endpoint: Configured');
  console.log(`   Target: ${env.AC_API}`);
  console.log(`   Type: ${env.AC_API.includes('localhost') ? 'LOCAL' : 'LIVE'}`);
} else {
  console.log('⚠️  API Endpoint: Using default');
  console.log('   Target: http://localhost:8888');
  console.log('   Set AC_API to override');
}
console.log();

console.log('─'.repeat(65));
console.log('\n📝 Setup Instructions:\n');

if (!env.MONGODB_CONNECTION_STRING) {
  console.log('1. For local MongoDB:');
  console.log('   export MONGODB_CONNECTION_STRING="mongodb://localhost:27017"');
  console.log('   export MONGODB_NAME="aesthetic"');
  console.log();
  console.log('   Or for live MongoDB:');
  console.log('   export MONGODB_CONNECTION_STRING="mongodb+srv://user:pass@cluster..."');
  console.log('   export MONGODB_NAME="aesthetic"');
  console.log();
}

if (!env.DO_SPACES_KEY) {
  console.log('2. For Digital Ocean Spaces:');
  console.log('   export DO_SPACES_KEY="your-key"');
  console.log('   export DO_SPACES_SECRET="your-secret"');
  console.log('   export DO_SPACES_ENDPOINT="nyc3.digitaloceanspaces.com"');
  console.log('   export DO_SPACES_BUCKET="aesthetic-computer"');
  console.log();
}

if (!env.AC_API) {
  console.log('3. For API endpoint:');
  console.log('   export AC_API="http://localhost:8888"  # Local');
  console.log('   export AC_API="https://aesthetic.computer"  # Live');
  console.log();
}

console.log('─'.repeat(65));
console.log('\n🧪 Quick Tests:\n');
console.log('  # Test MongoDB connection');
console.log('  node inspect-mongodb.mjs --count');
console.log();
console.log('  # Test Digital Ocean Spaces');
console.log('  node inspect-spaces.mjs --recent 5');
console.log();
console.log('  # Test API endpoints');
console.log('  node inspect-api.mjs --tv');
console.log();

if (env.MONGODB_CONNECTION_STRING && env.DO_SPACES_KEY && env.AC_API) {
  console.log('✨ All configured! Ready to run tools.\n');
} else {
  console.log('⚠️  Some configuration missing. See instructions above.\n');
}
