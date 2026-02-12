#!/usr/bin/env node

import { MongoClient } from 'mongodb';

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  gray: '\x1b[90m',
};

// MongoDB connection details
const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || 'aesthetic';
const COLLECTION_NAME = 'chat-system';

// Moderation prompt - explicit URL handling
const MODERATION_PROMPT = `Rate this chat message as PG-13 appropriate or not.

ALWAYS REPLY 't' FOR URLS AND LINKS (even if they contain words like "live", "lyt", etc.)

Block if message contains: sexual content, body functions, profanity, violence, drugs, hate speech.
Allow: URLs (https://, http://), links, normal conversation.

Reply with one letter: t (appropriate) or f (inappropriate)

Message: "`;

async function testMessage(message) {
  const startTime = Date.now();
  
  try {
    const response = await fetch('http://localhost:11434/api/generate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        model: 'gemma2:2b',
        prompt: MODERATION_PROMPT + '"' + message + '"',
        stream: true,
        options: {
          num_ctx: 256,
          temperature: 0,
          num_predict: 5,  // Just t or f
        }
      })
    });

    let fullResponse = '';
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    
    process.stdout.write(`   ${colors.gray}`);
    
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      
      const chunk = decoder.decode(value);
      const lines = chunk.split('\n').filter(line => line.trim());
      
      for (const line of lines) {
        try {
          const json = JSON.parse(line);
          if (json.response) {
            fullResponse += json.response;
            process.stdout.write(json.response);
          }
        } catch (e) {
          // Skip invalid JSON lines
        }
      }
    }
    
    process.stdout.write(`${colors.reset}\n`);
    
    const latency = Date.now() - startTime;
    const response_clean = fullResponse.toLowerCase().trim();
    
    // Extract decision - simple t or f
    let cleanedResponse = response_clean;
    if (cleanedResponse.includes('</think>')) {
      cleanedResponse = cleanedResponse.split('</think>')[1]?.trim() || cleanedResponse;
    }
    
    let decision = '';
    
    // Find t or f
    if (cleanedResponse.startsWith('t')) {
      decision = 't';
    } else if (cleanedResponse.startsWith('f')) {
      decision = 'f';
    } else if (cleanedResponse.includes(' t')) {
      decision = 't';
    } else if (cleanedResponse.includes(' f')) {
      decision = 'f';
    }
    
    // Show the full response as "sentiment" for debugging
    const displayResponse = cleanedResponse.substring(0, 60);
    console.log(`   ${colors.cyan}[${displayResponse}]${colors.reset} ${colors.gray}→ ${decision || 'unknown'}${colors.reset}`);
    
    // Check decision
    let outcome = 'error';
    if (decision === 't') {
      outcome = 'pass';
    } else if (decision === 'f') {
      outcome = 'fail';
    }
    
    return { outcome, latency, response: fullResponse, sentiment: cleanedResponse };
  } catch (error) {
    const latency = Date.now() - startTime;
    console.error(`\n${colors.red}Error testing message: ${error.message}${colors.reset}`);
    return { outcome: 'error', rationale: error.message, latency };
  }
}

async function main() {
  const limit = parseInt(process.argv[2]) || 50;
  const continuous = process.argv.includes('--continuous') || process.argv.includes('-c');
  
  console.log(`${colors.cyan}${colors.bright}Connecting to MongoDB...${colors.reset}`);
  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  
  try {
    await client.connect();
    console.log(`${colors.green}Connected to MongoDB${colors.reset}`);
    
    const db = client.db(MONGODB_NAME);
    const collection = db.collection(COLLECTION_NAME);
    
    let offset = 0;
    let totalPassCount = 0;
    let totalFailCount = 0;
    let totalErrorCount = 0;
    let failedMessages = [];
    
    do {
      console.log(`\n${colors.magenta}${colors.bright}Fetching ${limit} messages (offset: ${offset}) from ${COLLECTION_NAME}...${colors.reset}\n`);
      const messages = await collection
        .find({ text: { $exists: true, $ne: '' } })
        .sort({ when: -1 })
        .skip(offset)
        .limit(limit)
        .toArray();
      
      if (messages.length === 0) {
        console.log(`${colors.yellow}No more messages to test${colors.reset}`);
        break;
      }
      
      console.log(`${colors.blue}Found ${messages.length} messages to test${colors.reset}\n`);
      console.log(colors.gray + '='.repeat(80) + colors.reset);
      
      let passCount = 0;
      let failCount = 0;
      let errorCount = 0;
      
      for (const [index, msg] of messages.entries()) {
        const messageText = msg.text || '';
        const timestamp = msg.when ? new Date(msg.when).toISOString() : 'unknown';
        const user = msg.user || 'anonymous';
        
        console.log(`\n${colors.blue}[${offset + index + 1}] ${colors.gray}${timestamp}${colors.reset}`);
        console.log(`${colors.cyan}Message: "${messageText}"${colors.reset}`);
        
        const result = await testMessage(messageText);
        
        const latencyMs = result.latency || 0;
        const latencyColor = latencyMs < 1000 ? colors.green : latencyMs < 3000 ? colors.yellow : colors.red;
        
        if (result.outcome === 'pass') {
          passCount++;
          console.log(`${colors.green}${colors.bright}✅ PASS${colors.reset} ${latencyColor}⏱️  ${latencyMs}ms${colors.reset}`);
        } else if (result.outcome === 'fail') {
          failCount++;
          console.log(`${colors.red}${colors.bright}❌ FAIL${colors.reset} ${latencyColor}⏱️  ${latencyMs}ms${colors.reset}`);
          failedMessages.push({ messageText, rationale: result.sentiment || 'N/A', user, timestamp });
        } else {
          errorCount++;
          console.log(`${colors.yellow}${colors.bright}⚠️  ERROR${colors.reset} ${latencyColor}⏱️  ${latencyMs}ms${colors.reset}`);
        }
        
        console.log(colors.gray + '-'.repeat(80) + colors.reset);
      }
      
      totalPassCount += passCount;
      totalFailCount += failCount;
      totalErrorCount += errorCount;
      
      console.log(`\n${colors.magenta}Batch Summary:${colors.reset}`);
      console.log(`${colors.green}✅ Passed: ${passCount}${colors.reset}`);
      console.log(`${colors.red}❌ Failed: ${failCount}${colors.reset}`);
      console.log(`${colors.yellow}⚠️  Errors: ${errorCount}${colors.reset}`);
      
      offset += limit;
      
      if (continuous && messages.length === limit) {
        console.log(`\n${colors.cyan}Continuing to next batch...${colors.reset}`);
        await new Promise(resolve => setTimeout(resolve, 1000));
      } else {
        break;
      }
      
    } while (continuous);
    
    console.log('\n' + colors.bright + colors.magenta + '='.repeat(80) + colors.reset);
    console.log(`${colors.bright}${colors.magenta}FINAL SUMMARY:${colors.reset}`);
    console.log(`Total messages tested: ${offset}`);
    console.log(`${colors.green}${colors.bright}✅ Passed: ${totalPassCount}${colors.reset}`);
    console.log(`${colors.red}${colors.bright}❌ Failed: ${totalFailCount}${colors.reset}`);
    console.log(`${colors.yellow}${colors.bright}⚠️  Errors: ${totalErrorCount}${colors.reset}`);
    
    if (failedMessages.length > 0) {
      console.log(`\n${colors.red}${colors.bright}FAILED MESSAGES (${failedMessages.length}):${colors.reset}`);
      for (const [i, fail] of failedMessages.entries()) {
        console.log(`\n${colors.red}${i + 1}.${colors.reset} ${colors.gray}${fail.timestamp}${colors.reset}`);
        console.log(`   ${colors.cyan}"${fail.messageText}"${colors.reset}`);
        console.log(`   ${colors.yellow}${fail.rationale}${colors.reset}`);
      }
    }
    
    console.log('\n' + colors.magenta + '='.repeat(80) + colors.reset);
    
  } catch (error) {
    console.error(`${colors.red}Error: ${error.message}${colors.reset}`);
    process.exit(1);
  } finally {
    await client.close();
    console.log(`\n${colors.green}Disconnected from MongoDB${colors.reset}`);
  }
}

main();
