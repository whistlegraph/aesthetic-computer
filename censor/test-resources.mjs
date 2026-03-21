import { MongoClient } from 'mongodb';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

const MONGODB_URI = process.env.MONGODB_CONNECTION_STRING;
const OLLAMA_API = 'http://localhost:11434/api/generate';
const MODEL = 'gemma2:2b';
const MESSAGE_COUNT = 200;

// ANSI colors
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  cyan: '\x1b[36m',
  yellow: '\x1b[33m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  magenta: '\x1b[35m',
};

// Get Ollama process stats
async function getOllamaStats() {
  try {
    // Get PID of ollama serve process
    const { stdout: psOut } = await execAsync("pgrep -f 'ollama serve' | head -1");
    const pid = psOut.trim();
    
    if (!pid) {
      return { rss: 0, cpu: 0, error: 'Ollama process not found' };
    }
    
    // Get memory and CPU usage using ps
    const { stdout } = await execAsync(`ps -p ${pid} -o rss,pcpu --no-headers`);
    const [rss, cpu] = stdout.trim().split(/\s+/);
    
    return {
      pid: parseInt(pid),
      rss: parseInt(rss), // RSS in KB
      rssGB: (parseInt(rss) / 1024 / 1024).toFixed(2), // Convert to GB
      cpu: parseFloat(cpu),
    };
  } catch (error) {
    return { rss: 0, cpu: 0, error: error.message };
  }
}

// PG-13 content filter prompt
const systemPrompt = `ALWAYS REPLY 't' FOR URLS (http://, https://, www., or domain.tld patterns).

You are a PG-13 content filter for a chat room. Reply ONLY with 't' (true/allow) or 'f' (false/block).

Block (reply 'f') if the message contains:
- Sexual content or innuendo
- Body functions (bathroom humor, gross-out content)
- Profanity or explicit language
- Violence or threats
- Drug references
- Hate speech or slurs

Allow (reply 't') for:
- Normal conversation
- URLs and links
- Questions and discussions
- Greetings and casual chat

Reply with just 't' or 'f', nothing else.`;

async function testMessage(message) {
  const prompt = `${systemPrompt}\n\nMessage: "${message}"\n\nReply (t/f):`;
  
  const response = await fetch(OLLAMA_API, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      model: MODEL,
      prompt: prompt,
      stream: false,
    }),
  });

  const data = await response.json();
  const result = data.response.trim().toLowerCase();
  
  return {
    decision: result.includes('t') ? 't' : 'f',
    responseTime: data.total_duration / 1e9, // Convert to seconds
  };
}

async function main() {
  console.log(`${colors.bright}${colors.cyan}=== Ollama Resource Usage Test ===${colors.reset}\n`);
  console.log(`Model: ${colors.yellow}${MODEL}${colors.reset}`);
  console.log(`Messages: ${colors.yellow}${MESSAGE_COUNT}${colors.reset}\n`);

  // Get baseline stats before starting
  console.log(`${colors.cyan}📊 Baseline Stats${colors.reset}`);
  const baselineStats = await getOllamaStats();
  if (baselineStats.error) {
    console.log(`${colors.red}Error: ${baselineStats.error}${colors.reset}`);
    return;
  }
  console.log(`  PID: ${baselineStats.pid}`);
  console.log(`  RAM: ${colors.yellow}${baselineStats.rssGB} GB${colors.reset} (${baselineStats.rss.toLocaleString()} KB)`);
  console.log(`  CPU: ${colors.yellow}${baselineStats.cpu}%${colors.reset}\n`);

  // Connect to MongoDB
  const client = new MongoClient(MONGODB_URI);
  await client.connect();
  const db = client.db('aesthetic');
  const collection = db.collection('chat-system');

  // Get random messages
  const messages = await collection
    .aggregate([{ $sample: { size: MESSAGE_COUNT } }])
    .toArray();

  console.log(`${colors.cyan}🚀 Starting test with ${MESSAGE_COUNT} messages...${colors.reset}\n`);

  const stats = {
    total: 0,
    passed: 0,
    failed: 0,
    totalResponseTime: 0,
    peakRSS: baselineStats.rss,
    peakCPU: baselineStats.cpu,
    samples: [],
  };

  const startTime = Date.now();
  
  for (let i = 0; i < messages.length; i++) {
    const msg = messages[i];
    const text = msg.text || '';
    
    // Test the message
    const result = await testMessage(text);
    stats.total++;
    stats.totalResponseTime += result.responseTime;
    
    if (result.decision === 't') {
      stats.passed++;
    } else {
      stats.failed++;
    }

    // Sample resource usage every 10 messages
    if (i % 10 === 0 || i === messages.length - 1) {
      const currentStats = await getOllamaStats();
      stats.samples.push({
        messageNum: i + 1,
        rss: currentStats.rss,
        cpu: currentStats.cpu,
      });
      
      if (currentStats.rss > stats.peakRSS) stats.peakRSS = currentStats.rss;
      if (currentStats.cpu > stats.peakCPU) stats.peakCPU = currentStats.cpu;
      
      // Progress indicator
      const progress = ((i + 1) / messages.length * 100).toFixed(1);
      const bar = '█'.repeat(Math.floor(progress / 5)) + '░'.repeat(20 - Math.floor(progress / 5));
      process.stdout.write(`\r${colors.cyan}Progress:${colors.reset} [${bar}] ${progress}% (${i + 1}/${messages.length}) - RAM: ${currentStats.rssGB}GB, CPU: ${currentStats.cpu}%`);
    }
  }

  const endTime = Date.now();
  const totalTime = (endTime - startTime) / 1000;

  console.log(`\n\n${colors.bright}${colors.green}=== Test Complete ===${colors.reset}\n`);

  // Results summary
  console.log(`${colors.cyan}📈 Processing Stats${colors.reset}`);
  console.log(`  Total messages: ${stats.total}`);
  console.log(`  Passed (allowed): ${colors.green}${stats.passed}${colors.reset}`);
  console.log(`  Failed (blocked): ${colors.red}${stats.failed}${colors.reset}`);
  console.log(`  Total time: ${colors.yellow}${totalTime.toFixed(2)}s${colors.reset}`);
  console.log(`  Avg time/message: ${colors.yellow}${(stats.totalResponseTime / stats.total).toFixed(3)}s${colors.reset}`);
  console.log(`  Throughput: ${colors.yellow}${(stats.total / totalTime).toFixed(2)} msg/s${colors.reset}\n`);

  // Resource usage summary
  console.log(`${colors.cyan}💾 Resource Usage${colors.reset}`);
  console.log(`  Baseline RAM: ${colors.yellow}${baselineStats.rssGB} GB${colors.reset}`);
  console.log(`  Peak RAM: ${colors.yellow}${(stats.peakRSS / 1024 / 1024).toFixed(2)} GB${colors.reset} (${colors.magenta}+${((stats.peakRSS - baselineStats.rss) / 1024 / 1024).toFixed(2)} GB${colors.reset})`);
  console.log(`  Peak CPU: ${colors.yellow}${stats.peakCPU.toFixed(1)}%${colors.reset}\n`);

  // Final stats
  const finalStats = await getOllamaStats();
  console.log(`${colors.cyan}📊 Final Stats${colors.reset}`);
  console.log(`  Current RAM: ${colors.yellow}${finalStats.rssGB} GB${colors.reset}`);
  console.log(`  Current CPU: ${colors.yellow}${finalStats.cpu}%${colors.reset}\n`);

  // Show resource usage over time
  console.log(`${colors.cyan}📉 Resource Usage Over Time${colors.reset}`);
  console.log(`  ${'Message'.padEnd(10)} ${'RAM (GB)'.padEnd(10)} ${'CPU (%)'.padEnd(10)}`);
  console.log(`  ${'-'.repeat(35)}`);
  for (const sample of stats.samples) {
    const msgNum = `#${sample.messageNum}`.padEnd(10);
    const ram = (sample.rss / 1024 / 1024).toFixed(2).padEnd(10);
    const cpu = sample.cpu.toFixed(1).padEnd(10);
    console.log(`  ${msgNum} ${colors.yellow}${ram}${colors.reset} ${colors.yellow}${cpu}${colors.reset}`);
  }

  await client.close();
}

main().catch(console.error);
