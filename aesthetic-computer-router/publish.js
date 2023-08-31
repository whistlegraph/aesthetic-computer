import { exec } from 'child_process';
import { config } from 'dotenv';

config();  // Initializes and loads the .env file

const token = process.env.CLOUDFLARE_API_TOKEN;

const cmd = `CLOUDFLARE_API_TOKEN=${token} wrangler deploy`;

exec(cmd, (error, stdout, stderr) => {
    if (error) {
			console.error(`exec error: ${error}`);
			return;
    }
    console.log(`Output: ${stdout}`);
    if (stderr) {
			console.error(`stderr: ${stderr}`);
    }
});
