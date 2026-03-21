/**
 * Screenshot generation worker for aesthetic.computer
 * Based on Cloudflare's Browser Rendering API example
 */

// Polyfill Buffer for @cloudflare/puppeteer compatibility
// @ts-ignore
if (typeof Buffer === 'undefined') {
  // @ts-ignore
  globalThis.Buffer = class Buffer extends Uint8Array {
    static isBuffer(obj: any): boolean {
      return obj instanceof Uint8Array;
    }
    
    static from(arrayBuffer: ArrayBuffer | Uint8Array | number[], encoding?: string): Uint8Array {
      if (arrayBuffer instanceof ArrayBuffer) {
        return new Uint8Array(arrayBuffer);
      }
      if (arrayBuffer instanceof Uint8Array) {
        return arrayBuffer;
      }
      if (Array.isArray(arrayBuffer)) {
        return new Uint8Array(arrayBuffer);
      }
      return new Uint8Array(0);
    }
  };
}

import puppeteer from "@cloudflare/puppeteer";

export interface Env {
  MYBROWSER: Fetcher;
  BROWSER: DurableObjectNamespace;
  
  // Environment variables
  ENVIRONMENT: string;
  DOMAIN: string;
  CACHE_TTL_SECONDS: string;
  CDN_CACHE_TTL_SECONDS: string;
  MAX_SCREENSHOT_AGE_MS: string;
  BROWSER_TIMEOUT_MS: string;
  MAX_VIEWPORT_WIDTH: string;
  MAX_VIEWPORT_HEIGHT: string;
  MAX_REQUESTS_PER_MINUTE: string;
  MAX_BROWSER_SESSIONS: string;
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    
    // Health check endpoint
    if (url.pathname === '/health') {
      return Response.json({ status: 'ok', timestamp: Date.now() });
    }
    
    // Parse screenshot request from URL
    // Format: /icon/128x128/welcome.png or /preview/1200x630/prompt~wipe.png
    const pathMatch = url.pathname.match(/^\/(icon|preview)\/(\d+)x(\d+)\/(.+)$/);
    
    if (!pathMatch) {
      return Response.json(
        { error: 'Invalid request format. Use /icon/WxH/piece.png or /preview/WxH/piece.png' },
        { status: 400 }
      );
    }
    
    const [, type, widthStr, heightStr, piece] = pathMatch;
    const width = parseInt(widthStr);
    const height = parseInt(heightStr);
    
    // Validate dimensions
    const maxWidth = parseInt(env.MAX_VIEWPORT_WIDTH || '1920');
    const maxHeight = parseInt(env.MAX_VIEWPORT_HEIGHT || '1080');
    
    if (width > maxWidth || height > maxHeight) {
      return Response.json(
        { error: `Dimensions too large. Max: ${maxWidth}x${maxHeight}` },
        { status: 400 }
      );
    }
    
    // Get Durable Object instance
    const id = env.BROWSER.idFromName("browser");
    const obj = env.BROWSER.get(id);
    
    // Forward request to Durable Object with screenshot parameters
    const screenshotRequest = new Request(request.url, {
      method: 'POST',
      headers: request.headers,
      body: JSON.stringify({
        type,
        piece,
        width,
        height,
      }),
    });
    
    return await obj.fetch(screenshotRequest);
  },
};

const KEEP_BROWSER_ALIVE_IN_SECONDS = 60;

export class Browser {
  private state: DurableObjectState;
  private env: Env;
  private browser: any;
  private keptAliveInSeconds: number;
  private storage: DurableObjectStorage;

  constructor(state: DurableObjectState, env: Env) {
    this.state = state;
    this.env = env;
    this.browser = null;
    this.keptAliveInSeconds = 0;
    this.storage = this.state.storage;
  }

  async fetch(request: Request): Promise<Response> {
    try {
      // Parse request body
      const body = await request.json() as {
        type: string;
        piece: string;
        width: number;
        height: number;
      };
      
      // Handle screenshot (icon/preview)
      return await this.takeScreenshot(body);
      
    } catch (error) {
      console.error(`Browser DO error:`, error);
      return Response.json(
        { error: 'Request failed', message: String(error) },
        { status: 500 }
      );
    }
  }

  private async takeScreenshot(body: {
    type: string;
    piece: string;
    width: number;
    height: number;
  }): Promise<Response> {
    try {
      
      // Clean piece name (remove .png extension if present)
      const pieceName = body.piece.replace(/\.png$/, '');
      
      // Build target URL with appropriate query parameter
      // The client uses ?icon=WxH or ?preview=WxH to know which resolution to render
      const baseUrl = `https://aesthetic.computer`;
      const queryParam = body.type === 'icon' 
        ? `icon=${body.width}x${body.height}`
        : `preview=${body.width}x${body.height}`;
      const targetUrl = `${baseUrl}/${pieceName}?${queryParam}`;
      
      console.log(`Taking screenshot: ${body.width}x${body.height} of ${targetUrl}`);
      
      // If there's a browser session open, re-use it
      if (!this.browser || !this.browser.isConnected()) {
        console.log(`Browser DO: Starting new instance`);
        try {
          this.browser = await puppeteer.launch(this.env.MYBROWSER);
        } catch (e) {
          console.error(`Browser DO: Could not start browser instance. Error: ${e}`);
          return Response.json(
            { error: 'Failed to start browser', message: String(e) },
            { status: 500 }
          );
        }
      }
      
      // Reset keptAlive after each call to the DO
      this.keptAliveInSeconds = 0;
      
      const page = await this.browser.newPage();
      
      try {
        // Set viewport
        await page.setViewport({ 
          width: body.width, 
          height: body.height 
        });
        
        // Navigate to page
        const timeout = parseInt(this.env.BROWSER_TIMEOUT_MS || '30000');
        console.log(`Navigating to ${targetUrl} with timeout ${timeout}ms`);
        await page.goto(targetUrl, {
          waitUntil: 'networkidle2',
          timeout,
        });
        
        console.log(`Page loaded successfully`);
        
        // Get page title to verify content loaded
        const title = await page.title();
        console.log(`Page title: ${title}`);
        
        // Wait for canvas element (aesthetic.computer renders to canvas)
        try {
          await page.waitForSelector('canvas', { timeout: 10000 });
          console.log(`Canvas element found`);
        } catch (e) {
          console.log(`No canvas found: ${e}`);
        }
        
        // Wait for animations and content to render
        await new Promise(resolve => setTimeout(resolve, 3000));
        
        console.log(`Taking screenshot...`);
        // Take screenshot - try JPEG like in CF example
        const screenshot = await page.screenshot();
        
        console.log(`Screenshot captured. Type: ${screenshot?.constructor?.name}, Length: ${screenshot?.byteLength || screenshot?.length || 0}`);
        
        // Convert to proper buffer if needed
        let imageBuffer: Uint8Array;
        if (screenshot instanceof Uint8Array) {
          imageBuffer = screenshot;
        } else if (Buffer.isBuffer(screenshot)) {
          imageBuffer = new Uint8Array(screenshot);
        } else {
          imageBuffer = new Uint8Array(screenshot as any);
        }
        
        // Close tab when done
        await page.close();
        
        // Reset keptAlive after performing tasks
        this.keptAliveInSeconds = 0;
        
        // Set alarm to keep DO alive
        const currentAlarm = await this.storage.getAlarm();
        if (currentAlarm == null) {
          console.log(`Browser DO: setting alarm`);
          const TEN_SECONDS = 10 * 1000;
          await this.storage.setAlarm(Date.now() + TEN_SECONDS);
        }
        
        // Return screenshot - convert to ArrayBuffer for Response
        // @ts-ignore - Workers runtime accepts Uint8Array
        return new Response(imageBuffer, {
          headers: {
            'Content-Type': 'image/png',
            'Cache-Control': `public, max-age=${this.env.CACHE_TTL_SECONDS || '3600'}`,
            'CDN-Cache-Control': `public, max-age=${this.env.CDN_CACHE_TTL_SECONDS || '86400'}`,
          },
        });
        
      } catch (error) {
        await page.close();
        throw error;
      }
      
    } catch (error) {
      console.error(`Screenshot error:`, error);
      return Response.json(
        { error: 'Screenshot generation failed', message: String(error) },
        { status: 500 }
      );
    }
  }

  async alarm(): Promise<void> {
    this.keptAliveInSeconds += 10;
    
    // Extend browser DO life
    if (this.keptAliveInSeconds < KEEP_BROWSER_ALIVE_IN_SECONDS) {
      console.log(
        `Browser DO: has been kept alive for ${this.keptAliveInSeconds} seconds. Extending lifespan.`
      );
      await this.storage.setAlarm(Date.now() + 10 * 1000);
    } else {
      console.log(
        `Browser DO: exceeded life of ${KEEP_BROWSER_ALIVE_IN_SECONDS}s.`
      );
      if (this.browser) {
        console.log(`Closing browser.`);
        await this.browser.close();
        this.browser = null;
      }
    }
  }
}
