// 🚀 Graphics Performance Optimization Module
// Zero-copy pixel buffer transfer and async rendering optimizations

export class PixelBufferOptimizer {
  constructor() {
    this.asyncRenderingSupported = this.checkAsyncRenderingSupport();
    this.sharedArrayBufferSupported = this.checkSharedArrayBufferSupport();
    
    // Each canvas owns its newest pending frame. Equal dimensions do not
    // imply equal pixels, so never reuse another frame's bitmap promise.
    this.renderTickets = new WeakMap();
    
    // Performance counters
    this.stats = {
      framesProcessed: 0,
      totalTransferTime: 0,
      totalRenderTime: 0,
      asyncRenderCount: 0,
      zeroCopyCount: 0
    };
    
    console.log(`🎨 Graphics Optimizer initialized:
      - Async rendering: ${this.asyncRenderingSupported ? '✅' : '❌'}
      - SharedArrayBuffer: ${this.sharedArrayBufferSupported ? '✅' : '❌'}`);
  }

  checkAsyncRenderingSupport() {
    return typeof createImageBitmap !== 'undefined' && 
           (typeof OffscreenCanvas !== 'undefined' || typeof window !== 'undefined');
  }

  checkSharedArrayBufferSupport() {
    return typeof SharedArrayBuffer !== 'undefined' && 
           (typeof crossOriginIsolated !== 'undefined' ? crossOriginIsolated : false);
  }

  // Zero-copy ImageData creation from transferred ArrayBuffer
  createImageDataZeroCopy(transferredBuffer, width, height) {
    const startTime = performance.now();
    
    try {
      // Use transferred buffer directly - no copy!
      const pixels = new Uint8ClampedArray(transferredBuffer);
      const imageData = new ImageData(pixels, width, height);
      
      this.stats.zeroCopyCount++;
      this.stats.totalTransferTime += performance.now() - startTime;
      
      return imageData;
    } catch (error) {
      console.warn('🟡 Zero-copy ImageData creation failed, falling back:', error);
      
      // Fallback: copy the buffer
      const pixels = new Uint8ClampedArray(transferredBuffer);
      const imageData = new ImageData(new Uint8ClampedArray(pixels), width, height);
      
      this.stats.totalTransferTime += performance.now() - startTime;
      return imageData;
    }
  }

  // Upload the newest frame for this canvas and release every GPU bitmap.
  async renderImageDataAsync(imageData, ctx, x = 0, y = 0) {
    const ticket = {};
    this.renderTickets.set(ctx, ticket);
    const current = () => this.renderTickets.get(ctx) === ticket;
    if (!this.asyncRenderingSupported) {
      ctx.putImageData(imageData, x, y);
      return true;
    }

    const startTime = performance.now();
    let bitmap;
    try {
      bitmap = await createImageBitmap(imageData);
      // A slower earlier upload must never overwrite more recent input.
      if (!current()) return false;
      if (ctx.transferFromImageBitmap) ctx.transferFromImageBitmap(bitmap);
      else ctx.drawImage(bitmap, x, y);
      this.stats.asyncRenderCount++;
      return true;
    } catch (error) {
      if (!current()) return false;
      console.warn('🟡 Async bitmap rendering failed, falling back to putImageData:', error);
      ctx.putImageData(imageData, x, y);
      return true;
    } finally {
      bitmap?.close();
      this.stats.totalRenderTime += performance.now() - startTime;
    }
  }

  // Optimized dirty box rendering
  async renderDirtyBox(pixelBuffer, dirtyBox, ctx) {
    const { x, y, w: width, h: height } = dirtyBox;
    
    if (width <= 0 || height <= 0) return;
    
    try {
      const imageData = this.createImageDataZeroCopy(pixelBuffer, width, height);
      
      if (this.asyncRenderingSupported && width * height > 1024) {
        // Use async rendering for larger dirty boxes (non-blocking)
        this.renderImageDataAsync(imageData, ctx, x, y).catch(err => {
          console.warn('🟡 Async dirty box rendering failed:', err);
          // Fallback to sync rendering
          try {
            ctx.putImageData(imageData, x, y);
          } catch (syncErr) {
            console.warn('🟡 Sync fallback also failed:', syncErr);
          }
        });
      } else {
        // Use sync rendering for small dirty boxes
        ctx.putImageData(imageData, x, y);
      }
      
    } catch (error) {
      console.warn('🟡 Dirty box rendering failed:', error);
    }
  }

  // Performance monitoring
  getPerformanceStats() {
    const avgTransferTime = this.stats.totalTransferTime / Math.max(1, this.stats.framesProcessed);
    const avgRenderTime = this.stats.totalRenderTime / Math.max(1, this.stats.framesProcessed);
    
    return {
      ...this.stats,
      avgTransferTime: avgTransferTime.toFixed(2),
      avgRenderTime: avgRenderTime.toFixed(2),
      zeroCopyRatio: (this.stats.zeroCopyCount / Math.max(1, this.stats.framesProcessed) * 100).toFixed(1),
      asyncRenderRatio: (this.stats.asyncRenderCount / Math.max(1, this.stats.framesProcessed) * 100).toFixed(1)
    };
  }

  // Reset performance counters
  resetStats() {
    this.stats = {
      framesProcessed: 0,
      totalTransferTime: 0,
      totalRenderTime: 0,
      asyncRenderCount: 0,
      zeroCopyCount: 0
    };
  }

  // Log performance stats periodically
  logPerformanceStats() {
    if (this.stats.framesProcessed % 100 === 0 && this.stats.framesProcessed > 0) {
      const stats = this.getPerformanceStats();
      console.log(`🎨 Graphics Performance (${this.stats.framesProcessed} frames):
        - Avg transfer: ${stats.avgTransferTime}ms
        - Avg render: ${stats.avgRenderTime}ms  
        - Zero-copy: ${stats.zeroCopyRatio}%
        - Async render: ${stats.asyncRenderRatio}%`);
    }
  }
}

// Global instance
export const pixelOptimizer = new PixelBufferOptimizer();
