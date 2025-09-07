#!/usr/bin/env node

// Bouncing Ball Demo with Ultra-Fast Sixel Rendering
// Targeting 30-60 FPS performance

import { performance } from 'perf_hooks';

class UltraFastSixelRenderer {
    constructor(width = 64, height = 64, scale = 3) {
        this.width = width;
        this.height = height;
        this.scale = scale;
        this.outputWidth = width * scale;
        this.outputHeight = height * scale;
        
        // Pre-allocate buffer
        this.buffer = new Uint8Array(width * height);
        
        // Pre-compute scaled positions for faster rendering
        this.scaleMap = new Array(this.outputWidth * this.outputHeight);
        for (let y = 0; y < this.outputHeight; y++) {
            for (let x = 0; x < this.outputWidth; x++) {
                const srcX = Math.floor(x / scale);
                const srcY = Math.floor(y / scale);
                this.scaleMap[y * this.outputWidth + x] = srcY * width + srcX;
            }
        }
        
        // Simple 2-color palette for maximum speed
        this.palette = [
            '\x1b[48;2;0;0;0m',     // Black background
            '\x1b[48;2;255;255;255m' // White ball
        ];
        
        // Pre-build sixel color definitions
        this.sixelColors = '#0;2;0;0;0#1;2;100;100;100';
        
        // Alternative screen buffer for fastest rendering
        process.stdout.write('\x1b[?1049h\x1b[H');
    }
    
    clear() {
        this.buffer.fill(0);
    }
    
    // Ultra-fast circle drawing using integer math
    drawCircle(centerX, centerY, radius, color = 1) {
        const radiusSquared = radius * radius;
        const minX = Math.max(0, centerX - radius);
        const maxX = Math.min(this.width - 1, centerX + radius);
        const minY = Math.max(0, centerY - radius);
        const maxY = Math.min(this.height - 1, centerY + radius);
        
        for (let y = minY; y <= maxY; y++) {
            const dy = y - centerY;
            const dySquared = dy * dy;
            
            for (let x = minX; x <= maxX; x++) {
                const dx = x - centerX;
                const dxSquared = dx * dx;
                
                if (dxSquared + dySquared <= radiusSquared) {
                    this.buffer[y * this.width + x] = color;
                }
            }
        }
    }
    
    // Ultra-fast sixel rendering using direct method
    render() {
        const bands = Math.ceil(this.outputHeight / 6);
        let sixel = `\x1b[H\x1bPq${this.sixelColors}`;
        
        for (let band = 0; band < bands; band++) {
            const startY = band * 6;
            
            // Process each color
            for (let colorIndex = 0; colorIndex < 2; colorIndex++) {
                if (colorIndex > 0) sixel += '$'; // Carriage return for new color
                sixel += `#${colorIndex}`;
                
                let hasPixels = false;
                let currentRun = '';
                let runLength = 0;
                let lastChar = '';
                
                for (let x = 0; x < this.outputWidth; x++) {
                    let sixelChar = 0;
                    
                    // Check 6 pixels vertically for this band
                    for (let bit = 0; bit < 6; bit++) {
                        const y = startY + bit;
                        if (y < this.outputHeight) {
                            const srcIndex = this.scaleMap[y * this.outputWidth + x];
                            const pixelColor = this.buffer[srcIndex];
                            if (pixelColor === colorIndex) {
                                sixelChar |= (1 << bit);
                            }
                        }
                    }
                    
                    const char = String.fromCharCode(63 + sixelChar);
                    
                    if (char === lastChar && runLength < 99) {
                        runLength++;
                    } else {
                        if (runLength > 3) {
                            currentRun += `!${runLength}${lastChar}`;
                        } else {
                            currentRun += lastChar.repeat(runLength);
                        }
                        lastChar = char;
                        runLength = 1;
                    }
                    
                    if (sixelChar > 0) hasPixels = true;
                }
                
                // Add final run
                if (runLength > 3) {
                    currentRun += `!${runLength}${lastChar}`;
                } else {
                    currentRun += lastChar.repeat(runLength);
                }
                
                if (hasPixels) {
                    sixel += currentRun;
                }
                currentRun = '';
                runLength = 0;
                lastChar = '';
            }
            
            if (band < bands - 1) sixel += '-'; // New line for next band
        }
        
        sixel += '\x1b\\'; // End sixel sequence
        process.stdout.write(sixel);
    }
    
    cleanup() {
        process.stdout.write('\x1b[?1049l'); // Exit alt screen
    }
}

class BouncingBall {
    constructor(renderer) {
        this.renderer = renderer;
        this.x = renderer.width / 2;
        this.y = renderer.height / 2;
        this.vx = 0.8;
        this.vy = 0.6;
        this.radius = 4;
        
        // Boundary collision with radius consideration
        this.minX = this.radius;
        this.maxX = renderer.width - this.radius;
        this.minY = this.radius;
        this.maxY = renderer.height - this.radius;
    }
    
    update() {
        // Update position
        this.x += this.vx;
        this.y += this.vy;
        
        // Bounce off walls
        if (this.x <= this.minX || this.x >= this.maxX) {
            this.vx = -this.vx;
            this.x = Math.max(this.minX, Math.min(this.maxX, this.x));
        }
        
        if (this.y <= this.minY || this.y >= this.maxY) {
            this.vy = -this.vy;
            this.y = Math.max(this.minY, Math.min(this.maxY, this.y));
        }
    }
    
    draw() {
        this.renderer.drawCircle(
            Math.round(this.x), 
            Math.round(this.y), 
            this.radius
        );
    }
}

async function runBouncingBallDemo(duration = 10000) {
    const renderer = new UltraFastSixelRenderer(64, 64, 3);
    const ball = new BouncingBall(renderer);
    
    console.log('🏀 Starting Bouncing Ball Demo');
    console.log(`Target: 30-60 FPS for ${duration/1000} seconds`);
    console.log('Resolution: 64x64 @ 3x scale = 192x192 output');
    console.log('Press Ctrl+C to exit\n');
    
    const startTime = performance.now();
    let frameCount = 0;
    let lastFpsTime = startTime;
    let lastFpsFrames = 0;
    
    const animate = () => {
        const currentTime = performance.now();
        
        // Clear and update
        renderer.clear();
        ball.update();
        ball.draw();
        renderer.render();
        
        frameCount++;
        
        // Calculate FPS every 60 frames
        if (frameCount % 60 === 0) {
            const elapsed = currentTime - lastFpsTime;
            const fps = (frameCount - lastFpsFrames) / (elapsed / 1000);
            process.stdout.write(`\x1b[${renderer.outputHeight + 2};1H`);
            process.stdout.write(`\x1b[K🎯 FPS: ${fps.toFixed(1)} | Frame: ${frameCount} | Time: ${((currentTime - startTime)/1000).toFixed(1)}s`);
            lastFpsTime = currentTime;
            lastFpsFrames = frameCount;
        }
        
        // Continue animation
        if (currentTime - startTime < duration) {
            setImmediate(animate);
        } else {
            const totalTime = currentTime - startTime;
            const avgFps = frameCount / (totalTime / 1000);
            
            renderer.cleanup();
            console.log(`\n\n🏆 Demo Complete!`);
            console.log(`📊 Average FPS: ${avgFps.toFixed(1)}`);
            console.log(`📈 Total Frames: ${frameCount}`);
            console.log(`⏱️  Total Time: ${(totalTime/1000).toFixed(1)}s`);
            
            if (avgFps >= 30) {
                console.log(`✅ SUCCESS: Achieved target 30+ FPS!`);
            } else {
                console.log(`❌ Below target: ${avgFps.toFixed(1)} FPS`);
            }
        }
    };
    
    // Handle cleanup on exit
    process.on('SIGINT', () => {
        renderer.cleanup();
        console.log('\n\n👋 Demo interrupted');
        process.exit(0);
    });
    
    animate();
}

// CLI interface
const args = process.argv.slice(2);
const duration = args[0] ? parseInt(args[0]) : 10000;

runBouncingBallDemo(duration);
