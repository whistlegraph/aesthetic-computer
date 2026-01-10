//! Vello WASM Bindings for Aesthetic Computer
//!
//! Provides a JavaScript-friendly API for Vello's GPU-accelerated 2D rendering.
//! This module creates a Scene that can be used with the Vello renderer.

use wasm_bindgen::prelude::*;
use vello::{
    kurbo::{Affine, Circle, Line, Rect, RoundedRect, Stroke},
    peniko::{Color, Fill},
    Scene,
};

// Initialize panic hook for better error messages
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
    web_sys::console::log_1(&"Vello WASM initialized".into());
}

/// Scene builder for Vello - constructs draw commands
#[wasm_bindgen]
pub struct VelloScene {
    scene: Scene,
    clear_r: u8,
    clear_g: u8,
    clear_b: u8,
    clear_a: u8,
    width: u32,
    height: u32,
}

#[wasm_bindgen]
impl VelloScene {
    /// Create a new Vello scene builder
    #[wasm_bindgen(constructor)]
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            scene: Scene::new(),
            clear_r: 0,
            clear_g: 0,
            clear_b: 0,
            clear_a: 255,
            width,
            height,
        }
    }

    /// Resize the scene
    #[wasm_bindgen]
    pub fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
    }

    /// Clear the scene with a color
    #[wasm_bindgen]
    pub fn clear(&mut self, r: u8, g: u8, b: u8, a: u8) {
        self.scene.reset();
        self.clear_r = r;
        self.clear_g = g;
        self.clear_b = b;
        self.clear_a = a;
    }

    /// Draw a line
    #[wasm_bindgen]
    pub fn line(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, r: u8, g: u8, b: u8, a: u8, stroke_width: f64) {
        let color = Color::rgba8(r, g, b, a);
        let stroke = Stroke::new(stroke_width);
        self.scene.stroke(
            &stroke,
            Affine::IDENTITY,
            color,
            None,
            &Line::new((x1, y1), (x2, y2)),
        );
    }

    /// Draw a filled rectangle
    #[wasm_bindgen]
    pub fn rect(&mut self, x: f64, y: f64, w: f64, h: f64, r: u8, g: u8, b: u8, a: u8) {
        let color = Color::rgba8(r, g, b, a);
        self.scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            color,
            None,
            &Rect::new(x, y, x + w, y + h),
        );
    }

    /// Draw a stroked rectangle
    #[wasm_bindgen]
    pub fn rect_stroke(&mut self, x: f64, y: f64, w: f64, h: f64, r: u8, g: u8, b: u8, a: u8, stroke_width: f64) {
        let color = Color::rgba8(r, g, b, a);
        let stroke = Stroke::new(stroke_width);
        self.scene.stroke(
            &stroke,
            Affine::IDENTITY,
            color,
            None,
            &Rect::new(x, y, x + w, y + h),
        );
    }

    /// Draw a filled circle
    #[wasm_bindgen]
    pub fn circle(&mut self, cx: f64, cy: f64, radius: f64, r: u8, g: u8, b: u8, a: u8) {
        let color = Color::rgba8(r, g, b, a);
        self.scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            color,
            None,
            &Circle::new((cx, cy), radius),
        );
    }

    /// Draw a stroked circle
    #[wasm_bindgen]
    pub fn circle_stroke(&mut self, cx: f64, cy: f64, radius: f64, r: u8, g: u8, b: u8, a: u8, stroke_width: f64) {
        let color = Color::rgba8(r, g, b, a);
        let stroke = Stroke::new(stroke_width);
        self.scene.stroke(
            &stroke,
            Affine::IDENTITY,
            color,
            None,
            &Circle::new((cx, cy), radius),
        );
    }

    /// Draw a rounded rectangle
    #[wasm_bindgen]
    pub fn rounded_rect(&mut self, x: f64, y: f64, w: f64, h: f64, radius: f64, r: u8, g: u8, b: u8, a: u8) {
        let color = Color::rgba8(r, g, b, a);
        self.scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            color,
            None,
            &RoundedRect::new(x, y, x + w, y + h, radius),
        );
    }

    /// Get the width
    #[wasm_bindgen]
    pub fn width(&self) -> u32 {
        self.width
    }

    /// Get the height
    #[wasm_bindgen]
    pub fn height(&self) -> u32 {
        self.height
    }

    /// Get the clear color R
    #[wasm_bindgen]
    pub fn clear_r(&self) -> u8 {
        self.clear_r
    }

    /// Get the clear color G
    #[wasm_bindgen]
    pub fn clear_g(&self) -> u8 {
        self.clear_g
    }

    /// Get the clear color B
    #[wasm_bindgen]
    pub fn clear_b(&self) -> u8 {
        self.clear_b
    }

    /// Get the clear color A
    #[wasm_bindgen]
    pub fn clear_a(&self) -> u8 {
        self.clear_a
    }
}

impl Default for VelloScene {
    fn default() -> Self {
        Self::new(800, 600)
    }
}
