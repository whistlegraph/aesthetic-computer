#!/usr/bin/env python3
"""
Unit test for GameBoy turtle graphics math
Tests angle increments, sine/cosine lookups, and circle drawing
"""

import math

# Sine table matching the GameBoy version
sine_table = [
  0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45,
  48, 51, 54, 57, 59, 62, 65, 67, 70, 73, 75, 78, 80, 82, 85, 87,
  89, 91, 94, 96, 98, 100, 102, 103, 105, 107, 108, 110, 112, 113, 114, 116,
  117, 118, 119, 120, 121, 122, 123, 124, 124, 125, 126, 126, 127, 127, 127, 127,
  127, 127, 127, 127, 127, 126, 126, 125, 124, 124, 123, 122, 121, 120, 119, 118,
  117, 116, 114, 113, 112, 110, 108, 107, 105, 103, 102, 100, 98, 96, 94, 91,
  89, 87, 85, 82, 80, 78, 75, 73, 70, 67, 65, 62, 59, 57, 54, 51,
  48, 45, 42, 39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3,
  0, -3, -6, -9, -12, -15, -18, -21, -24, -27, -30, -33, -36, -39, -42, -45,
  -48, -51, -54, -57, -59, -62, -65, -67, -70, -73, -75, -78, -80, -82, -85, -87,
  -89, -91, -94, -96, -98, -100, -102, -103, -105, -107, -108, -110, -112, -113, -114, -116,
  -117, -118, -119, -120, -121, -122, -123, -124, -124, -125, -126, -126, -127, -127, -127, -127,
  -127, -127, -127, -127, -127, -126, -126, -125, -124, -124, -123, -122, -121, -120, -119, -118,
  -117, -116, -114, -113, -112, -110, -108, -107, -105, -103, -102, -100, -98, -96, -94, -91,
  -89, -87, -85, -82, -80, -78, -75, -73, -70, -67, -65, -62, -59, -57, -54, -51,
  -48, -45, -42, -39, -36, -33, -30, -27, -24, -21, -18, -15, -12, -9, -6, -3
]

def sin_lookup(angle):
    """Lookup sine value (angle 0-255 maps to 0-360 degrees)"""
    return sine_table[angle & 0xFF]

def cos_lookup(angle):
    """Lookup cosine value"""
    return sine_table[(angle + 64) & 0xFF]

def test_angle_progression():
    """Test that angles progress correctly through full circle"""
    print("Testing angle progression:")
    angle = 64 << 8  # Start at 90 degrees in 8.8 fixed point
    
    for step in range(0, 360, 30):
        angle_byte = (angle >> 8) & 0xFF
        actual_deg = (angle_byte / 256.0) * 360.0
        print(f"  Step {step:3d}: angle_byte={angle_byte:3d} ({actual_deg:6.2f}°)")
        angle = (angle + 182) & 0xFFFF
    
    final_angle = (angle >> 8) & 0xFF
    final_deg = (final_angle / 256.0) * 360.0
    print(f"  Final after 360 steps: {final_angle} ({final_deg:.2f}°)")
    print()

def test_circle_drawing():
    """Simulate drawing a circle and check if it closes"""
    print("Simulating circle drawing:")
    
    angle = 64 << 8  # 90 degrees in 8.8 fixed point
    turtle_x_fixed = 80 << 8
    turtle_y_fixed = 72 << 8
    radius_fixed = 30 << 8
    
    # Move up by radius
    turtle_y_fixed -= radius_fixed
    
    # Convert to signed 16-bit like in C
    def to_int16(val):
        val = val & 0xFFFF
        if val >= 0x8000:
            return val - 0x10000
        return val
    
    turtle_x_fixed = to_int16(turtle_x_fixed)
    turtle_y_fixed = to_int16(turtle_y_fixed)
    
    start_x = turtle_x_fixed >> 8
    start_y = turtle_y_fixed >> 8
    print(f"  Start position: ({start_x}, {start_y})")
    
    positions = [(start_x, start_y)]
    
    # Draw 360 steps
    for step in range(360):
        angle_byte = (angle >> 8) & 0xFF
        
        # Calculate deltas (signed 8-bit values)
        sin_val = sin_lookup(angle_byte)
        cos_val = cos_lookup(angle_byte)
        
        dx = (cos_val * 128) // 64
        dy = (sin_val * 128) // 64
        
        turtle_x_fixed = to_int16(turtle_x_fixed + dx)
        turtle_y_fixed = to_int16(turtle_y_fixed + dy)
        
        # Turn right
        angle = (angle + 182) & 0xFFFF
        
        pixel_x = (turtle_x_fixed >> 8) & 0xFF
        pixel_y = (turtle_y_fixed >> 8) & 0xFF
        positions.append((pixel_x, pixel_y))
        
        if step % 60 == 0:
            print(f"  Step {step:3d}: pos=({pixel_x:3d},{pixel_y:3d}) angle={angle_byte:3d} dx={dx:3d} dy={dy:3d}")
    
    end_x = turtle_x_fixed >> 8
    end_y = turtle_y_fixed >> 8
    print(f"  End position:   ({end_x}, {end_y})")
    
    distance = math.sqrt((end_x - start_x)**2 + (end_y - start_y)**2)
    print(f"  Distance from start: {distance:.2f} pixels")
    
    # Check for sudden jumps (scattering issue)
    max_jump = 0
    for i in range(1, len(positions)):
        dx = positions[i][0] - positions[i-1][0]
        dy = positions[i][1] - positions[i-1][1]
        jump = math.sqrt(dx*dx + dy*dy)
        if jump > max_jump:
            max_jump = jump
            if jump > 5:  # Flag large jumps
                print(f"  ⚠ Large jump at step {i}: {jump:.2f} pixels from {positions[i-1]} to {positions[i]}")
    
    print(f"  Max jump between steps: {max_jump:.2f} pixels")
    print()

def test_trig_accuracy():
    """Compare our lookup table to actual trig functions"""
    print("Testing trig accuracy (0°, 90°, 180°, 270°):")
    
    for degrees in [0, 90, 180, 270]:
        angle_byte = int((degrees / 360.0) * 256) & 0xFF
        
        sin_table_val = sin_lookup(angle_byte)
        cos_table_val = cos_lookup(angle_byte)
        
        radians = math.radians(degrees)
        sin_actual = math.sin(radians) * 127  # Scale to match our -127 to 127 range
        cos_actual = math.cos(radians) * 127
        
        sin_error = abs(sin_table_val - sin_actual)
        cos_error = abs(cos_table_val - cos_actual)
        
        print(f"  {degrees:3d}°: sin={sin_table_val:4d} (err={sin_error:.1f}) cos={cos_table_val:4d} (err={cos_error:.1f})")
    print()

if __name__ == "__main__":
    print("=" * 60)
    print("GameBoy Turtle Graphics Math Test")
    print("=" * 60)
    print()
    
    test_angle_progression()
    test_trig_accuracy()
    test_circle_drawing()
    
    print("=" * 60)
    print("Tests complete!")
    print("=" * 60)
