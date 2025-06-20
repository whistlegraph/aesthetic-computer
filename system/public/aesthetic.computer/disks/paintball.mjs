// Paintball, 25.06.04.01.16 
// Paint on a ball. 

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
  Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
 */

// Vector math utilities
function vec3(x, y, z) {
  return { x, y, z };
}

function add(a, b) {
  return vec3(a.x + b.x, a.y + b.y, a.z + b.z);
}

function sub(a, b) {
  return vec3(a.x - b.x, a.y - b.y, a.z - b.z);
}

function scale(v, s) {
  return vec3(v.x * s, v.y * s, v.z * s);
}

function dot(a, b) {
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

function length(v) {
  return Math.sqrt(dot(v, v));
}

function normalize(v) {
  const len = length(v);
  return len > 0 ? scale(v, 1 / len) : vec3(0, 0, 0);
}

// Ray-sphere intersection
function intersectSphere(rayOrigin, rayDir, sphereCenter, sphereRadius) {
  const oc = sub(rayOrigin, sphereCenter);
  const a = dot(rayDir, rayDir);
  const b = 2.0 * dot(oc, rayDir);
  const c = dot(oc, oc) - sphereRadius * sphereRadius;
  const discriminant = b * b - 4 * a * c;

  if (discriminant < 0) {
    return -1; // No intersection
  }

  const t1 = (-b - Math.sqrt(discriminant)) / (2 * a);
  const t2 = (-b + Math.sqrt(discriminant)) / (2 * a);

  // Return the closest positive intersection
  if (t1 > 0) return t1;
  if (t2 > 0) return t2;
  return -1;
}

// Ray-plane intersection (for ground)
function intersectPlane(rayOrigin, rayDir, planePoint, planeNormal) {
  const denom = dot(planeNormal, rayDir);
  if (Math.abs(denom) < 0.0001) {
    return -1; // Ray is parallel to plane
  }

  const t = dot(sub(planePoint, rayOrigin), planeNormal) / denom;
  return t > 0 ? t : -1;
}

// Simple animated ground plane
function getAnimatedGroundHeight(time) {
  return Math.sin(time * 0.5) * 0.1; // Simple oscillating ground
}

// Fast cloud-like gradient
function getCloudGradient(rayDir, time) {
  const skyHeight = Math.max(0, rayDir.y);
  const cloudFactor =
    Math.sin(rayDir.x * 2 + time * 0.3) *
      Math.cos(rayDir.z * 1.5 + time * 0.2) *
      0.3 +
    0.7;
  return cloudFactor * skyHeight;
}

// Simple lighting calculation
function calculateLighting(point, normal, lightPos, lightColor) {
  const lightDir = normalize(sub(lightPos, point));
  const intensity = Math.max(0, dot(normal, lightDir));
  return scale(lightColor, intensity);
}

// Enhanced lighting with ambient blue tint
function calculateFullLighting(
  point,
  normal,
  lightPos,
  lightColor,
  ambient,
  materialColor,
) {
  // Direct lighting
  const lightDir = normalize(sub(lightPos, point));
  const intensity = Math.max(0, dot(normal, lightDir));
  const directLight = scale(lightColor, intensity);

  // Combine ambient (blue) + direct lighting with material
  const totalLight = add(ambient, directLight);

  return vec3(
    Math.min(1, materialColor.x * totalLight.x + ambient.x * 0.5),
    Math.min(1, materialColor.y * totalLight.y + ambient.y * 0.5),
    Math.min(1, materialColor.z * totalLight.z + ambient.z * 0.5),
  );
}

// Blend two colors with alpha
function blendColors(frontColor, backColor, alpha) {
  return vec3(
    frontColor.x * alpha + backColor.x * (1 - alpha),
    frontColor.y * alpha + backColor.y * (1 - alpha),
    frontColor.z * alpha + backColor.z * (1 - alpha),
  );
}

// Matrix rotation utility for sphere rotation
function rotateY(point, angle) {
  const cos = Math.cos(angle);
  const sin = Math.sin(angle);
  return vec3(
    point.x * cos + point.z * sin,
    point.y,
    -point.x * sin + point.z * cos,
  );
}

function rotateX(point, angle) {
  const cos = Math.cos(angle);
  const sin = Math.sin(angle);
  return vec3(
    point.x,
    point.y * cos - point.z * sin,
    point.y * sin + point.z * cos,
  );
}

// Array to store dots placed on the sphere
let sphereDots = [];

// Continuous drawing tracking
let lastDrawTime = 0;
const drawInterval = 50; // milliseconds between auto-dots when holding pen down

function boot({ resolution }) {
  // resolution(96);
}

function paint({ screen, pen }) {
  const { width, height, pixels } = screen;
  
  // Handle continuous drawing when pen is down
  if (pen.down) {
    const currentTime = Date.now();
    if (currentTime - lastDrawTime >= drawInterval) {
      lastDrawTime = currentTime;
      // Place dot at current pen position
      placeDotAtPenPosition(pen, screen, true); // true indicates continuous drawing
    }
  }
  // Time setup - needs to be first for other animations
  const time = Date.now() * 0.001; // Convert to seconds
  // Camera setup with forward movement
  const forwardSpeed = 2.0; // Units per second
  const camera = vec3(0, 1, time * forwardSpeed); // Moving forward continuously
  const viewportHeight = 2.0;
  const viewportWidth = viewportHeight * (width / height);
  const focalLength = 1.0;
  // Sphere rotation setup (used throughout the function)
  const sphereRotationY = time * 0.5; // Rotate around Y axis
  const sphereRotationX = time * 0.3; // Slight rotation around X axis

  // Sphere setup - positioned ahead and moving relative to camera (now translucent)
  const sphereCenter = vec3(0, 1, camera.z - 3); // Keep sphere ahead of moving camera
  const sphereRadius = 1.0;
  const sphereColor = vec3(0.7, 0.3, 0.3);
  const sphereAlpha = 0.7; // Main sphere is now translucent
    // PERFORMANCE OPTIMIZATION: Pre-calculate all rotated dot positions once per frame
  // Also limit the number of dots we process for performance
  const maxDotsToProcess = 100; // Limit for performance
  const dotsToProcess = sphereDots.slice(0, maxDotsToProcess);
  
  const rotatedDots = dotsToProcess.map(sphereDot => {
    let rotatedPos = rotateY(sphereDot.position, sphereRotationY);
    rotatedPos = rotateX(rotatedPos, sphereRotationX);
    return {
      worldPos: add(sphereCenter, rotatedPos),
      color: sphereDot.color,
      radius: sphereDot.radius
    };
  });
  // Removed orbiting sphere - no longer needed
  // Simple animated ground plane setup
  const animatedGroundHeight = getAnimatedGroundHeight(time);
  const groundPoint = vec3(0, animatedGroundHeight, 0);
  const groundNormal = vec3(0, 1, 0); // Pointing up
  const groundColor = vec3(0.3, 0.4, 0.3); // Slightly green
  const gridColor = vec3(0.5, 0.6, 0.5);
  // Animated light setup - follows camera movement
  const lightDistance = 3.0;
  const lightPos = vec3(
    Math.cos(time) * lightDistance,
    Math.sin(time * 0.7) * 2.0 + 2.0, // Keep light above ground
    camera.z + Math.sin(time) * lightDistance - 1, // Light moves with camera
  );
  const lightColor = vec3(1, 1, 1);
  const ambient = vec3(0.15, 0.2, 0.3); // Strong blue ambient light
  // Sky gradient colors with cloud influence
  const skyTop = vec3(0.4, 0.6, 1.0); // Deeper blue
  const skyMid = vec3(0.6, 0.8, 1.0); // Light blue
  const skyHorizon = vec3(0.9, 0.95, 1.0); // Very light blue/white

  // Render each pixel
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      // Convert screen coordinates to normalized device coordinates
      const u = (x / width) * 2 - 1;
      const v = ((height - y) / height) * 2 - 1;

      // Create ray
      const rayDir = normalize(
        vec3((u * viewportWidth) / 2, (v * viewportHeight) / 2, -focalLength),
      );
      // Test intersection with sphere
      const sphereT = intersectSphere(
        camera,
        rayDir,
        sphereCenter,
        sphereRadius,
      );      // Removed orbiting sphere intersection test

      // Simple ground intersection (much faster)
      const groundT = intersectPlane(camera, rayDir, groundPoint, groundNormal);

      // Determine which surfaces we hit and handle transparency
      let color;

      // Calculate background
      let backgroundColor;
      if (groundT > 0) {
        // Hit the simple animated ground
        const hitPoint = add(camera, scale(rayDir, groundT));

        // Enhanced grid pattern with perspective and motion
        const x = Math.abs(hitPoint.x);
        const z = Math.abs(hitPoint.z);
        const fx = x - Math.floor(x) * 1;
        const fz = z - Math.floor(z) * 1;
        const lineWidth = 0.05;

        // Add some variation based on distance for depth perception
        const distance = length(sub(hitPoint, camera));
        const fadeDistance = 15.0; // Grid fades out at distance
        const fade = Math.max(0, 1 - distance / fadeDistance);

        const gridPattern =
          fx < lineWidth ||
          fx > 1 - lineWidth ||
          fz < lineWidth ||
          fz > 1 - lineWidth
            ? fade
            : 0;

        const baseColor =
          gridPattern > 0
            ? scale(gridColor, fade)
            : scale(groundColor, fade * 0.8 + 0.2); // Slightly dim distant ground

        backgroundColor = calculateFullLighting(
          hitPoint,
          groundNormal,
          lightPos,
          lightColor,
          ambient,
          baseColor,
        );
      } else {
        // Fast animated sky with clouds
        const skyHeight = Math.max(0, rayDir.y);
        const cloudFactor = getCloudGradient(rayDir, time);

        // Simple sky gradient
        let skyColor;
        if (skyHeight > 0.5) {
          skyColor = vec3(0.4, 0.6, 1.0); // Deep blue
        } else {
          const factor = skyHeight * 2; // 0 to 1
          skyColor = add(
            scale(vec3(0.9, 0.95, 1.0), 1 - factor), // horizon
            scale(vec3(0.6, 0.8, 1.0), factor), // mid
          );
        }

        // Blend in clouds
        const cloudColor = vec3(0.9, 0.9, 0.95);
        skyColor = blendColors(cloudColor, skyColor, cloudFactor * 0.3);

        backgroundColor = add(skyColor, scale(ambient, 0.2));
      }
      // Start with background color
      color = backgroundColor;

      // Layer the translucent main sphere if we hit it
      if (sphereT > 0 && (groundT < 0 || sphereT < groundT)) {
        const hitPoint = add(camera, scale(rayDir, sphereT));
        const normal = normalize(sub(hitPoint, sphereCenter));
        const sphereColor_lit = calculateFullLighting(
          hitPoint,
          normal,
          lightPos,
          lightColor,
          ambient,
          sphereColor,
        );        // Check if we're hitting any dots on the rotating sphere
        // Use pre-calculated rotated positions for performance
        let dotColor = null;
        for (const rotatedDot of rotatedDots) {
          const distToDot = length(sub(hitPoint, rotatedDot.worldPos));
          
          if (distToDot < rotatedDot.radius) {
            // We're hitting this dot!
            dotColor = rotatedDot.color;
            break;
          }
        }

        // Use dot color if we hit a dot, otherwise use sphere color
        const finalSphereColor = dotColor || sphereColor_lit;

        // Blend main sphere with background
        color = blendColors(finalSphereColor, color, sphereAlpha);
      }      // Check for dots visible through the sphere (backside rendering)
      // Use pre-calculated rotated positions for performance
      if (sphereT <= 0 || (groundT > 0 && groundT < sphereT)) {
        // Only check if we didn't hit the front of sphere
        for (const rotatedDot of rotatedDots) {
          const dotWorldPos = rotatedDot.worldPos;

          // Quick distance check for early rejection
          const roughDistance = length(sub(dotWorldPos, camera));
          if (roughDistance > 20.0) continue; // Skip distant dots

          // Check if this dot is roughly along our ray direction
          const rayToDot = sub(dotWorldPos, camera);

          // Project the dot onto the ray direction
          const projectedDistance = dot(rayToDot, rayDir);
          
          // Early reject if dot is behind camera
          if (projectedDistance <= 0) continue;
          
          const closestPointOnRay = add(
            camera,
            scale(rayDir, projectedDistance),
          );
          const distanceFromRay = length(sub(dotWorldPos, closestPointOnRay));

          // If the dot is close to our ray and in front of camera
          if (distanceFromRay < rotatedDot.radius * 1.5) {
            // Calculate if this dot is on the back side of the sphere
            const dotToCamera = normalize(sub(camera, dotWorldPos));
            const dotNormal = normalize(sub(dotWorldPos, sphereCenter));
            const backSide = dot(dotToCamera, dotNormal) < 0;
            if (backSide) {
              // Render dot with reduced opacity (seen through sphere)
              const backDotColor = scale(rotatedDot.color, 0.4); // Dimmer
              color = blendColors(backDotColor, color, 0.3); // More transparent
              break;
            }
          }
        }
      }      // Removed orbiting sphere rendering

      // Convert to RGB and set pixel
      const pixelIndex = (y * width + x) * 4;
      pixels[pixelIndex] = Math.floor(color.x * 255); // R
      pixels[pixelIndex + 1] = Math.floor(color.y * 255); // G
      pixels[pixelIndex + 2] = Math.floor(color.z * 255); // B
      pixels[pixelIndex + 3] = 255; // A
    }
  }
}

// 🎪 Act - Handle user interactions
function act({ event: e, screen }) {
  // Handle single clicks/touches
  if (e.is("touch") || e.is("touch:1")) {
    placeDotAtPenPosition({ x: e.x, y: e.y, down: true }, screen, false);
  }
}

// Helper function to place a dot at the given pen position
function placeDotAtPenPosition(pen, screen, isContinuous = false) {
  // Convert pen coordinates to ray
  const { width, height } = screen;
  const time = Date.now() * 0.001;

  // Camera setup (same as in paint function)
  const forwardSpeed = 2.0;
  const camera = vec3(0, 1, time * forwardSpeed);
  const viewportHeight = 2.0;
  const viewportWidth = viewportHeight * (width / height);
  const focalLength = 1.0;

  // Sphere setup (same as in paint function)
  const sphereCenter = vec3(0, 1, camera.z - 3);
  const sphereRadius = 1.0;

  // Current sphere rotation (same as in paint function)
  const sphereRotationY = time * 0.5;
  const sphereRotationX = time * 0.3;
  // Convert screen coordinates to normalized device coordinates
  const u = (pen.x / width) * 2 - 1;
  const v = ((height - pen.y) / height) * 2 - 1;

  // Create ray
  const rayDir = normalize(
    vec3((u * viewportWidth) / 2, (v * viewportHeight) / 2, -focalLength),
  );

  // Test intersection with main sphere
  const sphereT = intersectSphere(camera, rayDir, sphereCenter, sphereRadius);

  if (sphereT > 0) {
    // Hit the sphere! Calculate the 3D position
    const hitPoint = add(camera, scale(rayDir, sphereT));

    // Convert to sphere surface coordinates (relative to sphere center)
    const surfacePoint = sub(hitPoint, sphereCenter);

    // Apply INVERSE rotation to get the position in the sphere's local coordinate system
    // We need to undo the current rotation to store the dot in the "unrotated" space
    let localSurfacePoint = rotateX(surfacePoint, -sphereRotationX); // Undo X rotation first
    localSurfacePoint = rotateY(localSurfacePoint, -sphereRotationY); // Then undo Y rotation    // For continuous drawing, use smaller green dots
    let dotColor = vec3(1, 1, 0); // Default yellow for clicks
    let dotRadius = 0.08; // Standard size

    if (isContinuous) {
      dotColor = vec3(0, 1, 0.5); // Green for continuous drawing
      dotRadius = 0.06; // Smaller for continuous drawing
    }

    // Store the dot position in the sphere's local coordinate system
    sphereDots.push({
      position: localSurfacePoint,
      color: dotColor,
      radius: dotRadius,
    });

    // Limit number of dots to prevent performance issues
    if (sphereDots.length > 200) {
      // Increased limit for drawing
      sphereDots.shift(); // Remove oldest dot
    }
  }
}
