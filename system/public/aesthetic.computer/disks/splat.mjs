// Splat, 2026.3.15
// CPU-rendered 3D Gaussian splat with drag-to-rotate.

const { sin, cos, exp, sqrt, floor, min, max, PI } = Math;

// Splat definition: position, 3D covariance, color, opacity
const splat = {
  pos: [0, 0, 0],
  // Anisotropic covariance (stretched ellipsoid) via scale + rotation
  scale: [1.2, 0.6, 0.8],
  color: [255, 120, 40],
  opacity: 0.95,
};

// Camera state
let camTheta = 0.4; // azimuth
let camPhi = 0.3; // elevation
let camDist = 4.0;
let dragging = false;
let lastPen = null;
let autoRotate = true;

function boot() {}

function paint({ screen, wipe }) {
  const { width: W, height: H, pixels } = screen;

  if (autoRotate) camTheta += 0.008;

  // Camera position on sphere
  const cp = cos(camPhi),
    sp = sin(camPhi);
  const ct = cos(camTheta),
    st = sin(camTheta);
  const eye = [camDist * cp * st, camDist * sp, camDist * cp * ct];

  // View matrix (look-at origin, up = [0,1,0])
  const fwd = normalize(neg(eye));
  const right = normalize(cross(fwd, [0, 1, 0]));
  const up = cross(right, fwd);

  // Build 3x3 view rotation (rows = right, up, -fwd)
  const V = [
    [right[0], right[1], right[2]],
    [up[0], up[1], up[2]],
    [-fwd[0], -fwd[1], -fwd[2]],
  ];

  // Transform splat position to camera space
  const rel = sub(splat.pos, eye);
  const cam = mulMV(V, rel);

  // Splat is behind camera
  if (cam[2] <= 0.01) {
    wipe(20, 20, 30);
    return;
  }

  // Perspective projection params
  const focal = min(W, H) * 1.2;
  const cx = W / 2,
    cy = H / 2;

  // Project center
  const invZ = 1 / cam[2];
  const px = focal * cam[0] * invZ + cx;
  const py = -focal * cam[1] * invZ + cy; // flip Y

  // Build 3D covariance from scale (diagonal) rotated by a local rotation
  // Splat local rotation (tilt it for visual interest)
  const tilt = 0.5;
  const Rlocal = [
    [cos(tilt), 0, sin(tilt)],
    [0, 1, 0],
    [-sin(tilt), 0, cos(tilt)],
  ];
  const S = splat.scale;
  // Sigma = R * diag(s^2) * R^T
  const Sigma3D = computeCov3D(Rlocal, S);

  // Project 3D covariance to 2D: Sigma2D = J * V * Sigma3D * V^T * J^T
  // J is the Jacobian of the perspective projection
  const J = [
    [focal * invZ, 0, -focal * cam[0] * invZ * invZ],
    [0, -focal * invZ, focal * cam[1] * invZ * invZ],
  ];

  const VSigma = mulMM3(V, Sigma3D);
  const VSigmaVT = mulMM3T(VSigma, V);
  const JV = mul2x3_3x3(J, VSigmaVT);
  const Sigma2D = mul2x3_3x2T(JV, J);

  // Add small regularization
  Sigma2D[0][0] += 0.3;
  Sigma2D[1][1] += 0.3;

  // Invert 2x2 covariance
  const det = Sigma2D[0][0] * Sigma2D[1][1] - Sigma2D[0][1] * Sigma2D[1][0];
  if (det <= 0) {
    wipe(20, 20, 30);
    return;
  }
  const invDet = 1 / det;
  const invSigma = [
    [Sigma2D[1][1] * invDet, -Sigma2D[0][1] * invDet],
    [-Sigma2D[1][0] * invDet, Sigma2D[0][0] * invDet],
  ];

  // Bounding box: 3 sigma
  const r0 = 3 * sqrt(Sigma2D[0][0]);
  const r1 = 3 * sqrt(Sigma2D[1][1]);
  const x0 = max(0, floor(px - r0));
  const x1 = min(W - 1, floor(px + r0));
  const y0 = max(0, floor(py - r1));
  const y1 = min(H - 1, floor(py + r1));

  // Background color
  const bgR = 20, bgG = 20, bgB = 30;

  // Clear to background
  for (let i = 0; i < W * H * 4; i += 4) {
    pixels[i] = bgR;
    pixels[i + 1] = bgG;
    pixels[i + 2] = bgB;
    pixels[i + 3] = 255;
  }

  // Render the Gaussian splat
  const [sR, sG, sB] = splat.color;
  const alpha = splat.opacity;

  for (let y = y0; y <= y1; y++) {
    const dy = y - py;
    for (let x = x0; x <= x1; x++) {
      const dx = x - px;

      // Mahalanobis distance: d^T * Sigma^-1 * d
      const maha =
        dx * (invSigma[0][0] * dx + invSigma[0][1] * dy) +
        dy * (invSigma[1][0] * dx + invSigma[1][1] * dy);

      if (maha > 9) continue; // 3-sigma cutoff

      const g = exp(-0.5 * maha) * alpha;

      const idx = (y * W + x) * 4;
      // Alpha blend over background
      const invG = 1 - g;
      pixels[idx] = min(255, floor(sR * g + bgR * invG));
      pixels[idx + 1] = min(255, floor(sG * g + bgG * invG));
      pixels[idx + 2] = min(255, floor(sB * g + bgB * invG));
    }
  }

  // Draw crosshair at projected center
  for (let i = -3; i <= 3; i++) {
    setpx(pixels, W, H, floor(px) + i, floor(py), 255, 255, 255);
    setpx(pixels, W, H, floor(px), floor(py) + i, 255, 255, 255);
  }
}

function act({ event: e, pen }) {
  if (e.is("touch")) {
    dragging = true;
    autoRotate = false;
    lastPen = { x: pen.x, y: pen.y };
  }
  if (e.is("draw") && dragging && lastPen) {
    const dx = pen.x - lastPen.x;
    const dy = pen.y - lastPen.y;
    camTheta -= dx * 0.015;
    camPhi += dy * 0.015;
    camPhi = max(-PI / 2 + 0.01, min(PI / 2 - 0.01, camPhi));
    lastPen = { x: pen.x, y: pen.y };
  }
  if (e.is("lift")) {
    dragging = false;
    lastPen = null;
  }
  // Scroll / pinch to zoom
  if (e.is("scroll")) {
    camDist += e.delta * 0.01;
    camDist = max(1.5, min(15, camDist));
  }
  // Press 'r' to resume auto-rotation
  if (e.is("keyboard:down:r")) {
    autoRotate = !autoRotate;
  }
}

export { boot, paint, act };

// --- Math helpers ---

function neg(v) {
  return [-v[0], -v[1], -v[2]];
}

function sub(a, b) {
  return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
}

function cross(a, b) {
  return [
    a[1] * b[2] - a[2] * b[1],
    a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0],
  ];
}

function normalize(v) {
  const l = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  return l > 0 ? [v[0] / l, v[1] / l, v[2] / l] : [0, 0, 0];
}

// 3x3 matrix * 3-vector
function mulMV(M, v) {
  return [
    M[0][0] * v[0] + M[0][1] * v[1] + M[0][2] * v[2],
    M[1][0] * v[0] + M[1][1] * v[1] + M[1][2] * v[2],
    M[2][0] * v[0] + M[2][1] * v[1] + M[2][2] * v[2],
  ];
}

// Compute 3D covariance: R * diag(s^2) * R^T
function computeCov3D(R, s) {
  // RS = R * diag(s)
  const RS = [
    [R[0][0] * s[0], R[0][1] * s[1], R[0][2] * s[2]],
    [R[1][0] * s[0], R[1][1] * s[1], R[1][2] * s[2]],
    [R[2][0] * s[0], R[2][1] * s[1], R[2][2] * s[2]],
  ];
  // Sigma = RS * RS^T
  const C = [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 3; i++)
    for (let j = 0; j < 3; j++)
      for (let k = 0; k < 3; k++) C[i][j] += RS[i][k] * RS[j][k];
  return C;
}

// 3x3 * 3x3
function mulMM3(A, B) {
  const C = [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 3; i++)
    for (let j = 0; j < 3; j++)
      for (let k = 0; k < 3; k++) C[i][j] += A[i][k] * B[k][j];
  return C;
}

// 3x3 * 3x3^T
function mulMM3T(A, B) {
  const C = [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 3; i++)
    for (let j = 0; j < 3; j++)
      for (let k = 0; k < 3; k++) C[i][j] += A[i][k] * B[j][k];
  return C;
}

// 2x3 * 3x3
function mul2x3_3x3(A, B) {
  const C = [
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 2; i++)
    for (let j = 0; j < 3; j++)
      for (let k = 0; k < 3; k++) C[i][j] += A[i][k] * B[k][j];
  return C;
}

// 2x3 * (2x3)^T = 2x2
function mul2x3_3x2T(A, B) {
  const C = [
    [0, 0],
    [0, 0],
  ];
  for (let i = 0; i < 2; i++)
    for (let j = 0; j < 2; j++)
      for (let k = 0; k < 3; k++) C[i][j] += A[i][k] * B[j][k];
  return C;
}

// Set a single pixel with bounds check
function setpx(pixels, W, H, x, y, r, g, b) {
  if (x < 0 || x >= W || y < 0 || y >= H) return;
  const i = (y * W + x) * 4;
  pixels[i] = r;
  pixels[i + 1] = g;
  pixels[i + 2] = b;
  pixels[i + 3] = 255;
}
