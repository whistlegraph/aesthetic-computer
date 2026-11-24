import { execSync } from 'child_process';
import fs from 'fs-extra';
import path from 'path';
import { fileURLToPath } from 'url';

// --- Configuration ---
// Read GCP Project ID from nanos config file
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const nanosGcpConfigPath = path.resolve(__dirname, '../nanos/config-gcp.json');
let GCLOUD_PROJECT;
try {
  const nanosConfig = fs.readJsonSync(nanosGcpConfigPath);
  GCLOUD_PROJECT = nanosConfig.CloudConfig.ProjectID;
  if (!GCLOUD_PROJECT) {
    throw new Error('ProjectID not found in nanos/config-gcp.json');
  }
} catch (error) {
  console.error(`Error reading GCLOUD_PROJECT from ${nanosGcpConfigPath}:`, error);
  process.exit(1);
}

// Region for Cloud Run deployment
const GCLOUD_REGION = 'us-west1'; 
// Name for the Cloud Run service
const SERVICE_NAME = 'remote-chromium'; 
// Docker image name (without the registry path)
const IMAGE_NAME = 'remote-chromium'; 
// Path to the Dockerfile directory
const DOCKERFILE_DIR = path.resolve(__dirname);
// GCP Artifact Registry repository name (replace if you have a specific one)
const ARTIFACT_REGISTRY_REPO = 'docker-repo'; // A common default, or choose your own

// Construct the full image URI for Artifact Registry
const IMAGE_URI = `${GCLOUD_REGION}-docker.pkg.dev/${GCLOUD_PROJECT}/${ARTIFACT_REGISTRY_REPO}/${IMAGE_NAME}:latest`;

// --- Helper Functions ---
function runCommand(command, errorMessage) {
  try {
    console.log(`Executing: ${command}`);
    const output = execSync(command, { stdio: 'inherit' });
    console.log(output ? output.toString() : '');
  } catch (error) {
    console.error(`Error ${errorMessage}:`, error.stderr ? error.stderr.toString() : error.message);
    process.exit(1);
  }
}

async function ensureGcloudAuthenticated() {
  try {
    execSync('gcloud auth print-access-token', { stdio: 'ignore' });
    console.log('gcloud is already authenticated.');
  } catch (error) {
    console.log('gcloud authentication required. Please log in.');
    runCommand('gcloud auth login', 'authenticating with gcloud');
    runCommand('gcloud auth application-default login', 'setting up application default credentials');
  }
}

async function configureDockerForGCR() {
    console.log('Configuring Docker to use gcloud as a credential helper...');
    runCommand(`gcloud auth configure-docker ${GCLOUD_REGION}-docker.pkg.dev`, 'configuring Docker for Artifact Registry');
}

// --- Main Orchestration Steps ---
async function main() {
  const mode = process.argv[2]; // Get the mode from command line arguments

  if (mode === 'dev') {
    console.log('Starting local development environment...');
    // 1. Build the Docker image for local development
    const localImageName = `${IMAGE_NAME}:local`;
    console.log(`Building Docker image: ${localImageName}`);
    runCommand(`docker build -t ${localImageName} ${DOCKERFILE_DIR}`, 'building Docker image for local dev');

    // 2. Run the Docker container locally
    console.log(`Running Docker container ${localImageName} locally on port 9222...`);
    // Ensure no other container is using port 9222 or use a different port
    runCommand(`docker run --rm -p 9222:9222 ${localImageName}`, 'running local Docker container');
    console.log(`Local Chromium instance should be accessible for remote debugging on ws://localhost:9222`);

  } else if (mode === 'deploy') {
    console.log('Starting Chromium deployment to GCP Cloud Run...');

    // 0. Ensure gcloud is authenticated and Docker is configured
    await ensureGcloudAuthenticated();
    await configureDockerForGCR();

    // 1. Build the Docker image
    console.log(`Building Docker image: ${IMAGE_URI}`);
    runCommand(`docker build -t ${IMAGE_URI} ${DOCKERFILE_DIR}`, 'building Docker image');

    // 2. Push the Docker image to GCP Artifact Registry
    //    Ensure the Artifact Registry repository exists. 
    //    You might need to create it first: 
    //    gcloud artifacts repositories create ${ARTIFACT_REGISTRY_REPO} --repository-format=docker --location=${GCLOUD_REGION} --description="Docker repository"
    console.log(`Pushing image to Artifact Registry: ${IMAGE_URI}`);
    runCommand(`docker push ${IMAGE_URI}`, 'pushing image to Artifact Registry');

    // 3. Deploy to Cloud Run
    console.log(`Deploying ${SERVICE_NAME} to Cloud Run in ${GCLOUD_REGION}...`);
    const deployCommand = `gcloud run deploy ${SERVICE_NAME} \
      --image ${IMAGE_URI} \
      --platform managed \
      --region ${GCLOUD_REGION} \
      --port 9222 \
      --allow-unauthenticated \
      --project ${GCLOUD_PROJECT}`;
      // Add --set-env-vars or other configurations as needed
      // For example, to ensure it uses a specific user data dir if needed by screenshot.mjs interaction:
      // --set-env-vars="USER_DATA_DIR=/tmp/chromium-user-data-cloudrun"

    runCommand(deployCommand, 'deploying to Cloud Run');

    console.log('Fetching service URL...');
    runCommand(`gcloud run services describe ${SERVICE_NAME} --platform managed --region ${GCLOUD_REGION} --format='value(status.url)' --project ${GCLOUD_PROJECT}`, 'fetching service URL');

    console.log(
      `\nDeployment complete! Your remote Chromium instance should be accessible at the URL printed above.`
    );
    console.log(
      `You can connect to it via WebSocket for screenshot.mjs using the wss:// version of the host (port will be 443 implicitly).`
    );
  } else {
    console.error('Invalid mode. Please use "dev" or "deploy".');
    console.log('Usage: node conductor.mjs [dev|deploy]');
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Deployment script failed:', error);
  process.exit(1);
});
