// patch.mjs - Admin-only endpoint for spawning GitHub Copilot coding agents
// 2026.02.01
//
// POST /api/patch - Spawn a GitHub Copilot coding agent with the given prompt
// Requires authentication and admin privileges (@jeffrey only)
//
// Request body:
// {
//   prompt: string,          // The task/instruction for the PR agent
//   branch?: string,         // Optional: target branch (defaults to main)
//   title?: string           // Optional: PR title prefix
// }
//
// Response:
// {
//   success: boolean,
//   message: string,
//   data?: {
//     jobId: string,         // ID for tracking the agent job
//     status: string,        // "queued" | "running" | "completed" | "failed"
//   }
// }

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";

const dev = process.env.CONTEXT === "dev";

// GitHub Copilot Coding Agent configuration
const GITHUB_TOKEN = process.env.GITHUB_COPILOT_TOKEN;
const REPO_OWNER = "whistlegraph";
const REPO_NAME = "aesthetic-computer";

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {}, {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type, Authorization",
    });
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { success: false, message: "Method not allowed" });
  }

  try {
    // 1. Authenticate the user
    const user = await authorize(event.headers);
    
    if (!user) {
      shell.log("🔐 Patch: No user found");
      return respond(401, { success: false, message: "Authentication required" });
    }

    if (!user.email_verified) {
      shell.log("🔐 Patch: Email not verified");
      return respond(401, { success: false, message: "Email verification required" });
    }

    // 2. Check admin privileges (only @jeffrey can use this)
    const isAdmin = await hasAdmin(user);
    const handle = await handleFor(user.sub);
    
    shell.log(`🔐 Patch: User @${handle}, admin: ${isAdmin}`);

    if (!isAdmin) {
      shell.log(`🚫 Patch: Unauthorized access attempt by @${handle}`);
      return respond(403, { 
        success: false, 
        message: "Admin privileges required. Only @jeffrey can use this command." 
      });
    }

    // 3. Parse the request body
    let body;
    try {
      body = JSON.parse(event.body);
    } catch (e) {
      return respond(400, { success: false, message: "Invalid JSON body" });
    }

    const { prompt, branch = "main", title } = body;

    if (!prompt || typeof prompt !== "string" || prompt.trim().length === 0) {
      return respond(400, { 
        success: false, 
        message: "A prompt/instruction is required for the PR agent" 
      });
    }

    // 4. Check for GitHub token
    if (!GITHUB_TOKEN) {
      shell.error("🔴 Patch: GITHUB_COPILOT_TOKEN not configured");
      return respond(503, { 
        success: false, 
        message: "GitHub Copilot integration not configured. Please set GITHUB_COPILOT_TOKEN." 
      });
    }

    // 5. Spawn the GitHub Copilot PR agent
    shell.log(`🤖 Patch: Spawning PR agent for @${handle}`);
    shell.log(`📝 Prompt: ${prompt.substring(0, 100)}...`);

    const agentResult = await spawnCopilotAgent({
      prompt: prompt.trim(),
      branch,
      title: title || `[patch] ${prompt.substring(0, 50)}...`,
      handle,
    });

    if (agentResult.success) {
      shell.log(`✅ Patch: Agent spawned successfully, job: ${agentResult.jobId}`);
      return respond(200, {
        success: true,
        message: "PR agent spawned successfully",
        data: {
          jobId: agentResult.jobId,
          status: agentResult.status,
          url: agentResult.url,
        },
      });
    } else {
      shell.error(`🔴 Patch: Agent spawn failed: ${agentResult.error}`);
      return respond(500, {
        success: false,
        message: agentResult.error || "Failed to spawn PR agent",
      });
    }

  } catch (error) {
    shell.error(`🔴 Patch error: ${error.message}`);
    return respond(500, { 
      success: false, 
      message: dev ? error.message : "Internal server error" 
    });
  }
}

/**
 * Spawn a GitHub Copilot coding agent to create a PR
 * Uses the GitHub API to trigger Copilot workspace/coding agent
 */
async function spawnCopilotAgent({ prompt, branch, title, handle }) {
  try {
    const { Octokit } = await import("@octokit/rest");
    
    const octokit = new Octokit({
      auth: GITHUB_TOKEN,
    });

    // Create an issue that Copilot can work on
    // GitHub Copilot Coding Agent can be triggered by creating issues
    // with specific labels or via the GitHub API
    
    const issueBody = `## Task Description

${prompt}

---
*Triggered by @${handle} via aesthetic.computer/patch command*
*Target branch: ${branch}*

## Instructions for Copilot Agent

Please implement this change and create a pull request. Follow existing code patterns and conventions in the repository.

---
🤖 **This issue was auto-generated by the AC patch system**
`;

    // First, try to create the issue
    const issueResponse = await octokit.issues.create({
      owner: REPO_OWNER,
      repo: REPO_NAME,
      title: title,
      body: issueBody,
      labels: ["copilot", "auto-patch"],
    });

    const issueNumber = issueResponse.data.number;
    const issueUrl = issueResponse.data.html_url;

    shell.log(`📋 Created issue #${issueNumber}: ${issueUrl}`);

    // Trigger Copilot coding agent on this issue
    // This uses the repository dispatch event which Copilot can respond to
    try {
      await octokit.repos.createDispatchEvent({
        owner: REPO_OWNER,
        repo: REPO_NAME,
        event_type: "copilot-patch",
        client_payload: {
          issue_number: issueNumber,
          prompt: prompt,
          branch: branch,
          triggered_by: handle,
        },
      });
      shell.log(`🚀 Dispatched copilot-patch event for issue #${issueNumber}`);
    } catch (dispatchError) {
      // Dispatch might fail if not set up, but issue creation is still useful
      shell.log(`⚠️ Could not dispatch event (may need workflow setup): ${dispatchError.message}`);
    }

    return {
      success: true,
      jobId: `issue-${issueNumber}`,
      status: "queued",
      url: issueUrl,
    };

  } catch (error) {
    shell.error(`🔴 spawnCopilotAgent error: ${error.message}`);
    return {
      success: false,
      error: error.message,
    };
  }
}
