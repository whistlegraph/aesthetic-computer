import {approvalFor} from '../src/approvals.mjs';

// MCP confirmation and shell approval responses use different wire protocols.
// A real form or authentication challenge cannot be answered by a permission button.
export function nativeApproval(request) {
  const approval = approvalFor(request);
  if (!approval) return null;
  const mcp = request.method === 'mcpServer/elicitation/request';
  const canAccept = !!approval.responses.y;
  return {...approval, mcp,
    title: canAccept ? 'Allow this action?' : 'Input needed',
    detail: approval.subject,
    canAccept,
    alwaysLabel: canAccept && mcp ? 'Always allow' : approval.responses.a ? 'Allow for session' : null,
    alwaysScope: canAccept && mcp ? 'Applies to tool calls from all connected MCP servers.' : null,
  };
}

export function nativeApprovalResponse(approval, decision) {
  if (decision === 'always') return approval.mcp ? approval.responses.y : approval.responses.a;
  return approval.responses[{accept:'y', decline:'n', cancel:'\u0003'}[decision]];
}
