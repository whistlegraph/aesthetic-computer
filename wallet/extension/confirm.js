// Keeps Wallet - Beacon Confirmation Popup
// Reads pending request from background, shows details, sends approval/rejection

(async function () {
  'use strict';

  const contentEl = document.getElementById('content');

  // Get requestId from URL params
  const params = new URLSearchParams(window.location.search);
  const requestId = params.get('requestId');

  if (!requestId) {
    contentEl.innerHTML = '<div class="error">No request ID provided.</div>';
    return;
  }

  // Fetch pending request details from background
  let pending;
  try {
    pending = await chrome.runtime.sendMessage({
      type: 'BEACON_GET_PENDING',
      payload: { requestId },
    });
  } catch (err) {
    contentEl.innerHTML =
      '<div class="error">Failed to load request details.</div>';
    return;
  }

  if (pending.error) {
    contentEl.innerHTML = `<div class="error">${escapeHtml(pending.error)}</div>`;
    return;
  }

  // Render based on request type
  const { requestType, peerName, peerIcon, request, walletAddress } = pending;

  let detailsHtml = '';

  switch (requestType) {
    case 'permission_request':
      detailsHtml = renderPermissionRequest(request, walletAddress);
      break;
    case 'operation_request':
      detailsHtml = renderOperationRequest(request, walletAddress);
      break;
    case 'sign_payload_request':
      detailsHtml = renderSignPayloadRequest(request, walletAddress);
      break;
    default:
      detailsHtml = `<div class="detail-row">
        <div class="detail-label">Type</div>
        <div class="detail-value">${escapeHtml(requestType)}</div>
      </div>`;
  }

  const requestLabel = formatRequestType(requestType);

  contentEl.innerHTML = `
    <div class="card">
      <div class="dapp-name">${escapeHtml(peerName)}</div>
      <div class="request-type">${escapeHtml(requestLabel)}</div>
      ${detailsHtml}
    </div>
    <div class="buttons">
      <button class="btn btn-reject" id="btn-reject">Reject</button>
      <button class="btn btn-approve" id="btn-approve">Approve</button>
    </div>
  `;

  // Button handlers
  document.getElementById('btn-approve').addEventListener('click', async () => {
    await sendConfirmation(true);
  });

  document.getElementById('btn-reject').addEventListener('click', async () => {
    await sendConfirmation(false);
  });

  // Close window on Escape
  document.addEventListener('keydown', async (e) => {
    if (e.key === 'Escape') {
      await sendConfirmation(false);
    }
  });

  async function sendConfirmation(approved) {
    // Disable buttons to prevent double-click
    const approveBtn = document.getElementById('btn-approve');
    const rejectBtn = document.getElementById('btn-reject');
    if (approveBtn) approveBtn.disabled = true;
    if (rejectBtn) rejectBtn.disabled = true;

    try {
      await chrome.runtime.sendMessage({
        type: 'BEACON_CONFIRM',
        payload: { requestId, approved },
      });
    } catch (err) {
      console.error('Failed to send confirmation:', err);
    }

    // Close the popup window
    window.close();
  }

  // --- Render helpers ---

  function renderPermissionRequest(req, address) {
    const network = req.network?.type || 'unknown';
    const scopes = req.scopes
      ? req.scopes.join(', ')
      : 'sign, operation_request';

    return `
      <div class="detail-row">
        <div class="detail-label">Wants to connect</div>
        <div class="detail-value">This dApp is requesting permission to see your address and request signatures.</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Your Address</div>
        <div class="detail-value">${escapeHtml(address || 'Unknown')}</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Network</div>
        <div class="detail-value">${escapeHtml(network)}</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Scopes</div>
        <div class="detail-value">${escapeHtml(scopes)}</div>
      </div>
    `;
  }

  function renderOperationRequest(req, address) {
    const ops = req.operationDetails || [];
    let opsHtml = '';

    for (const op of ops) {
      let detail = '';
      if (op.kind === 'transaction') {
        const amount = op.amount
          ? (parseInt(op.amount) / 1000000).toFixed(6) + ' tez'
          : '0 tez';
        detail = `To: ${escapeHtml(op.destination || '?')} | ${amount}`;
        if (op.parameters) {
          detail += ` | Entrypoint: ${escapeHtml(op.parameters.entrypoint || '?')}`;
        }
      } else if (op.kind === 'delegation') {
        detail = op.delegate
          ? `Delegate to: ${escapeHtml(op.delegate)}`
          : 'Remove delegation';
      } else if (op.kind === 'origination') {
        detail = 'Deploy new contract';
      }

      opsHtml += `
        <div class="op-item">
          <div class="op-kind">${escapeHtml(op.kind)}</div>
          ${detail ? `<div class="op-detail">${detail}</div>` : ''}
        </div>
      `;
    }

    return `
      <div class="detail-row">
        <div class="detail-label">From</div>
        <div class="detail-value">${escapeHtml(address || 'Unknown')}</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Operations (${ops.length})</div>
        <div class="operations-list">${opsHtml || '<div class="op-item">No operations</div>'}</div>
      </div>
    `;
  }

  function renderSignPayloadRequest(req, address) {
    const payload = req.payload || '';
    // Truncate long payloads for display
    const displayPayload =
      payload.length > 200 ? payload.substring(0, 200) + '...' : payload;
    const signingType = req.signingType || 'raw';

    return `
      <div class="detail-row">
        <div class="detail-label">Signing Type</div>
        <div class="detail-value">${escapeHtml(signingType)}</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Payload</div>
        <div class="detail-value">${escapeHtml(displayPayload)}</div>
      </div>
      <div class="detail-row">
        <div class="detail-label">Signing As</div>
        <div class="detail-value">${escapeHtml(address || 'Unknown')}</div>
      </div>
    `;
  }

  function formatRequestType(type) {
    switch (type) {
      case 'permission_request':
        return 'Connect';
      case 'operation_request':
        return 'Sign Operation';
      case 'sign_payload_request':
        return 'Sign Payload';
      default:
        return type;
    }
  }

  function escapeHtml(str) {
    if (!str) return '';
    return String(str)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }
})();
