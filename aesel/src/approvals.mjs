// Typed adapters for Codex app-server requests. Never infer permission from a
// form's defaults, MCP tool descriptions, or an authentication URL.
const clean = value => String(value || '').replace(/[\u0000-\u001f\u007f-\u009f]/g, ' ').slice(0, 600);
const commandMethods = new Set(['item/commandExecution/requestApproval', 'item/fileChange/requestApproval']);
export function approvalFor(request) {
  const {id, method, params = {}} = request;
  if (commandMethods.has(method)) return {
    id, method, kind: 'command', subject: clean(params.command || params.reason || params.grantRoot || 'file change'),
    choicesText: 'y once  a session  n deny',
    responses: {y:{decision:'accept'}, a:{decision:'acceptForSession'}, n:{decision:'decline'}, '\u0003':{decision:'cancel'}},
  };
  if (method === 'item/tool/requestUserInput') {
    const questions = params.questions;
    // This adapter handles approval choices, not general questionnaires or secrets.
    if (Array.isArray(questions) && questions.length === 1 && !questions[0].isSecret) {
      const q = questions[0], options = q.options || [];
      const option = labels => options.find(o => labels.includes(o.label?.toLowerCase()));
      const once = option(['allow', 'accept', 'allow once', 'accept once']);
      const session = option(['allow for this session', 'accept for this session']);
      const deny = option(['decline', 'deny', 'reject']);
      const cancel = option(['cancel']);
      if (q.id && once && (deny || cancel)) {
        const response = o => ({answers:{[q.id]:{answers:[o.label]}}});
        return {id, method, kind:'tool', subject:clean(q.question),
          choicesText:`y once  ${session ? 'a session  ' : ''}n deny`,
          responses:{y:response(once), ...(session ? {a:response(session)} : {}), n:response(deny || cancel), '\u0003':cancel ? response(cancel) : {answers:{}}}};
      }
    }
    return {id, method, kind:'unsupported', subject:'Structured tool question is not supported by this approval drawer', choicesText:'n dismiss', responses:{n:{answers:{}}, '\u0003':{answers:{}}}};
  }
  if (method === 'mcpServer/elicitation/request') {
    const schema = params.requestedSchema;
    // Only a no-input confirmation can fit this yes/no drawer. In particular,
    // never fabricate values for required fields or accept URL/device proofs.
    const emptyForm = ['form','openai/form','openaiForm'].includes(params.mode) && schema?.type === 'object' &&
      schema.properties && typeof schema.properties === 'object' && !Array.isArray(schema.properties) &&
      Object.keys(schema.properties).length === 0 &&
      (!schema.required || (Array.isArray(schema.required) && schema.required.length === 0));
    const cancel = {action:'cancel',content:null}, decline = {action:'decline',content:null};
    return {id, method, kind:emptyForm ? 'tool' : 'unsupported',
      subject:emptyForm ? `${clean(params.serverName)}: ${clean(params.message)}` : `${clean(params.serverName)}: ${clean(params.mode)} input requires a supported form/authentication UI`,
      choicesText:emptyForm ? 'y once  n deny' : 'n dismiss',
      responses:{...(emptyForm ? {y:{action:'accept',content:{}}} : {}), n:emptyForm ? decline : cancel, '\u0003':cancel}};
  }
  return null;
}

export class ApprovalQueue {
  constructor(){this.pending=[];}
  enqueue(request, owner){
    const approval=approvalFor(request);
    if(!approval)return null;
    // An engine replacement cannot receive an old engine's request IDs.
    this.pending=this.pending.filter(item=>item.owner===owner);
    if(!this.pending.some(item=>item.id===approval.id))this.pending.push({...approval,owner});
    return this.current;
  }
  get current(){return this.pending[0] || null;}
  resolve(id, owner){this.pending=this.pending.filter(item=>item.owner===owner && item.id!==id);return this.current;}
  answer(key, owner){
    this.pending=this.pending.filter(item=>item.owner===owner);
    const approval=this.current, response=approval?.responses[key.toLowerCase()];
    if(!response)return null;
    this.pending.shift();return {approval,response,next:this.current};
  }
}

// Shape only: never log messages, defaults, options/enum values, tool arguments,
// URLs, challenges, or form values. Bounded identifiers still need escaping.
export function approvalShape(request) {
  const ident = value => typeof value === 'string' ? value.replace(/[^a-zA-Z0-9_./:-]/g, '?').slice(0, 80) : undefined;
  const keys = value => value && typeof value === 'object' && !Array.isArray(value)
    ? Object.keys(value).slice(0, 12).map(ident) : [];
  const p = request.params || {}, properties = p.requestedSchema?.properties;
  const types = new Set(['object','array','string','number','integer','boolean','null']);
  const shape = {method:ident(request.method),serverName:ident(p.serverName),mode:ident(p.mode),metadataKeys:keys(p._meta)};
  if (properties && typeof properties === 'object' && !Array.isArray(properties)) {
    shape.properties = Object.entries(properties).slice(0, 12).map(([name, schema]) => ({
      name:ident(name), type:types.has(schema?.type) ? schema.type : 'unspecified',
    }));
  }
  return shape;
}

// Default YOLO covers every provider/server's tool execution approval.
// Data questions and authentication still require actual input, not fabricated values.
export function defaultApprovalResponse(request) {
  const approval=approvalFor(request);
  return approval && ['command','tool'].includes(approval.kind) ? approval.responses.y : null;
}
