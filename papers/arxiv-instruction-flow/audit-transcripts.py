import json,pathlib,collections,re,hashlib,datetime,shlex
HOME=pathlib.Path.home(); ROOT='/Users/jas/aesthetic-computer'; START='2026-09-01'; END='2026-09-15'
files={'claude':sorted((HOME/'.claude/projects/-Users-jas-aesthetic-computer').glob('*.jsonl')),'codex':[p for d in range(1,15) for p in sorted((HOME/f'.codex/sessions/2026/09/{d:02}').glob('*.jsonl'))]}
seen_calls=set(); seen_prompts=set(); counts=collections.Counter(); promptterms=collections.Counter(); tools=collections.Counter(); nested=collections.Counter(); shells=collections.Counter(); tool_sessions=collections.defaultdict(set); nested_sessions=collections.defaultdict(set); shell_sessions=collections.defaultdict(set); term_sessions=collections.defaultdict(set); active=collections.defaultdict(set)
terms={'compush':r'\bcompush(?:ed|ing|es)?\b','compushloy':r'\bcompushloy(?:ed|ing|s)?\b','oskieploy':r'\boskieploy(?:ed|ing|s)?\b','prox':r'\bprox(?:es)?\b','easel':r'\beasel\b','frame_screenshot':r'\bframe\s+(?:screenshot|capture)|\bscreenshot.{0,20}\bframe\b','papers_platter':r'(?:/papers\b|\bpapers?\s+stack\b|\bplatter\b)','sticky':r'\bstick(?:y|ies)\b','cleaner':r'\bcleaner\b','wordcrust':r'\bwordcrust\b','slidecop':r'\bslidecop\b','unipointer':r'\bunipointer\b','loopboy':r'\bloopboy\b','chrome':r'\bchrome\b','cdp':r'\bcdp\b'}
allowed={'rg','grep','sed','head','git','node','python','python3','fish'}
def shell(cmd,sess):
 if not isinstance(cmd,str) or '\n' in cmd:return
 # Only standalone simple lines; reject metacharacters rather than interpret shell.
 for line in cmd.splitlines():
  if not line.strip() or line.lstrip().startswith('#') or re.search(r'[;|&<>`()]|\$\(',line):continue
  try:tok=shlex.split(line)
  except ValueError:continue
  while tok and re.match(r'^[A-Za-z_][A-Za-z_0-9]*=',tok[0]):tok.pop(0)
  if tok and pathlib.PurePosixPath(tok[0]).name in allowed:
   name=pathlib.PurePosixPath(tok[0]).name;shells[name]+=1;shell_sessions[name].add(sess)
def call(provider,name,args,cid,sess):
 key=(provider,cid)
 if not cid:counts['calls_missing_id_excluded']+=1;return
 if key in seen_calls:counts['duplicate_call_ids_excluded']+=1;return
 seen_calls.add(key);tools[name]+=1;tool_sessions[name].add(sess);counts[provider+'_calls']+=1;active[provider].add(sess)
 if name in ('Bash','exec_command','functions.exec_command'):
  a=args
  if isinstance(a,str):
   try:a=json.loads(a)
   except: a={}
  if isinstance(a,dict):shell(a.get('command',a.get('cmd')),sess)
 if name in ('functions.exec','exec') and isinstance(args,str):
  # Syntactic call-site candidates only, not execution proof. Comments/strings not parsed.
  for nm in re.findall(r'\btools\.([A-Za-z_$][\w$]*)\s*\(',args):
   if nm in ('find','filter','map','some','forEach'):counts['exec_non_tool_method_candidates_excluded']+=1;continue
   nested[nm]+=1;nested_sessions[nm].add(sess)
def prompt(provider,txt,pid,ts,sess):
 if not isinstance(txt,str):return
 if not txt.strip() or len(txt)>400:counts['prompts_empty_or_long_excluded']+=1;return
 if re.search(r'<(?:task-notification|system-reminder|environment_context|INSTRUCTIONS)|AGENTS\.md instructions|^\s*(?:```|#{1,6}\s|>\s)|Message Type:|^\s*\[.*?(?:agent|system)',txt,re.I):counts['prompts_marked_or_forwarded_excluded']+=1;return
 # Codex often lacks ids; timestamp+content deduplicates exact history copies.
 key=(provider,pid or hashlib.sha256((ts+'\0'+txt).encode()).hexdigest())
 if key in seen_prompts:counts['duplicate_prompts_excluded']+=1;return
 seen_prompts.add(key);counts[provider+'_accepted_prompts']+=1;active[provider].add(sess)
 for term,pat in terms.items():
  if re.search(pat,txt,re.I):promptterms[term]+=1;term_sessions[term].add(sess)
for provider,paths in files.items():
 counts[provider+'_files_scanned']=len(paths);counts[provider+'_bytes_scanned']=sum(p.stat().st_size for p in paths)
 for p in paths:
  sess=provider+':'+p.stem;cwd=None
  with p.open() as f:
   for line in f:
    counts['jsonl_lines']+=1
    try:o=json.loads(line)
    except:counts['malformed_lines']+=1;continue
    if provider=='claude' and o.get('cwd'):cwd=o['cwd']
    if provider=='codex' and o.get('type') in ('session_meta','turn_context'):
     cwd=o.get('payload',{}).get('cwd',cwd)
    ts=o.get('timestamp','')
    if not isinstance(ts,str):continue
    try: day=datetime.datetime.fromisoformat(ts.replace('Z','+00:00')).astimezone(datetime.timezone.utc).date().isoformat()
    except (ValueError,TypeError):continue
    if not START<=day<END:continue
    counts['in_window_records']+=1
    thiscwd=o.get('cwd',cwd)
    if thiscwd!=ROOT:counts['nonexact_or_missing_cwd_records_excluded']+=1;continue
    counts[provider+'_in_scope_records']+=1
    if provider=='claude':
     msg=o.get('message',{});content=msg.get('content',[])
     if o.get('type')=='attachment' and o.get('attachment',{}).get('type')=='queued_command':
      a=o['attachment']
      if a.get('commandMode')=='prompt' and a.get('origin',{}).get('kind')=='human':
       counts['claude_eligible_human_queued_prompts']+=1
       prompt(provider,a.get('prompt'),a.get('source_uuid') or o.get('uuid'),ts,sess)
      else:counts['claude_nonhuman_queued_attachments_excluded']+=1
     elif o.get('type')=='assistant' and isinstance(content,list):
      for b in content:
       if isinstance(b,dict) and b.get('type')=='tool_use':call(provider,b.get('name','?'),b.get('input'),b.get('id'),sess)
     elif o.get('type')=='user':
      if isinstance(content,list) and any(isinstance(b,dict) and b.get('type')=='tool_result' for b in content):counts['claude_tool_result_user_records_excluded']+=1;continue
      txt=content if isinstance(content,str) else '\n'.join(b.get('text','') for b in content if isinstance(b,dict) and b.get('type')=='text')
      prompt(provider,txt,o.get('uuid'),ts,sess)
    elif o.get('type')=='response_item':
     q=o.get('payload',{});typ=q.get('type')
     if typ in ('function_call','custom_tool_call'):call(provider,q.get('name','?'),q.get('arguments',q.get('input')),q.get('call_id',q.get('id')),sess)
     elif typ=='message' and q.get('role')=='user':
      txt='\n'.join(b.get('text','') for b in q.get('content',[]) if b.get('type') in ('input_text','text'))
      prompt(provider,txt,q.get('id'),ts,sess)
def family(name):
 if name.startswith('mcp__'):
  return 'mcp:'+name.split('__')[1]
 return name
def families(counter,ss):
 c=collections.Counter(); sets=collections.defaultdict(set)
 for k,v in counter.items():c[family(k)]+=v;sets[family(k)].update(ss[k])
 return rows(c,sets)
def rows(counter,ss):return [{'name':k,'count':v,'sessions':len(ss[k])} for k,v in counter.most_common()]
result={'window_utc':[START,END],'scope':'exact repository cwd; no nested cwd; no private text retained','counts':dict(counts),'active_sessions':{k:len(v) for k,v in active.items()},'native_tool_families':families(tools,tool_sessions),'exec_candidate_families':families(nested,nested_sessions),'native_tool_calls':rows(tools,tool_sessions),'exec_syntactic_dispatch_candidates':rows(nested,nested_sessions),'simple_direct_shell_lines':rows(shells,shell_sessions),'short_user_prompt_mentions':rows(promptterms,term_sessions),'term_patterns':terms,'limitations':['Native call records prove invocation requests, not successful execution.','Exec regex finds syntactic tools.NAME( candidates; loops/dynamic names/comments/strings are not interpreted.','Shell counts use single-line direct Bash/exec_command records only; wrappers and all multiline/compound commands omitted.','Exact cwd intentionally excludes all child directories and worktrees.','Prompt filtering heuristic; timestamps+content for idless Codex copies; no event_msg prompts counted.','Claude top-level project logs only; subagent transcript subdirectories excluded.','Revision2 adds structured Claude queued_command attachments only when commandMode=prompt and origin.kind=human, deduped by source_uuid; prior user-message-only count was underinclusive.','Cwd inherited from preceding explicit cwd within each native session when an event omits cwd; explicit cwd always wins.']}
path='/tmp/ac-prox-tool-audit.json';pathlib.Path(path).write_text(json.dumps(result,indent=2));print(json.dumps({k:v for k,v in result.items() if k not in ('native_tool_calls','exec_syntactic_dispatch_candidates','term_patterns')},indent=2))
