export function connectionFailure(error){
 const text=[error?.message||error||'',error?.code,error?.cause?.code,JSON.stringify(error?.codexErrorInfo||'')].join(' ');
 const status=error?.status ?? error?.statusCode;
 if([400,401,403,404].includes(status)||/insufficient.quota|usage limit|billing|invalid.api.key|unauthorized|authentication/i.test(text))return false;
 return error?.name==='TimeoutError'||[408,429,500,502,503,504].includes(status)||/Load failed|Failed to fetch|fetch failed|network(?: error| request failed)|networkerror|internet disconnected|ERR_(?:INTERNET_DISCONNECTED|NETWORK_CHANGED|CONNECTION_|NAME_NOT_RESOLVED)|ECONNRESET|ECONNREFUSED|ENOTFOUND|EAI_AGAIN|ETIMEDOUT|UND_ERR_(?:CONNECT_TIMEOUT|HEADERS_TIMEOUT|BODY_TIMEOUT|SOCKET)|socket hang up|connection (?:error|closed|lost|timed out)|stream (?:disconnected|closed|interrupted)|responseStream(?:Disconnected|ConnectionFailed)|httpConnectionFailed|(?:request|stream|operation|turn\/start|thread\/resume|initialize) timed out|HTTP (?:408|429|500|502|503|504)/i.test(text);
}
export function conciseFailure(error){
 const text=String(error?.message||error||'');
 const syntax=/(?:SyntaxError|TypeError|ReferenceError):[^\n]+/.exec(text)?.[0];
 return (syntax||text.split('\n')[0]).slice(0,220);
}
