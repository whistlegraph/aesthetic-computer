export function connectionFailure(error){
 const text=String(error?.message||error||'')+' '+String(error?.cause?.code||'');
 return /Failed to fetch|fetch failed|network(?: error| request failed)|networkerror|internet disconnected|ERR_(?:INTERNET_DISCONNECTED|NETWORK_CHANGED|CONNECTION_|NAME_NOT_RESOLVED)|ECONNRESET|ECONNREFUSED|ENOTFOUND|EAI_AGAIN|ETIMEDOUT|socket hang up|connection (?:error|closed|lost)|HTTP (?:502|503|504)/i.test(text);
}
export function conciseFailure(error){
 const text=String(error?.message||error||'');
 const syntax=/(?:SyntaxError|TypeError|ReferenceError):[^\n]+/.exec(text)?.[0];
 return (syntax||text.split('\n')[0]).slice(0,220);
}
