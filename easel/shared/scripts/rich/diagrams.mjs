import mermaid from 'mermaid';
mermaid.initialize({startOnLoad:false,securityLevel:'strict',suppressErrorRendering:true,maxTextSize:30000,theme:'neutral',htmlLabels:false,flowchart:{htmlLabels:false},fontFamily:'Helvetica, Arial, sans-serif'});
window.AeselDiagrams={render:(id,text)=>mermaid.render(id,text)};
