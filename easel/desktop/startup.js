// Apply the host palette before the document's first paint.
(() => {
 const theme=window.aesel?.initialTheme;
 if(!theme)return;
 for(const [name,value] of Object.entries({background:theme.background,foreground:theme.foreground,accent:theme.cursor})){
  document.documentElement.style.setProperty('--aesel-'+name,value);
 }
})();
