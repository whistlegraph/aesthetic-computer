// Revision-checked, atomic edits. No fuzzy matching or partial application.
export const EDIT_PIECE = {
 name:'edit_piece',
 description:'Layer a small change onto the current piece. Prefer this over rewriting the file. Send the current revision and exact unique search/replacement pairs. All edits form one atomic checkpoint, applied in order. Preserve unrelated code. Use several calls for independently runnable layers.',
 input_schema:{type:'object',properties:{revision:{type:'string',description:'Current opaque revision token from the piece context or last edit result.'},edits:{type:'array',minItems:1,maxItems:24,items:{type:'object',properties:{search:{type:'string',description:'Exact nonempty unique text in the current source.'},replace:{type:'string'}},required:['search','replace'],additionalProperties:false}},note:{type:'string'}},required:['revision','edits'],additionalProperties:false}
};
export function applyPieceEdits(source,edits){
 if(!Array.isArray(edits)||!edits.length||edits.length>24)throw Error('Provide 1–24 exact edits.');
 let next=source;
 for(const edit of edits){
  if(typeof edit?.search!=='string'||!edit.search||typeof edit.replace!=='string')throw Error('Each edit needs nonempty search and string replace.');
  const at=next.indexOf(edit.search);
  if(at<0)throw Error('Search text not found; inspect the current source before retrying.');
  if(next.indexOf(edit.search,at+1)>=0)throw Error('Search text is ambiguous; include more surrounding code.');
  next=next.slice(0,at)+edit.replace+next.slice(at+edit.search.length);
  if(next.length>500000)throw Error('Piece exceeds the size limit.');
 }
 return next;
}
