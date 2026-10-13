import test from 'node:test';
import assert from 'node:assert/strict';
import {starterPainting,validatePainting,paintingFilename,requestsNewPainting} from '../Resources/Web/painting.mjs';
test('bounded still painting data and PNG names',()=>{
 const painting=starterPainting();assert.deepEqual(validatePainting(painting),painting);
 painting.title='Crème / Clouds';assert.equal(paintingFilename(painting),'creme-clouds.png');
 assert.equal(requestsNewPainting('new empty painting'),true);assert.equal(requestsNewPainting('add a new painting on the wall'),false);
 for(const patch of [{width:99999},{height:0},{background:'url(https://example.com)'},{title:''},{layers:[{id:'x',kind:'script'}]},{layers:[{id:'x',kind:'path',color:'#123456',points:[[0,0],[1,1]],stroke:NaN}]}])assert.throws(()=>validatePainting({...painting,...patch}));
 const layer={id:'x',kind:'rect',color:'#112233',x:0,y:0,width:4,height:5};
 assert.throws(()=>validatePainting({...painting,layers:[layer,layer]}));
 const result=validatePainting({...painting,layers:[layer]});result.layers[0].width=99;assert.equal(layer.width,4,'validation isolates staged data');
});
