// Included inside QuickJsEngine's anonymous namespace, after CallScope.
// Input ownership stays with QuickJS for the full native call.
struct SceneFloats {
  JSContext* ctx; JSValue buffer=JS_UNDEFINED; const float* data=nullptr; size_t count=0;
  explicit SceneFloats(JSContext* c):ctx(c){}
  ~SceneFloats(){JS_FreeValue(ctx,buffer);}
  bool read(JSValueConst value){
    if(JS_GetTypedArrayType(value)!=JS_TYPED_ARRAY_FLOAT32)return false;
    size_t off=0,len=0,stride=0,total=0;
    buffer=JS_GetTypedArrayBuffer(ctx,value,&off,&len,&stride);
    if(JS_IsException(buffer))return false;
    auto* bytes=JS_GetArrayBuffer(ctx,&total,buffer);
    if(!bytes||stride!=4||off>total||len>total-off)return false;
    data=reinterpret_cast<const float*>(bytes+off);count=len/4;return true;
  }
};
struct ScenePoint{double x,y,z,u=0,v=0,q=1;};
static ScenePoint SceneMix(ScenePoint a,ScenePoint b,double t){return {a.x+(b.x-a.x)*t,a.y+(b.y-a.y)*t,a.z+(b.z-a.z)*t,a.u+(b.u-a.u)*t,a.v+(b.v-a.v)*t,a.q+(b.q-a.q)*t};}
template<class Distance> static std::vector<ScenePoint> SceneClip(const std::vector<ScenePoint>& in,Distance dist){
  std::vector<ScenePoint> out;out.reserve(in.size()+2);
  for(size_t i=0;i<in.size();i++){auto a=in[i],b=in[(i+1)%in.size()];double da=dist(a),db=dist(b);if(da>=0)out.push_back(a);if((da>=0)!=(db>=0))out.push_back(SceneMix(a,b,da/(da-db)));}
  return out;
}
static void SceneTriangle(Graphics& g,ScenePoint a,ScenePoint b,ScenePoint c,Color ink){
  g.triangle({float(a.x),float(a.y),float(a.z),float(b.x),float(b.y),float(b.z),float(c.x),float(c.y),float(c.z),ink});
}
static Color SceneColor(double r,double g,double b){return {uint8_t(std::clamp(r,0.,255.)),uint8_t(std::clamp(g,0.,255.)),uint8_t(std::clamp(b,0.,255.)),255};}
JSValue SceneDisc(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));double v[7]{};
 if(!scope||!scope->api||argc<7)return JS_ThrowTypeError(ctx,"disc3d requires x,y,z,radius,r,g,b");
 for(int i=0;i<7;i++)if(JS_ToFloat64(ctx,&v[i],args[i])||!std::isfinite(v[i]))return JS_ThrowTypeError(ctx,"non-finite disc");
 if(v[3]<0||v[3]>8192||std::abs(v[0])+v[3]>32200||std::abs(v[1])+v[3]>32200)return JS_UNDEFINED;
 int n=v[3]<6?8:v[3]<13?12:v[3]<26?16:v[3]<52?20:28;auto color=SceneColor(v[4],v[5],v[6]);
 ScenePoint center{v[0],v[1],v[2]},last{v[0]+v[3],v[1],v[2]};
 for(int i=1;i<=n;i++){double a=i*6.283185307179586/n;ScenePoint next{v[0]+std::cos(a)*v[3],v[1]+std::sin(a)*v[3],v[2]};SceneTriangle(scope->api->graphics,center,last,next,color);last=next;}
 return JS_UNDEFINED;
}
// The fan both capsule3d and a retained mesh's edge capsules emit.
static void SceneCapsuleFan(Graphics& g,double x1,double y1,double x2,double y2,double z,double width,Color color){
 const double radius=width*.5,dx=x2-x1,dy=y2-y1,length=std::hypot(dx,dy);
 if(radius<0||radius>8192)return;
 for(double v:{x1,y1,x2,y2})if(std::abs(v)+radius>32200)return;
 const double ux=length>1e-6?dx/length:1,uy=length>1e-6?dy/length:0,nx=-uy*radius,ny=ux*radius;
 ScenePoint a{x1+nx,y1+ny,z},b{x1-nx,y1-ny,z},c{x2+nx,y2+ny,z},d{x2-nx,y2-ny,z};SceneTriangle(g,a,b,c,color);SceneTriangle(g,b,d,c,color);
 const int n=radius<6?3:radius<13?4:radius<26?6:radius<52?8:12;
 for(int end:{-1,1}){const double x=end<0?x1:x2,y=end<0?y1:y2;ScenePoint center{x,y,z},last{x+nx,y+ny,z};
  for(int i=1;i<=n;i++){const double angle=i*3.141592653589793/n,side=std::cos(angle),along=std::sin(angle)*end*radius;ScenePoint next{x+nx*side+ux*along,y+ny*side+uy*along,z};SceneTriangle(g,center,last,next,color);last=next;}}
}
JSValue SceneCapsule(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));double v[9]{};
 if(!scope||!scope->api||argc<9)return JS_ThrowTypeError(ctx,"capsule3d requires endpoints,z,width,r,g,b");
 for(int i=0;i<9;i++)if(JS_ToFloat64(ctx,&v[i],args[i])||!std::isfinite(v[i]))return JS_ThrowTypeError(ctx,"non-finite capsule");
 SceneCapsuleFan(scope->api->graphics,v[0],v[1],v[2],v[3],v[4],v[5],SceneColor(v[6],v[7],v[8]));
 return JS_UNDEFINED;
}
JSValue SceneMesh(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));SceneFloats verts(ctx),faces(ctx),camera(ctx);
 if(!scope||!scope->api||argc<3||!verts.read(args[0])||!faces.read(args[1])||!camera.read(args[2]))return JS_ThrowTypeError(ctx,"sceneMesh requires three Float32Arrays");
 if(verts.count%3||faces.count%10||camera.count!=27||verts.count>300000||faces.count>100000)return JS_ThrowRangeError(ctx,"sceneMesh layout or budget");
 const auto* m=camera.data;
 for(size_t i=0;i<27;i++)if(!std::isfinite(m[i]))return JS_ThrowTypeError(ctx,"non-finite camera");
 if(m[17]<=0||m[18]<-32200||m[19]<-32200||m[20]>32200||m[21]>32200||m[18]>=m[20]||m[19]>=m[21])return JS_ThrowRangeError(ctx,"invalid viewport");
 std::vector<ScenePoint> points;points.reserve(verts.count/3);
 for(size_t i=0;i<verts.count;i+=3){double x=verts.data[i]-m[0],y=verts.data[i+1]-m[1],z=verts.data[i+2]-m[2];if(!std::isfinite(x)||!std::isfinite(y)||!std::isfinite(z))return JS_ThrowTypeError(ctx,"non-finite vertex");points.push_back({x*m[3]+y*m[4]+z*m[5],x*m[6]+y*m[7]+z*m[8],x*m[9]+y*m[10]+z*m[11]});}
 // Validate the complete index stream before submitting any geometry.
 for(size_t i=0;i<faces.count;i+=10){for(int j=0;j<10;j++)if(!std::isfinite(faces.data[i+j]))return JS_ThrowTypeError(ctx,"non-finite face");for(int j=0;j<4;j++){double id=faces.data[i+j];if(id<0||id>=points.size()||std::floor(id)!=id)return JS_ThrowRangeError(ctx,"invalid mesh index");}}
 int emitted=0;
 for(size_t i=0;i<faces.count;i+=10){const auto* f=faces.data+i;std::vector<ScenePoint> poly{points[size_t(f[0])],points[size_t(f[1])],points[size_t(f[2])],points[size_t(f[3])]};
  poly=SceneClip(poly,[&](ScenePoint p){return p.z-m[17];});if(poly.size()<3)continue;
  for(auto& p:poly){double k=m[14]+(m[15]/p.z-m[14])*m[16];p={m[12]+p.x*k,m[13]-p.y*k,std::clamp(double(m[22])+p.z*m[23],-1.499,1.4)};}
  poly=SceneClip(poly,[&](ScenePoint p){return p.x-m[18];});poly=SceneClip(poly,[&](ScenePoint p){return m[20]-p.x;});poly=SceneClip(poly,[&](ScenePoint p){return p.y-m[19];});poly=SceneClip(poly,[&](ScenePoint p){return m[21]-p.y;});
  const double light=.72+std::max(0.,double(-f[7]*m[24]-f[8]*m[25]-f[9]*m[26]))*.28;const auto color=SceneColor(std::round(f[4]*light),std::round(f[5]*light),std::round(f[6]*light));
  for(size_t j=1;j+1<poly.size();j++){SceneTriangle(scope->api->graphics,poly[0],poly[j],poly[j+1],color);++emitted;}
 }
 return JS_NewInt32(ctx,emitted);
}

// ---- Retained meshes (sceneApi 2). Uploaded once per map, drawn by handle
// with the same 27-float camera sceneMesh takes. Validation, bounds and
// lighting happen at upload; a draw is a bounds test, then a clip walk over
// stack arrays with an all-inside fast path — no per-face allocation.
struct RetainedMesh {
 std::vector<ScenePoint> verts;    // world space
 std::vector<float> faces;         // 10 per face: i0..i3, r,g,b, nx,ny,nz
 std::vector<float> capsules;      // 11 per capsule: x1,y1,z1, x2,y2,z2, worldWidth, r,g,b, depthBias
 double aabb[6]{};
 std::vector<Color> lit; double litLight[3]{NAN,NAN,NAN};
 std::vector<ScenePoint> view;     // per-draw scratch
};
static bool SceneCameraValid(const float* m){
 for(size_t i=0;i<27;i++)if(!std::isfinite(m[i]))return false;
 return !(m[17]<=0||m[18]<-32200||m[19]<-32200||m[20]>32200||m[21]>32200||m[18]>=m[20]||m[19]>=m[21]);
}
static ScenePoint SceneToView(const float* m,const ScenePoint& p){
 const double x=p.x-m[0],y=p.y-m[1],z=p.z-m[2];
 return {x*m[3]+y*m[4]+z*m[5],x*m[6]+y*m[7]+z*m[8],x*m[9]+y*m[10]+z*m[11]};
}
static ScenePoint SceneProject(const float* m,const ScenePoint& p){
 const double k=m[14]+(m[15]/p.z-m[14])*m[16];
 return {m[12]+p.x*k,m[13]-p.y*k,std::clamp(double(m[22])+p.z*m[23],-1.499,1.4),p.u,p.v};
}
template<class Distance> static int SceneClipFixed(const ScenePoint* in,int n,ScenePoint* out,Distance dist){
 int count=0;
 for(int i=0;i<n;i++){const ScenePoint& a=in[i];const ScenePoint& b=in[(i+1)%n];const double da=dist(a),db=dist(b);
  if(da>=0)out[count++]=a;if((da>=0)!=(db>=0))out[count++]=SceneMix(a,b,da/(da-db));}
 return count;
}
JSValue MeshUpload(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));SceneFloats verts(ctx),faces(ctx),capsules(ctx);
 if(!scope||!scope->api||argc<2||!verts.read(args[0])||!faces.read(args[1]))return JS_NewInt32(ctx,-1);
 const bool hasCapsules=argc>2&&!JS_IsUndefined(args[2])&&!JS_IsNull(args[2]);
 if(hasCapsules&&!capsules.read(args[2]))return JS_NewInt32(ctx,-1);
 if(verts.count%3||faces.count%10||capsules.count%11||verts.count>300000||faces.count>100000||capsules.count>22000)return JS_NewInt32(ctx,-1);
 auto mesh=std::make_shared<RetainedMesh>();
 mesh->verts.reserve(verts.count/3);
 for(size_t i=0;i<verts.count;i+=3){for(int j=0;j<3;j++)if(!std::isfinite(verts.data[i+j]))return JS_NewInt32(ctx,-1);
  mesh->verts.push_back({verts.data[i],verts.data[i+1],verts.data[i+2]});}
 for(size_t i=0;i<faces.count;i+=10){for(int j=0;j<10;j++)if(!std::isfinite(faces.data[i+j]))return JS_NewInt32(ctx,-1);
  for(int j=0;j<4;j++){const double id=faces.data[i+j];if(id<0||id>=double(mesh->verts.size())||std::floor(id)!=id)return JS_NewInt32(ctx,-1);}}
 for(size_t i=0;i<capsules.count;i++)if(!std::isfinite(capsules.data[i]))return JS_NewInt32(ctx,-1);
 mesh->faces.assign(faces.data,faces.data+faces.count);
 if(hasCapsules)mesh->capsules.assign(capsules.data,capsules.data+capsules.count);
 double* b=mesh->aabb;b[0]=b[1]=b[2]=1e300;b[3]=b[4]=b[5]=-1e300;
 for(const auto& p:mesh->verts){b[0]=std::min(b[0],p.x);b[1]=std::min(b[1],p.y);b[2]=std::min(b[2],p.z);b[3]=std::max(b[3],p.x);b[4]=std::max(b[4],p.y);b[5]=std::max(b[5],p.z);}
 for(size_t i=0;i<capsules.count;i+=11)for(int e=0;e<6;e+=3){b[0]=std::min(b[0],double(capsules.data[i+e]));b[1]=std::min(b[1],double(capsules.data[i+e+1]));b[2]=std::min(b[2],double(capsules.data[i+e+2]));b[3]=std::max(b[3],double(capsules.data[i+e]));b[4]=std::max(b[4],double(capsules.data[i+e+1]));b[5]=std::max(b[5],double(capsules.data[i+e+2]));}
 if(mesh->verts.empty()&&capsules.count==0){b[0]=b[1]=b[2]=b[3]=b[4]=b[5]=0;}
 auto& store=scope->scene.meshes;
 for(size_t i=0;i<store.size();i++)if(!store[i]){store[i]=mesh;return JS_NewInt32(ctx,int32_t(i));}
 if(store.size()>=4096)return JS_NewInt32(ctx,-1);
 store.push_back(mesh);return JS_NewInt32(ctx,int32_t(store.size()-1));
}
JSValue MeshFree(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));int32_t handle=-1;
 if(!scope||argc<1||JS_ToInt32(ctx,&handle,args[0]))return JS_FALSE;
 auto& store=scope->scene.meshes;
 if(handle<0||size_t(handle)>=store.size()||!store[handle])return JS_FALSE;
 store[handle].reset();return JS_TRUE;
}
// The retained surface is mapped onto the exact pool mesh, including its
// curved facets. Projection and clipping stay native and independent of age.
JSValue DecalMesh(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));int32_t handle=-1;SceneFloats camera(ctx);
 if(!scope||!scope->api||argc<6||JS_ToInt32(ctx,&handle,args[0]))return JS_ThrowTypeError(ctx,"decalMesh requires mesh, camera and surface bounds");
 auto& store=scope->scene.meshes;
 if(handle<0||size_t(handle)>=store.size()||!store[handle])return JS_NewInt32(ctx,0);
 if(!camera.read(args[1])||camera.count!=27||!SceneCameraValid(camera.data))return JS_ThrowRangeError(ctx,"invalid decal camera");
 double bounds[4];for(int i=0;i<4;i++)if(JS_ToFloat64(ctx,&bounds[i],args[i+2])||!std::isfinite(bounds[i])||std::abs(bounds[i])>32768)return JS_ThrowRangeError(ctx,"invalid decal bounds");
 if(bounds[2]<=0||bounds[3]<=0)return JS_ThrowRangeError(ctx,"empty decal surface");
 auto& mesh=*store[handle];const auto* m=camera.data;auto& g=scope->api->graphics;
 mesh.view.resize(mesh.verts.size());
 for(size_t i=0;i<mesh.verts.size();i++){
  const auto& p=mesh.verts[i];auto q=SceneToView(m,p);
  q.u=(p.x-bounds[0])/bounds[2];q.v=(p.z-bounds[1])/bounds[3];mesh.view[i]=q;
 }
 int emitted=0;ScenePoint a[12],b[12];
 for(size_t i=0;i<mesh.faces.size();i+=10){
  const auto* f=mesh.faces.data()+i;if(std::abs(f[8])<1e-8)continue;
  int n=4;for(int j=0;j<4;j++)a[j]=mesh.view[size_t(f[j])];
  n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.z-m[17];});if(n<3)continue;
  for(int j=0;j<n;j++){
   a[j]=SceneProject(m,b[j]);a[j].z=(std::max)(-1.499,a[j].z-.0005);
   // UV/depth and 1/depth interpolate linearly in screen space, including
   // newly clipped vertices. The pixel shader restores the world-space UV.
   a[j].q=m[16]>.999?1/b[j].z:1;
   a[j].u*=a[j].q;a[j].v*=a[j].q;
  }
  n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.x-m[18];});if(n<3)continue;
  n=SceneClipFixed(b,n,a,[&](const ScenePoint& p){return m[20]-p.x;});if(n<3)continue;
  n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.y-m[19];});if(n<3)continue;
  n=SceneClipFixed(b,n,a,[&](const ScenePoint& p){return m[21]-p.y;});if(n<3)continue;
  for(int j=1;j+1<n;j++){
   const auto& p=a[0];const auto& q=a[j];const auto& r=a[j+1];
   g.decal_triangle({float(p.x),float(p.y),float(p.z),float(p.u),float(p.v),
    float(q.x),float(q.y),float(q.z),float(q.u),float(q.v),
    float(r.x),float(r.y),float(r.z),float(r.u),float(r.v),{255,255,255,255},float(p.q),float(q.q),float(r.q)});emitted++;
  }
 }return JS_NewInt32(ctx,emitted);
}
// sdfFigure(prims, count, camera) → true when the host raymarches the figure
// (sceneApi 3). prims is count × 12 floats: a.xyz, r1, b.xyz, r2, r, g, b
// (0–1) and hard (1 = plain union, 0 = smooth blend). One call per figure
// replaces the ~500 triangle calls a tessellated figure costs in QuickJS.
JSValue SdfFigure(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));SceneFloats prims(ctx),camera(ctx);int32_t count=0;
 if(!scope||!scope->api||argc<3||!prims.read(args[0])||JS_ToInt32(ctx,&count,args[1])||!camera.read(args[2]))
  return JS_ThrowTypeError(ctx,"sdfFigure requires prims, count and camera");
 if(count<1||count>48||prims.count<size_t(count)*12||camera.count!=27||!SceneCameraValid(camera.data))
  return JS_ThrowRangeError(ctx,"sdfFigure layout or budget");
 for(size_t i=0;i<size_t(count)*12;i++){const float v=prims.data[i];
  if(!std::isfinite(v)||std::abs(v)>1e6f)return JS_ThrowRangeError(ctx,"non-finite sdf prim");}
 for(int32_t i=0;i<count;i++){const float* p=prims.data+i*12;
  if(p[3]<0||p[3]>512||p[7]<0||p[7]>512)return JS_ThrowRangeError(ctx,"sdf prim radius");}
 return JS_NewBool(ctx,scope->api->graphics.sdf_figure(camera.data,prims.data,size_t(count)));
}
// meshDraw(handle, camera, capsuleScale = 1, tintR = 1, tintG = 1, tintB = 1) → triangles emitted.
// capsuleScale is the game's cameraScale(): edge capsules store a world width
// and take their pixel width from it, as drawQuadMesh did in JS. Zero or less
// skips the capsules — the P1 inset never drew them, and keeps not drawing them.
JSValue MeshDraw(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));int32_t handle=-1;SceneFloats camera(ctx);
 if(!scope||!scope->api||argc<2||JS_ToInt32(ctx,&handle,args[0]))return JS_NewInt32(ctx,0);
 auto& store=scope->scene.meshes;
 if(handle<0||size_t(handle)>=store.size()||!store[handle])return JS_NewInt32(ctx,0);
 if(!camera.read(args[1])||camera.count!=27||!SceneCameraValid(camera.data))return JS_ThrowRangeError(ctx,"meshDraw needs a 27-float camera");
 double extra[5]{1,1,1,1,1};
 for(int i=0;i<5&&argc>2+i;i++){if(JS_ToFloat64(ctx,&extra[i],args[2+i])||!std::isfinite(extra[i]))return JS_ThrowRangeError(ctx,"meshDraw scale/tint");}
 const double capsuleScale=std::clamp(extra[0],0.0,1024.0);const double tint[3]{std::clamp(extra[1],0.0,4.0),std::clamp(extra[2],0.0,4.0),std::clamp(extra[3],0.0,4.0)};
 const bool tinted=tint[0]!=1||tint[1]!=1||tint[2]!=1;
 RetainedMesh& mesh=*store[handle];const float* m=camera.data;const double near=m[17];
 auto& g=scope->api->graphics;
 // Bounds: the eight corners in view space. All behind the lens, or all past
 // one edge of the band once projected, and nothing here can be seen.
 {const double* b=mesh.aabb;int behind=0,left=0,right=0,above=0,below=0;
  for(int c=0;c<8;c++){ScenePoint p=SceneToView(m,{c&1?b[3]:b[0],c&2?b[4]:b[1],c&4?b[5]:b[2]});
   if(p.z<near){behind++;continue;}ScenePoint q=SceneProject(m,p);
   if(q.x<m[18])left++;else if(q.x>m[20])right++;if(q.y<m[19])above++;else if(q.y>m[21])below++;}
  if(behind==8)return JS_NewInt32(ctx,0);
  if(behind==0&&(left==8||right==8||above==8||below==8))return JS_NewInt32(ctx,0);}
 // Lighting is a function of the light alone; relight only when it moves.
 const size_t faceCount=mesh.faces.size()/10;
 if(mesh.lit.size()!=faceCount||mesh.litLight[0]!=m[24]||mesh.litLight[1]!=m[25]||mesh.litLight[2]!=m[26]){
  mesh.lit.resize(faceCount);mesh.litLight[0]=m[24];mesh.litLight[1]=m[25];mesh.litLight[2]=m[26];
  for(size_t i=0;i<faceCount;i++){const float* f=mesh.faces.data()+i*10;
   const double light=.72+std::max(0.,double(-f[7]*m[24]-f[8]*m[25]-f[9]*m[26]))*.28;
   mesh.lit[i]=SceneColor(std::round(f[4]*light),std::round(f[5]*light),std::round(f[6]*light));}}
 mesh.view.resize(mesh.verts.size());
 for(size_t i=0;i<mesh.verts.size();i++)mesh.view[i]=SceneToView(m,mesh.verts[i]);
 int emitted=0;ScenePoint a[9],b[9];
 for(size_t i=0;i<faceCount;i++){const float* f=mesh.faces.data()+i*10;
  Color color=mesh.lit[i];
  color.a=uint8_t(std::clamp(extra[4],0.0,1.0)*255);
  if(tinted){const auto alpha=color.a;color=SceneColor(color.r*tint[0],color.g*tint[1],color.b*tint[2]);color.a=alpha;}
  int n=4;for(int j=0;j<4;j++)a[j]=mesh.view[size_t(f[j])];
  bool front=true;for(int j=0;j<4;j++)if(a[j].z<near){front=false;break;}
  if(front){
   for(int j=0;j<4;j++)a[j]=SceneProject(m,a[j]);
   int left=0,right=0,above=0,below=0,inside=0;
   for(int j=0;j<4;j++){if(a[j].x<m[18])left++;else if(a[j].x>m[20])right++;if(a[j].y<m[19])above++;else if(a[j].y>m[21])below++;
    if(a[j].x>=m[18]&&a[j].x<=m[20]&&a[j].y>=m[19]&&a[j].y<=m[21])inside++;}
   if(left==4||right==4||above==4||below==4)continue;
   if(inside==4){SceneTriangle(g,a[0],a[1],a[2],color);SceneTriangle(g,a[0],a[2],a[3],color);emitted+=2;continue;}
  }else{
   n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.z-near;});if(n<3)continue;
   for(int j=0;j<n;j++)a[j]=SceneProject(m,b[j]);
  }
  n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.x-m[18];});if(n<3)continue;
  n=SceneClipFixed(b,n,a,[&](const ScenePoint& p){return m[20]-p.x;});if(n<3)continue;
  n=SceneClipFixed(a,n,b,[&](const ScenePoint& p){return p.y-m[19];});if(n<3)continue;
  n=SceneClipFixed(b,n,a,[&](const ScenePoint& p){return m[21]-p.y;});if(n<3)continue;
  for(int j=1;j+1<n;j++){SceneTriangle(g,a[0],a[j],a[j+1],color);emitted++;}
 }
 // Edge capsules: worldSegment's near clip, then the fan at min depth + bias.
 const double guard=m[20]-m[18];
 for(size_t i=0;capsuleScale>0&&i<mesh.capsules.size();i+=11){const float* c=mesh.capsules.data()+i;
  ScenePoint p=SceneToView(m,{c[0],c[1],c[2]}),q=SceneToView(m,{c[3],c[4],c[5]});
  if(p.z<near&&q.z<near)continue;
  if(p.z<near)p=SceneMix(p,q,(near-p.z)/(q.z-p.z));else if(q.z<near)q=SceneMix(q,p,(near-q.z)/(p.z-q.z));
  const ScenePoint from=SceneProject(m,p),to=SceneProject(m,q);
  if(!std::isfinite(from.x)||!std::isfinite(from.y)||!std::isfinite(to.x)||!std::isfinite(to.y))continue;
  if((from.x<m[18]-guard&&to.x<m[18]-guard)||(from.x>m[20]+guard&&to.x>m[20]+guard)||(from.y<m[19]-guard&&to.y<m[19]-guard)||(from.y>m[21]+guard&&to.y>m[21]+guard))continue;
  Color color=SceneColor(c[7],c[8],c[9]);
  if(tinted){const auto alpha=color.a;color=SceneColor(color.r*tint[0],color.g*tint[1],color.b*tint[2]);color.a=alpha;}
  const double width=std::max(1.0,double(c[6])*capsuleScale),depth=std::clamp(std::min(from.z,to.z)+double(c[10]),-1.499,1.4);
  const double radius=width*.5;const int fan=radius<6?3:radius<13?4:radius<26?6:radius<52?8:12;
  SceneCapsuleFan(g,from.x,from.y,to.x,to.y,depth,width,color);emitted+=2+2*fan;
 }
 return JS_NewInt32(ctx,emitted);
}
