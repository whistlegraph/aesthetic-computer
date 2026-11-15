# Full-bleed store figures: the screen fills the frame, the type sits on a paper plate over it.
from PIL import Image, ImageDraw, ImageFont, ImageFilter
R='/Users/jas/aesthetic-computer/system/public/'
YW=R+'type/webfonts/ywft-processing-bold.ttf'
LM=R+'papers.aesthetic.computer/fonts/lmroman10-bold.otf'
BG=(244,242,236); INK=(26,26,26); PINK=(255,105,180)
shots=[('prompt','raw/{}_prompt.png','prompt','One prompt. Type a word and it becomes something to use.','top'),
       ('chat','raw/{}_chat.png','laer-klokken','A room with people in it.','top'),
       ('notepat','raw/{}_notepat.png','notepat','A keyboard for ten fingers. Chords work.','top'),
       ('paint','raw/{}_line.png','line','Each word is a brush — rect, oval, line, shape. Enter dl to keep the painting.','top'),
       ('kidlisp','raw/{}_roz.png','$roz','Eight lines of KidLisp. The source sits on top of what it draws.','bottom'),
       ('juke','raw/{}_juke.png','juke','Songs made here, played here. Every track in the pop catalog.','bottom')]
def greedy(d,words,f,maxw):
    lines=[];cur=''
    for w in words:
        t=(cur+' '+w).strip()
        if d.textlength(t,font=f)<=maxw: cur=t
        else: lines.append(cur);cur=w
    lines.append(cur); return lines
def wrap(d,text,f,maxw):
    words=text.split(); n=len(greedy(d,words,f,maxw)); lo,hi=1,maxw
    while hi-lo>4:
        mid=(lo+hi)/2; g=greedy(d,words,f,mid)
        if len(g)<=n and all(d.textlength(l,font=f)<=maxw for l in g): hi=mid
        else: lo=mid
    return greedy(d,words,f,hi)
for dev,(W,H) in {'iphone69':(1320,2868),'ipad13':(2064,2752)}.items():
    ph=dev=='iphone69'
    for i,(name,src,title,prose,where) in enumerate(shots,1):
        im=Image.open(src.format(dev)).convert('RGB')
        if im.size!=(W,H): im=im.resize((W,H),Image.LANCZOS)
        d=ImageDraw.Draw(im)
        inset=int(W*0.04); pad=int(W*(0.05 if ph else 0.03)); pw=W-2*inset
        tsize=int(W*(0.14 if ph else 0.08)); ft=ImageFont.truetype(YW,tsize)
        while d.textlength(title+'.',font=ft)>pw-2*pad: ft=ImageFont.truetype(YW,ft.size-4)
        fp=ImageFont.truetype(LM,int(W*(0.049 if ph else 0.034)))
        lines=wrap(d,prose,fp,pw-2*pad); lh=int(fp.size*1.18)
        ph_=pad+int(tsize*1.1)+lh*len(lines)+pad
        # float the block to the calmest band of this screen: least edge detail under it, with a mild pull toward top or bottom
        g=im.convert('L').resize((W//8,H//8)); ed=g.filter(ImageFilter.FIND_EDGES); gw,gh=ed.size; px=ed.load()
        rows=[sum(px[xx,yy] for xx in range(gw)) for yy in range(gh)]
        best=None
        for yy in range(int(gh*0.035),gh-int(gh*0.025)-ph_//8,max(1,gh//120)):
            busy=sum(rows[yy:yy+ph_//8])/(gw*(ph_//8))
            mid=yy+ph_//16; pull=min(mid,gh-mid)/gh*6
            sc=busy+pull
            if best is None or sc<best[0]: best=(sc,yy)
        y0=best[1]*8
        if (dev,name)==('ipad13','notepat'): y0=int(H*0.035)  # keys fill this screen; sit over the key tops
        # no plate: type straight on the screen with a hard offset shadow, coloured against what is under it
        region=im.crop((inset,y0,inset+pw,y0+ph_)).convert('L').resize((32,32))
        light=sum(region.tobytes())/1024>150
        FG,SH=((26,26,26),(255,255,255)) if light else ((255,255,255),(0,0,0))
        # a soft, feathered wash behind the type so it reads over busy screens while the interface shows through
        tw_=max([d.textlength(title+'.',font=ft)]+[d.textlength(l,font=fp) for l in lines])
        box=(inset+pad-int(pad*0.6),y0+int(pad*0.2),int(inset+pad+tw_+pad*0.6),y0+ph_-int(pad*0.3))
        mask=Image.new('L',(W,H),0); ImageDraw.Draw(mask).rounded_rectangle(box,int(W*0.03),fill=int(255*(0.62 if light else 0.5)))
        mask=mask.filter(ImageFilter.GaussianBlur(W*0.018))
        im.paste(Image.new('RGB',(W,H),(255,255,255) if light else (0,0,0)),(0,0),mask); d=ImageDraw.Draw(im)
        def put(xy,t,f,fill):
            o=max(3,int(f.size*(0.07 if f is ft else 0.1)))
            d.text((xy[0]+o,xy[1]+o),t,font=f,fill=SH); d.text(xy,t,font=f,fill=fill)
        x=inset+pad; y=y0+pad-int(tsize*0.12)
        put((x,y),title,ft,FG); put((x+d.textlength(title,font=ft),y),'.',ft,PINK)
        y+=int(tsize*1.1)+int(tsize*0.12)
        for ln in lines: put((x,y),ln,fp,FG); y+=lh
        im.save(f'../screenshots/ios/en-US/{i}_{dev}_{name}.png')
names=[s[0] for s in shots]
for dev in ['iphone69','ipad13']:
    ims=[Image.open(f'../screenshots/ios/en-US/{i}_{dev}_{n}.png') for i,n in enumerate(names,1)]
    h=900; ims=[im.resize((int(im.width*h/im.height),h),Image.LANCZOS) for im in ims]
    s=Image.new('RGB',(sum(i.width for i in ims)+10*len(ims)+10,h+20),(120,120,120)); x=10
    for im in ims: s.paste(im,(x,10)); x+=im.width+10
    s.save(f'sheet_{dev}.png')
