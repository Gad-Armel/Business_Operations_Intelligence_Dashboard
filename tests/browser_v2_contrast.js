(async () => {
  const pause=ms=>new Promise(r=>setTimeout(r,ms));
  const rgb=hex=>{const s=hex.trim().replace('#','');return [0,2,4].map(i=>parseInt(s.slice(i,i+2),16)/255);};
  const luminance=hex=>rgb(hex).map(v=>v<=.04045?v/12.92:((v+.055)/1.055)**2.4).reduce((s,v,i)=>s+v*[.2126,.7152,.0722][i],0);
  const contrast=(a,b)=>{const x=luminance(a),y=luminance(b);return (Math.max(x,y)+.05)/(Math.min(x,y)+.05);};
  const original=document.body.classList.contains('aw-light');const report=[];
  for(const light of [false,true]) {
    if(document.body.classList.contains('aw-light')!==light)document.querySelector('.aw-theme-toggle').click();await pause(150);
    const style=getComputedStyle(document.body);const token=name=>style.getPropertyValue('--v2-'+name).trim();
    const pairs=[['text','bg'],['text','panel'],['text','raised'],['muted','bg'],['muted','panel'],['muted','raised'],['gold','panel'],['success','panel'],['warning','panel'],['risk','panel'],['gold-ink','gold']];
    report.push({theme:light?'light':'dark',pairs:pairs.map(([fg,bg])=>({fg,bg,ratio:+contrast(token(fg),token(bg)).toFixed(2),pass:contrast(token(fg),token(bg))>=4.5}))});
  }
  if(document.body.classList.contains('aw-light')!==original)document.querySelector('.aw-theme-toggle').click();
  return report;
})()
