(async () => {
  const p=document.querySelector('.js-plotly-plot');
  if(!p)return {error:'no plot'};
  const before=p._fullLayout?.font?.color;
  try {await Plotly.relayout(p,{'font.color':'#edf0f5','font.family':'Inter, Segoe UI, sans-serif','font.size':12,'legend.font.color':'#b1bbc9',paper_bgcolor:'transparent',plot_bgcolor:'transparent','hoverlabel.bgcolor':'#161a20','hoverlabel.bordercolor':'#343e4b','hoverlabel.font.color':'#edf0f5','hoverlabel.font.family':'Inter, Segoe UI, sans-serif',colorway:['#d5b77f','#92b9ef','#7dd3b0','#c0a9e8','#efc378'],'transition.duration':0});return {id:p.id,before,after:p._fullLayout.font.color};}
  catch(e){return{id:p.id,before,error:e.stack};}
})()
