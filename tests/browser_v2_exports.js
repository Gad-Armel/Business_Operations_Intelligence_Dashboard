(async () => {
  const report=[];
  for (const id of ['quotation_download_excel','quotation_download_pdf']) {
    const link=document.getElementById(id);if(!link?.href)throw Error('Missing download URL: '+id);
    const response=await fetch(link.href);
    const bytes=new Uint8Array(await response.arrayBuffer());
    report.push({id,status:response.status,type:response.headers.get('content-type'),size:bytes.length,signature:String.fromCharCode(...bytes.slice(0,8))});
  }
  return report;
})()
