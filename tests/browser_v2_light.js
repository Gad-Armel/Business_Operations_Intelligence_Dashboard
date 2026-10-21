(async () => {
  if (!document.body.classList.contains('aw-light')) document.querySelector('.aw-theme-toggle').click();
  await new Promise(r=>setTimeout(r,450));
  return {theme:'light',background:getComputedStyle(document.body).getPropertyValue('--v2-bg'),font:getComputedStyle(document.body).fontFamily,overflow:document.documentElement.scrollWidth>innerWidth};
})()
