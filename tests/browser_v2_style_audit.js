({
  activeNav:[...document.querySelectorAll('.main-sidebar .nav-link.active')].map(e=>({label:e.innerText,style:['border','outline','box-shadow','background'].map(p=>[p,getComputedStyle(e).getPropertyValue(p)])})),
  sections:[...document.querySelectorAll('.card.bs4Dash')].filter(e=>e.id).slice(0,3).map(e=>({id:e.id,scripts:[...e.parentElement.querySelectorAll('script')].map(s=>s.getAttribute('data-for'))})),
  cards:[...document.querySelectorAll('.card.bs4Dash')].filter(e=>!e.id).length,
  details:[...document.querySelectorAll('details')].map(e=>({open:e.open,display:getComputedStyle(e).display})),
  fonts:document.fonts.check('14px Inter'),
  inputs:{year:Shiny.shinyapp.$inputValues.global_year,month:Shiny.shinyapp.$inputValues.global_month},
  page:Shiny.shinyapp.$inputValues.sidebar_tabs
})
