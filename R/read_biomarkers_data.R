#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param filepath
read_biomarkers_data <- function(filepath) {
  raw_data <- foreign::read.spss(filepath,
    to.data.frame = TRUE, use.value.labels = TRUE
  )
  raw_data %>%
    dplyr::transmute(
      id = hicid,
      wave = 6.5,
      sex = zf02m1cp,
      age_weeks = fcagew,
      vo2 = fcvo2mxm,
      waistcm = fcwaist,
      waist2height = fcwhrat,
      bmiz = fcbmizc,
      bodyfat = fcfatper,
      pulsewavevel = fcpvavmn,
      pulsepressamp = fcppamn,
      bpsysamp = fcsbpamn,
      bpsys = fcbrspmn,
      bpdia = fcbrdpmn,
      bpsysz = fcsbpzsc,
      bpdiaz = fcdbpzsc,
      phospholipids = fcb02c17l1,
      cholestnonhdl = fcb02c11c1,
      cholesttotal = fcb02c11a2,
      cholesttotalhdl = fcb02c11e2,
      glucose = fcb02c03a2,
      glycoprotein = fcb02c042,
      apolipa1 = fcb02c12a2,
      apolipb = fcb02c12b2,
      trigly = fcb02c14a2,
      fastingtime = fcblfast,
      sexualmaturity = fcsms,
      servesveg = fch25c01a,
      servesfruit = fch25c01b,
      servessoftdrink = fch25c01h,
      accvalidwkdays = fcvalwkday,
      accvalidwedays = fcvalweday,
      accmvpa = fca52mvtm,
      accsed = fca52sdtm,
      condition_vision = fch01c07v,
      condition_pa = fch01c07t,
      condition_breath = fch01c07u,
      condition_feetlegs = fch01c07p
    )
}
