raman_summary <- function(df){
  df %>% summarize(
    FWHM_max=max(FWHM),
    FHWM_min=min(FWHM),
    FWHM_range=max(FWHM)-min(FWHM),
    FWHM_mean=mean(FWHM),
    FWHM_median = median (FWHM),
    FWHM_sd=sd(FWHM),
    RS_max=max(raman_shift),
    RS_min=min(raman_shift),
    RS_range=max(raman_shift)-min(raman_shift),
    RS_mean=mean(raman_shift),
    RS_sd=sd(raman_shift)
  )
}

