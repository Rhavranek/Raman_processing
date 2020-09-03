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

raman_bygrain_figure <- function (df){
  df %>% ggplot()+
    geom_point(aes(x=raman_shift, y=FWHM, fill=as.character(grain)), pch=22, size =2)+
    geom_line(data=Nasdala_idealized_line, aes(x=Nasdala_shift, y=Nasdala_FWHM))+
    labs(x="Raman Shift (1/cm)", y = "FWHM (1/cm)" ) +
    scale_x_continuous(expand=c(0,0), limits=c(994,1008), breaks=seq(994, 1008, 4)) +
    scale_y_continuous(expand=c(0,0), limits = c(0,36), breaks=seq(0, 36, 6))
}



theme_figure <- function(legend = TRUE, grid = TRUE, plot_margin = c(1, 1, 1, 1), 
                         text_size = 12, axis_text_size = NULL, axis_x_rotate = 0) {
  the_theme <- theme_bw() + 
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      text = element_text(size = text_size),
      axis.text = element_text(color = "black"),
      plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_rect(fill="NA", colour = "black", size = 1), 
      strip.background = element_rect(color="black", linetype = 1),
      plot.margin = unit(plot_margin, "mm")
    )  
           
  # adjust grid
  if(!grid)
    the_theme <- the_theme + theme(panel.grid = element_blank())
  else
    the_theme <- the_theme + theme(panel.grid.minor = element_blank())
  # adjust legend
  if (!legend)
    the_theme <- the_theme + theme(legend.position = "none")
  # overwrite axis text size if provided
  if (!is.null(axis_text_size))
    the_theme <- the_theme + 
      theme(axis.text = element_text(size = axis_text_size)) 
  # axis rotation
  if (axis_x_rotate != 0) {
    the_theme <- the_theme + 
      theme(axis.text.x = element_text(angle = axis_x_rotate, vjust = 0.5, hjust = 1))
  }
  return(the_theme)
}

