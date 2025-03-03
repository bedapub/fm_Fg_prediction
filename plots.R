library(ggplot2)

create_plot <- function(data, df_tv, ymin, ymax, yminA, ymaxA, est.fm_50, est.Fg_50, lim_fm_min, FG_lim_min, lim_fm_max, FG_lim_max, Show_seg_var = TRUE) {
  p <- ggplot() +
    geom_ribbon(aes(x = data$x, ymin = ymin, ymax = ymax), fill = "orange", alpha = 0.3) +
    geom_line(aes(x = df_tv$x, y = df_tv$A), color = "blue", size = 2) +
    geom_ribbon(aes(x = data$x, ymin = yminA, ymax = ymaxA), fill = "blue", alpha = 0.3) +
    geom_line(aes(x = df_tv$x, y = df_tv$C), color = "orange", size = 2) +
    theme_bw() +
    labs(x = expression(f[m]), y = expression(F[G])) +
    theme(
      plot.title = element_text(size = 28),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 16)
    ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.10)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.10)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme(aspect.ratio = 1)
  
  
  print(lim_fm_min)
  print(FG_lim_min)
  print(lim_fm_max)
  print(FG_lim_max)
 
  
  if (Show_seg_var == TRUE) {
    if (est.fm_50 != 'out of range') {
      print("R")
      p <- p +
        geom_segment(aes(x = 0, y = est.Fg_50, xend = est.fm_50, yend = est.Fg_50), linetype = "dashed", color = "red", size = 1) +
        geom_segment(aes(x = est.fm_50, y = 0, xend = est.fm_50, yend = est.Fg_50), linetype = "dashed", color = "red", size = 1)
      
      if (lim_fm_min != "0" | FG_lim_min != "0") {
        print("R2")
        p <- p +
          geom_segment(aes(x = 0, y = FG_lim_min, xend = lim_fm_min, yend = FG_lim_min), linetype = "dotted", color = "black", size = 1) +
          geom_segment(aes(x = lim_fm_min, y = 0, xend = lim_fm_min, yend = FG_lim_min), linetype = "dotted", color = "black", size = 1)
      }
      
      if (FG_lim_max != "1" ) {
        print("R3")
        p <- p +
          geom_segment(aes(x = 0, y = FG_lim_max, xend = lim_fm_max, yend = FG_lim_max), linetype = "dotted", color = "black", size = 1) +
          geom_segment(aes(x = lim_fm_max, y = 0, xend = lim_fm_max, yend = FG_lim_max), linetype = "dotted", color = "black", size = 1)
      }
    }
  }
  
  return(p)
}