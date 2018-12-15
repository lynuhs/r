lynuhs_theme <- function(){
  bg <- "#b5f5ff"
  lineCol <- "#7ec8d3"

  theme_bw() +
    theme(plot.margin = unit(c(.5, .5, .5, .5),"cm")) +
    theme(plot.background = element_rect(fill = bg)) + 
    theme(panel.background = element_rect(fill = bg)) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.border = element_blank()) +
    theme(panel.grid = element_line(colour = lineCol, linetype = "dotted")) +
    theme(panel.grid.major.x = element_line(linetype = 0)) +
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(size = 14, color = "black", face = "bold")) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_text(angle = 30)) +
    theme(plot.title = element_text(margin = unit(c(0.1,0.1,0.1,0.1),"cm"))) +
    theme(plot.subtitle = element_text(margin = unit(c(0,0,1,0),"cm")))
}
