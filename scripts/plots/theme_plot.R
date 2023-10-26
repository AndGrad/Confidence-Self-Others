#### SET THEME FOR THE PLOTS --------------------------------------------------

font_add_google("Open Sans", "open_sans")
showtext_auto()

# set theme

theme_custom <- function(){ 
  
  font <- "open_sans"   #assign font family up front
  theme_bw(base_size = 30) %+replace%    #replace elements we want to change
    theme(  
      legend.background = element_rect(color = 'black', 
                                       fill = 'white',
                                       linetype = "solid",
                                       size= .2
      ),
      legend.title = element_text(size = 25, face = 'bold'), 
      legend.text = element_text(size = 25),
      axis.text.y = element_text(size = 25, angle = 40),
      legend.justification = "center", 
      legend.margin = margin(1, 1, 1, 1),
      legend.spacing.y = unit(0, "pt"),
      plot.title = element_text(hjust = 0.5, size = 32),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line( linetype = 'dashed', linewidth =.05, color="lightgrey" ) 
    )
  
}


theme_NHB<- function(){ 
  
  font <- "open_sans"   #assign font family up front
  theme_bw(base_size = 7) %+replace%    #replace elements we want to change
    theme(  
      legend.background = element_rect(color = 'black', 
                                       fill = 'white',
                                       linetype = "solid",
                                       size= .2
      ),
      legend.title = element_text(size = 6, face = 'bold'), 
      legend.text = element_text(size = 6),
      axis.text.y = element_text(size = 6, angle = 40),
      legend.justification = "center", 
      legend.margin = margin(1, 1, 1, 1),
      legend.spacing.y = unit(0, "pt"),
      plot.title = element_text(hjust = 0.5, size = 6),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line( linetype = 'dashed', linewidth =.05, color="lightgrey" ) 
    )
  
}


theme_set(theme_custom())
