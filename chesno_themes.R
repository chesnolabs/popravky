chesno_theme <- function() {
  
  theme(text = element_text(family = "OpenSans", size=30),
      plot.caption = element_text(hjust = 1, 
                                  size=18,
                                  colour = "darkgrey"),
      panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title=element_blank(),
      legend.text = element_text( size=12),
      legend.background = element_blank(),
      strip.background=element_blank(),
      axis.text.x = element_text(size = 12, colour = "black"), #цифри внизу #201d41
      axis.text.y = element_text(size = 12, colour = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(fill = "white"),
      panel.background= element_rect(fill = "white", color = "white"))
  
}

chesno_theme_2 <- function() {
  theme(text = element_text(family = "OpenSans", size=24, colour = "black" ),
        plot.caption = element_text(hjust = 1, 
                                    #face = "italic", 
                                    color="grey", 
                                    size=14),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(size = 18, colour = "black"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        #strip.text.x =element_text(size = 22),  # Facets names
        axis.text.x = element_text(size = 18,  
                                   colour = "black"), #цифри внизу#201d41
        axis.text.y = element_text(size = 18,  colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))
  
  
}

chesno_theme_3 <- function(){
  theme(text = element_text( size=20),
        plot.caption = element_text(hjust = 1, 
                                    color="grey", 
                                    size=18),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(family = "OpenSans", size = 14, colour = "black"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        #strip.text.x =element_text(size = 22),  # Facets names
        axis.text.x = element_text(size = 14,  colour = "black"), #цифри внизу#201d41
        axis.text.y = element_text(size = 14,   colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 2, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))
  
}
