

my_theme <- function(base_size = 12, base_family = "") { 

  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme( 
      axis.text         = element_text(size = rel(1.0), colour="black", face="bold"), 
      axis.title.x       = element_text(colour = 'black', hjust=1, size=base_size*1.2),
      axis.title.y       = element_text(colour = 'black', angle = 0, vjust = 1,, size=base_size*1.2),
      axis.ticks        = element_line(colour = "black"), 
      legend.key        = element_rect(colour = "grey80"), 
      panel.background  = element_rect(fill = "#99dbde", colour = "NA"),
      panel.border      = element_rect(fill = NA, colour = "black"), 
      plot.title = element_text(size = base_size * 2, vjust=1.2) 
    ) 
}

library(ggplot2)

pl <- ggplot(iris, xlab="Petal length")+ggtitle("Plant growth") + aes(x=Petal.Length, y=Sepal.Length)+
  geom_point()+my_theme()
pl

y
