args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]

library(ggplot2)
library(reshape2)
d =  read.table(input,header=T,sep="\t",check.names=F)
melt_rpm =  melt(d, id = "circleName")
p = ggplot(melt_rpm, aes(x= variable, y= log10(value+1))) + geom_boxplot(aes(fill=variable ),outlier.size = 0.5)

theme_zg <- function(..., bg='white'){
    require(grid)
    theme_classic(...) +
        theme(rect=element_rect(fill=bg),
              plot.margin=unit(rep(0.5,4), 'lines'),
              panel.background=element_rect(fill='transparent', color='black'),
              panel.border=element_rect(fill='transparent', color='transparent'),
              panel.grid=element_blank(),
              axis.title = element_text(color='black', vjust=0.1),
	      plot.title=element_text(hjust=0.5),#设置主标题的位置
              #axis.ticks.length = unit(-0.4,"lines"),
              #axis.ticks = element_line(color='black'),
              #axis.ticks.margin = unit(0.8,"lines"),
              legend.title=element_blank(),
              legend.key=element_rect(fill='transparent', color='transparent'))
}

p =p + theme_zg() + theme(axis.text.x=element_text(angle= 90,hjust= 0.8 ,colour = "black") , ) +
	ylab("log10(RPM+1)")+ 
	xlab("Sample") + 
	labs(title = "Boxplot for RPM values")
ggsave(p,file=out,height=10,width=10)
