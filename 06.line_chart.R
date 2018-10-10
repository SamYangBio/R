args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]


library(ggplot2)
data = read.table("year_distribution.test", header = T)

p = ggplot(data , aes(x=reorder(factor(data$year),data$order), y = data$count, group= data$gene, colour = data$gene))
p<-p+geom_line(size=0.5)#折线大小设置
p<-p+geom_point(size=1)#散点大小设置

theme_zg <- function(..., bg='white'){
	require(grid)
	theme_classic(...) +
	theme(rect=element_rect(fill=bg),
	      plot.margin=unit(rep(0.5,4), 'lines'),
	      panel.background=element_rect(fill='transparent', color='black'),
	      panel.border= element_blank(),
	      panel.grid=element_blank(),
	      axis.title = element_text(color='black', vjust=0.1),
	      plot.title=element_text(hjust=0.5),#设置主标题的位置
	      #axis.ticks.length = unit(-0.4,"lines"),
	      #axis.ticks = element_line(color='black'),
	      #axis.ticks.margin = unit(0.8,"lines"),
	      axis.line = element_line(size=0.1, colour = "black"),
	      legend.title=element_blank(),
	      legend.key=element_rect(fill='transparent', color='transparent'))
}

p =p + theme_zg() + theme(axis.text.x=element_text(angle= 90,hjust= 0.8 ) ) +
	ylab("Number of articles")+ 
	xlab("Year")  
	#labs(title = "Boxplot for RPM values")
ggsave(p,file=out, width=8,height=6)

