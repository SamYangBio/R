args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]

library(ggplot2)
d = read.table(input,sep="\t",header=F)
dlast <- table(cut(d$V2, breaks = c(0,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,max(d$V2))))
dlast1 <- as.vector(dlast)
var <- c("<201","201-300","301-400","401-500","501-600","601-700","701-800","801-900", "901-1000", "1001-1100", "1101-1200","1201-1300","1301-1400", "1401-1500", "1501-1600", "1601-1700", "1701-1800", "1801-1900", "1901-2000", ">2000")
dl <- data.frame(dimnames(dlast),dlast1, var ) 
colnames(dl) <- c("IN","F","var")
dl$V4 =  as.integer(rownames(dl))
p = ggplot(dl, aes(x = reorder(var,V4), y= F)) + 
	geom_histogram(stat="identity", position=position_dodge(0.7),width=0.5,fill= rgb(0/255,255/255,255/255))+
		labs(title = "circRNA length distribution")+
		theme_bw()+
		theme(axis.text.x=element_text(angle= 90,hjust=1,colour = "black"), #x轴坐标标签
		      axis.text.y=element_text(hjust=1,colour = "black"), #y轴坐标标签
		      panel.grid.major = element_blank(), #去除网格线
		      panel.grid.minor=element_blank(),   #去除网格线
		      panel.border=element_rect(color= "black"), #设置外边框
		      panel.background =element_blank(),     #除去背景颜色
		      #axis.line = element_line(size=0.2, colour = "black"), #设置坐标轴的粗细，颜色
		      plot.title=element_text(hjust=0.5),#设置主标题的位置
		      legend.position="none", #不显示legend
		      #plot.margin=unit(rep(0.5,4), 'lines')
		)+
		ylab("circRNA number")+
		xlab("circRNA length")+
		geom_text(aes(label = F ),vjust= -0.2, size = 3)#添加图注
ggsave(p,file=out,height=10,width=10)
