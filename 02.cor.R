args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]
library(ggplot2)
library(reshape2)
cor <- read.table(input,header=T,sep="\t",check.names=F)
cormelt <- melt(cor,id=c("miRNA"))
write.table(cormelt,file="tmp_cor.txt",quote=F,row.names=F,sep="\t")

#system("cat tmp_cor.txt|sed 1d |sort -k1,1 -k2,2 > tmp_cor_sort")
system("cat tmp_cor.txt|sed 1d  > tmp_cor_sort")
sort_cor <- read.table("tmp_cor_sort",header=F,sep="\t",check.names=F)
#sort_cor <- read.table("tmp_cor.txt",header=F,sep="\t",check.names=F)
#sort_cor[,3] = round(sort_cor[,3],2)
sort_cor$V4 <- as.integer(rownames(sort_cor))
head(sort_cor)
#sort_cor
p <- ggplot(sort_cor)+
	  geom_tile(aes(x=V2, y= reorder(V1,-V4), fill= V3 ))+
	 # scale_fill_gradient(low="white",high="#1E90FF",space="Lab",guide=guide_legend(title=expression(R^2),reverse=TRUE))+
	  scale_fill_gradient2(low = "blue",mid="white", high= "red",space="Lab", guide=guide_legend("none")) +
	  #geom_text(aes(x=V1,y=V2,label=V3))+
	  geom_text(aes(x=V2,y= reorder(V1,-V4), label = " "))+
	  #labs(title="Pearson correlation between samples")+
	  theme(axis.title=element_blank(),
	  axis.text.x=element_text(angle=45,hjust=1, size= 8,face="bold"),
	  axis.text.y=element_text(angle=0,hjust=1, size=10,face="bold"),
	  # axis.text=element_text(size=10,face="bold",,hjust=1),
	   plot.title=element_text(face="bold",hjust=0.5,size=15))
#system("rm -f tmp_cor.txt tmp_cor_sort")
ggsave(p,file=out,height=10,width=10)
#ggplot(cormelt)+geom_tile(aes(Sample,variable,fill=value))+scale_fill_gradient(limits=c(0.6,1),low="#FFFFFF",high="#1C86EE")+geom_text(aes(x=Sample,y=variable,label=value))+labs(title="Pearson correlation between samples")+theme(axis.title=element_blank(),plot.title=element_text(face="bold"))+scale_fill_continuous(guide=guide_legend(title=expression(R^2)))
