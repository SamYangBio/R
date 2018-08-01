args<-commandArgs(TRUE)
library(ggplot2)
input=args[1]
out=args[2]

data <- read.table(input,header=T)
pdf(file=out, width=10, height=10)
#data = data[order(data$num, decreasing = TRUE),]
#data = data[order(data$num),]
myLabel = as.vector(data$tsRNA_type)   
myLabel = paste(myLabel, "(", round(data$num /sum(data$num) * 100, 2), "%)", sep = "") 
count = data$num
##because the  first line 3'-tRF,5'-tRF,5'-half, i-tRF name rearrange the lines
data$tsRNA_type = c('a', 'b', 'c' , 'd')  
#p = ggplot(data, aes(x = "", y = reorder(num,-num), fill = tsRNA_type)) +
p = ggplot(data, aes(x = "", y = num, fill = tsRNA_type)) +
  geom_bar(stat = "identity", width = 0.2 ) +  
  #geom_rect() +
  coord_polar(theta = "y") + 
  theme_bw() + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_blank()) + 
  geom_text(aes(y = count/2 + c(0, cumsum(count)[-length(count)]), x = 1, label = myLabel), size = 5) +
  theme(panel.grid=element_blank()) +    ## 去掉白色圆框和中间的坐标线
  theme(panel.border=element_blank())   ## 去掉最外层正方形的框框
p
dev.off()

