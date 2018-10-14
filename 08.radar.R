args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]

library(fmsb)
data = read.table(input,header=T)
m  = max(data[1,])
data2  =rbind(rep(m,5) , rep(0,5) , data)
pdf(file=out, width=10, height=10)
radarchart(data2,pcol=rgb(0.2,0.5,0.5,0.9),cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,m,floor(m/4)), cglwd=0.8, vlcex=0.8, axistype=1 )
