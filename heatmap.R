args  <- commandArgs(TRUE)
input <- args[1]
out   <- args[2]

library('gplots')
a = read.table(input, sep = "\t", header = T, check.names = F)
lie  = ncol(a)
hang = nrow(a)
high_pre = hang/10
high = round(high_pre, digits = 0)
high = high + 10
high = ifelse(high < 7, 7, ifelse(high>200, 200, high))
x = a[,2:lie]
y = as.matrix(x)
y = log10(y+1)
rownames(y) = a[,1]
scale = ifelse(ncol(y)>2, "row", "none")
pdf(file = out,height=high)
par(oma = c(3,3,3,5))
heatmap.2(y, 
          Colv=NA,dendrogram=('row'), # make x axis clusterting
          col=colorRampPalette(c("navy","white","firebrick3")),
          trace = "none",
          cexCol=1,
          srtCol=45,
          scale = scale,
          cexRow = 0.6,
          lhei=c(10,100)
        )#,labRow=F)
