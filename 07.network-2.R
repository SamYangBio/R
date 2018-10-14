args<-commandArgs(TRUE)
input=args[1]
out=args[2]
input2=args[3]    #mRNA  up
input3=args[4]    #lncRNA up
#input4=args[5]
library(igraph)
options(stringsAsFactors=F)
t=read.table(input)
t=unique(t)
#t=data.frame(a=sample(t[,1],200),b=sample(t[,2],200))
d=data.frame(p1=t[,1],p2=t[,2],weight=0.1)
g = graph.data.frame(d, directed = TRUE)
v=c(unique(d[,1]),unique(setdiff(d[,2],d[,1])))    ###V(g)��˳��
######xTF=match(t[,1][t[,3]=="tmi",],v)
#####V(g)color
x_v=1:length(v)
x_MRNA=grep("XM_|NM_",v) ;x_lncRNA=x_v[-x_MRNA]
if(!is.na(input2)) {
upgene=read.table(input2);upgene=upgene[,1]
xup=match(upgene,v)   
}
if(!is.na(input3)) {
upgene3=read.table(input3);upgene3=upgene3[,1]
xup3=match(upgene3,v)
}
#if(!is.na(input4)) {
#upgene4=read.table(input4);upgene4=upgene4[,1]
#xup4=match(upgene4,v)
#}
#spes=unique(t[t[,3]!="PPI",2])
V(g)$color=rep("turquoise",vcount(g))

E(g)$color<-"grey90"
Vsi=ifelse(vcount(g)<100,6,ifelse(vcount(g)>700,2,4))
V(g)$size=rep(Vsi,vcount(g))
Vla=ifelse(vcount(g)<100,0.5,ifelse(vcount(g)>700,0.1,0.3))
V(g)$lable.cex=rep(Vla,vcount(g))*0.8
vertex.frame.color=rep("turquoise",vcount(g))
if(!is.na(match("xup",ls()))) {  
V(g)$color[xup]="red"     
vertex.frame.color[xup]="red"
}
if(!is.na(match("xup3",ls()))) {  
V(g)$color[xup3]="red"     
vertex.frame.color[xup3]="red"
}
shape=rep("circle",vcount(g))
shape[x_MRNA]="square"
V(g)$shape=shape
#if(!is.na(match("xup4",ls()))) {  
#V(g)$color[xup4]="green"     
#vertex.frame.color[xup4]="green"
#}
mRNA_E=V(g)[x_MRNA]
lncRNA_E=V(g)[x_lncRNA]
#E(g)[ mRNA_E %--% mRNA_E ]$color="lightblue"     
#E(g)[ mRNA_E %--% lncRNA_E ]$color="purple"
#E(g)[ lncRNA_E %--% lncRNA_E ]$color="pink"

pdf(out)
#layout.graphopt(g)  layout.circle(g)
lay1=layout.sphere(g)
#lay1=layout.circle(g)
plot(g,layout=lay1,vertex.shape=V(g)$shape,vertex.size=V(g)$size,vertex.label.cex=V(g)$lable.cex,edge.width = E(g)$weight,edge.color=E(g)$color,vertex.color=V(g)$color,edge.arrow.size=0,vertex.label.color="black",vertex.frame.color=vertex.frame.color)
dev.off()
