install.packages("networkD3")
library(networkD3)
library(plotly)

setwd("/Users/lituntun/Desktop/MJ/social media analysis/assignment 4")
node<-read.csv("ussenators_nodes.csv",header=T,as.is=T)
edge<-read.csv("ussenators_edges.csv",header=T,as.is=T)
node$Twitter_handle<-tolower(node$Twitter_handle)
edge$ego<-tolower(edge$ego)
edge$followed<-tolower(edge$followed)
handle<-node$Twitter_handle
handle
edge_filter<-data.frame()
for(i in 1:84976){
  if(edge$followed[i] %in% handle){
    edge_filter<-rbind(edge_filter,edge[i,])
  }
}
edge_filter<-edge_filter[,c(2,1)]
net<-graph.data.frame(edge_filter,node,directed = T)
net
degree<-degree(net,mode="in")
sort(degree,decreasing = T)
degree<-as.data.frame(degree)
degree_out<-degree(net,mode="out")
sort(degree_out,decreasing = T)
rownames(degree)<-NULL
node<-cbind(node,degree)
net<-graph.data.frame(edge_filter,node,directed = T)
net
sort(betweenness(net, directed=T, weights=NA),decreasing = T)
L_fr <- layout_with_fr(net)

V(net)$color[V(net)$party=="Republican"] <- "red"
V(net)$color[V(net)$party=="Democrat"] <- "blue"
V(net)$shape[V(net)$Gender=="Male"] <- "square"
V(net)$shape[V(net)$Gender=="Female"] <- "circle"

V(net)$color
V(net)$shape

Xn <- L_fr[,1]
Yn <- L_fr[,2]
es <- as.data.frame(get.edgelist(net))

edge_shapes <- list()  

for(i in 1:length(E(net))) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape <- list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}


network <- plot_ly(x = ~Xn, y = ~Yn, color = V(net)$party, mode = "markers", colors=palette(colour),symbol=V(net)$Gender,symbols=c("circle","square"),marker = list(size = V(net)$degree), text =paste0(V(net)$name,"<br>","followers:",V(net)$degree), hoverinfo = "text")
network
axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE) # Don't display axis and tick labels

p <- layout(network,
            title = 'following relationships between the 45 US Senators',
            shapes = edge_shapes,
            xaxis = axis,
            yaxis = axis
)
p
p <- hide_colorbar(p) 


Sys.setenv("plotly_username"="LiTongtong")
Sys.setenv("plotly_api_key"="e23XHz4LmQKIrENI6YAH")
api_create(p, filename = "network practice")

wc <- walktrap.community(net)
dendPlot(wc, mode="hclust")
membership(wc)
reciprocity(net)
test<-table(membership(wc))
plot(wc, net,vertex.size=V(net)$degree*0.4,layout=L_fr,edge.arrow.size=.2, edge.curved=0, vertex.frame.color='#555555', vertex.label.color='black',vertex.label.cex=.7,
     vertex.shape=V(net)$shape,asp=0)
table(node$party)
