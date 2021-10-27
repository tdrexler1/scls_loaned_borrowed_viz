library(statnet)
library(UserNetR)

data("Moreno")

gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)
network.size(Moreno)
summary(Moreno, print.adj = F)
den_hand <- 2*46/(33*32)
den_hand
gden(Moreno)
components(Moreno)
lgc <- component.largest(Moreno, result = "graph")
gd <- geodist(lgc)
max(gd$gdist)
gtrans(Moreno, mode="graph")

data("DHHS")
network.size(DHHS)
gden(DHHS)

Moreno1<-get.inducedSubgraph(Moreno,which(Moreno %v% "gender"=="1"))
gden(Moreno1)

list.vertex.attributes(DHHS)
get.vertex.attribute(DHHS, "vertex.names")

library(statnet)
netmat<-rbind(c("A","B"),
              c("A","C"),
              c("B","C"),
              c("B","D"),
              c("C","E"))
netmatdf<-data.frame(netmat)
netmatdf
netmat.edge<-netmatdf
netmat.graph<-network(netmat.edge,matrix.type="edgelist")
summary(netmat.graph)
gden(netmat.graph)
gplot(netmat.graph,displaylabels=TRUE)
