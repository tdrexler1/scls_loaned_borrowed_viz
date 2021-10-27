library(statnet)
library(UserNetR)

data("Moreno")

op <-  par(mar = rep(0,4), mfrow = c(1,2))
plot(Moreno, mode = "circle", vertex.cex = 1.5)
plot(Moreno, mode = "fruchtermanreingold", vertex.cex = 1.5)
par(op)

op <-  par(mar = rep(0,4), mfrow = c(1,2))
gplot(Moreno, gmode = "graph", mode = "random", vertex.cex = 1.5, main = "Random layout")
gplot(Moreno, gmode = "graph", mode = "fruchtermanreingold", main = "Fruchterman-Reingold", vertex.cex = 1.5)
par(op)

op <- par(mar = c(0,0,4,0), mfrow = c(2,3))
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'circle', main = "circle")
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'eigen', main = "eigen")
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'random', main = "random")
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'spring', main = "spring")
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'fruchtermanreingold', main = "fruchtermanreingold")
gplot(Bali, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = 'kamadakawai', main = "kamadakawai")
par(op)

mycoords1 <- gplot(Bali, gmode = "graph", vertex.cex = 1.5)
mycoords2 <- mycoords1
mycoords2[,2] <- mycoords1[,2]*1.5
mycoords1
mycoords2

op <- par(mar = c(4,3,4,3), mfrow = c(1,2))
gplot(Bali, gmode = "graph", coord = mycoords1, vertex.cex = 1.5, suppress.axes = FALSE, 
      ylim = c(min(mycoords2[,2])-1, max(mycoords2[,2])+1), main = "Original coordinates")
gplot(Bali, gmode = "graph", coord = mycoords2, vertex.cex = 1.5, suppress.axes = FALSE, 
      ylim = c(min(mycoords2[,2])-1, max(mycoords2[,2])+1), main = "Modified coordinates")
par(op)

detach(package:statnet)
library(igraph)
library(intergraph)

iBali <- asIgraph(Bali)
op <-  par(mar = c(0,0,3,0), mfrow = c(1,3))
plot(iBali, layout = layout_in_circle, main = "Circle")
plot(iBali, layout = layout_randomly, main = "Randomly")
plot(iBali, layout = layout_with_kk, main = "Kamada-Kawai")
par(op)