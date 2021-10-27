library(igraph)

clqexmp <- graph.formula(A:B:C:D--A:B:C:D, D-E, E-F-G-E)

clique.number(clqexmp)
cliques(clqexmp, min = 3)
maximal.cliques(clqexmp, min = 3)
largest.cliques(clqexmp)
V(clqexmp)[unlist(largest.cliques(clqexmp))]

library(UserNetR)
data(DHHS)
library(intergraph)
iDHHS <- asIgraph(DHHS)
graph.density(iDHHS)
iDHHS <- subgraph.edges(iDHHS, E(iDHHS)[collab > 2])
graph.density(iDHHS)

coreness <- graph.coreness(iDHHS)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness

Vname <- get.vertex.attribute(iDHHS, name='vertex.names', index = V(iDHHS))
V(iDHHS)$name <- Vname
V(iDHHS)$color <- coreness + 1
op <- par(mar = rep(0,4))
plot(iDHHS, vertex.label.cex = 0.8)

colors <- rainbow(maxCoreness)
plot(iDHHS, vertex.label = coreness, vertex.color = colors[coreness])

V(iDHHS)$name <- coreness
V(iDHHS)$color <- colors[coreness]
iDHHS1_6 <- iDHHS
iDHHS2_6 <- induced.subgraph(iDHHS, vids = which(coreness > 1))
iDHHS3_6 <- induced.subgraph(iDHHS, vids = which(coreness > 2))
iDHHS4_6 <- induced.subgraph(iDHHS, vids = which(coreness > 3))
iDHHS5_6 <- induced.subgraph(iDHHS, vids = which(coreness > 4))
iDHHS6_6 <- induced.subgraph(iDHHS, vids = which(coreness > 5))

lay <- layout.fruchterman.reingold(iDHHS)
par(mfrow = c(3,2), mar = c(3,0,2,0))
plot(iDHHS1_6, layout = lay, main = "All k-cores")
plot(iDHHS2_6, layout = lay[which(coreness > 1), ], main = "k-cores 2-6" )
plot(iDHHS3_6, layout = lay[which(coreness > 2), ], main = "k-cores 3-6" )
plot(iDHHS4_6, layout = lay[which(coreness > 3), ], main = "k-cores 4-6" )
plot(iDHHS5_6, layout = lay[which(coreness > 4), ], main = "k-cores 5-6" )
plot(iDHHS6_6, layout = lay[which(coreness > 5), ], main = "k-cores 6-6" )

g1 <- graph.formula(A-B-C-A, D-E-F-D, G-H-I-G, A-D-G-A)
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3)
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)

par(mfrow = c(1,2))
plot(g1, vertex.color = (V(g1)$grp_good), vertex.size = 20, main = "Good Grouping")
plot(g1, vertex.color = (V(g1)$grp_bad), vertex.size = 20, main = "Bad Grouping")
modularity(g1, V(g1)$grp_good)
modularity(g1, V(g1)$grp_bad)

library(intergraph)
data(DHHS)
iDHHS <- asIgraph(DHHS)
table(V(iDHHS)$agency)
V(iDHHS)[1:10]$agency
modularity(iDHHS, (V(iDHHS)$agency+1))

data(Moreno)
iMoreno <- asIgraph(Moreno)
table(V(iMoreno)$gender)
modularity(iMoreno, V(iMoreno)$gender)

data("Facebook")
levels(factor(V(Facebook)$group))
grp_num <- as.numeric(factor(V(Facebook)$group))
modularity(Facebook, grp_num)

cw <- cluster_walktrap(iMoreno)
membership(cw)
modularity(cw)
par(mfrow = c(1,1))
plot(cw, iMoreno)

cw_dh <- cluster_walktrap(iDHHS)
modularity(cw_dh)
membership(cw_dh)
table(V(iDHHS)$agency, membership(cw_dh))

data(Bali)
iBali <- asIgraph(Bali)

cw_bali <- cluster_walktrap(iBali)
modularity(cw_bali)
membership(cw_bali)
ceb_bali <- cluster_edge_betweenness(iBali)
modularity(ceb_bali)
membership(ceb_bali)
cs_bali <- cluster_spinglass(iBali)
modularity(cs_bali)
membership(cs_bali)
cfg_bali <- cluster_fast_greedy(iBali)
modularity(cfg_bali)
membership(cfg_bali)
clp_bali <- cluster_label_prop(iBali)
modularity(clp_bali)
membership(clp_bali)
cle_bali <- cluster_leading_eigen(iBali)
modularity(cle_bali)
membership(cle_bali)
cl_bali <- cluster_louvain(iBali)
modularity(cl_bali)
membership(cl_bali)
co_bali <- cluster_optimal(iBali)
modularity(co_bali)
membership(co_bali)

table(V(iBali)$role, membership(cw_bali))
compare(as.numeric(factor(V(iBali)$role)), cw_bali, method = "adjusted.rand")
compare(cw_bali, ceb_bali, method = "adjusted.rand")
compare(cw_bali, cs_bali, method = "adjusted.rand")
compare(cw_bali, cfg_bali, method = "adjusted.rand")

par(mfrow = c(3,2), mar = c(3,0,2,0))
plot(ceb_bali, iBali, vertex.label = V(iBali)$role, main = "Edge Betweenness")
plot(cfg_bali, iBali, vertex.label = V(iBali)$role, main = "Fast Greedy")
plot(clp_bali, iBali, vertex.label = V(iBali)$role, main = "Label Propagation")
plot(cle_bali, iBali, vertex.label = V(iBali)$role, main = "Leading Eigenvector")
plot(cs_bali, iBali, vertex.label = V(iBali)$role, main = "Spinglass")
plot(cw_bali, iBali, vertex.label = V(iBali)$role, main = "Walktrap")