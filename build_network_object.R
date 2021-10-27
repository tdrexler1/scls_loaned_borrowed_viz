library(statnet)

scl_net.edge <- read.csv("loaned_borrowed_data.csv")

scl_net.edge <- scl_net.edge[scl_net.edge$from_library != scl_net.edge$to_library, ]

scl_net <- network(scl_net.edge, matrix.type = "edgelist", loops = T)

summary(scl_net)


gplot(scl_net, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = "fruchtermanreingold")

plot(scl_net, attrname='daily_average', displaylabels=TRUE, thresh=10, usearrows=T, mode='fruchtermanreingold', displayisolates = F)


detach(package:statnet)

library(igraph)

i_scl_net <- graph_from_data_frame(scl_net.edge)

summary(i_scl_net)
edge_attr(i_scl_net)

plot(i_scl_net)

i_scl_net_pared <- delete.edges(i_scl_net, which(E(i_scl_net)$daily_average<10))
i_scl_net_pared <- delete.vertices(i_scl_net_pared, degree(i_scl_net_pared)==0 )

set_edge_attr(i_scl_net_pared, "color", value = ifelse(E(i_scl_net_pared)$daily_average >= 20, "blue", "grey") )

E(i_scl_net_pared)$daily_average

E(i_scl_net_pared)$color <- ifelse( E(i_scl_net_pared)$daily_average >= 20, "blue", "grey")

edge_attr(i_scl_net_pared)

plot(i_scl_net_pared, layout=layout_in_circle, color=E(i_scl_net_pared)$color)
