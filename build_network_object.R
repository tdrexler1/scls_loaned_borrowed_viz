library(circlize)
library(dplyr)

scls_flow_edges <- read.csv("loaned_borrowed_data.csv")

scls_flow_edges <- 
  scls_flow_edges[scls_flow_edges$from_library != scls_flow_edges$to_library, ]

scls_flow_edges <- 
  scls_flow_edges %>% 
  mutate(county = case_when(
    from_library %in% c('ACL', 'ROM') ~ 'Adams',
    from_library %in% c('CIA', 'COL', 'LDI', 'PAR', 'POR', 'POY', 'RAN', 'RIO', 'WID', 'WYO') ~ 'Columbia',
    from_library %in% c('BLV', 'BER', 'CBR', 'CSP', 'DCL', 'MRS', 'DEE', 'DFT', 'FCH', 'MAR', 'MAZ', 'MCF', 'MID', 'MOO', 'MTH', 'ORE', 'STO', 'SUN', 'VER', 'WAU') ~ 'Dane',
    from_library %in% c('MAD', 'HPB', 'HAW', 'LAK', 'MEA', 'MSB', 'PIN', 'SEQ', 'SMB') ~ 'Madison PL',
    TRUE ~ 'other'
    
  )
    
  )

scls_flow_edges_avg20 <- scls_flow_edges[scls_flow_edges$daily_average>=20.0, ]

circos.par(gap.degree = 2)

county_grouping <- structure(scls_flow_edges_avg20$county, names=scls_flow_edges_avg20$from_library)

chordDiagram(
  scls_flow_edges_avg20[ ,c(1,2,4)], 
  directional = T, 
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow",
  group = county_grouping
  )


library(statnet)
scl_net <- network(scl_net.edge, matrix.type = "edgelist", loops = T)

summary(scl_net)


gplot(scl_net, gmode = "graph", edge.col = "grey75", vertex.cex = 1.5, mode = "fruchtermanreingold")

plot(scl_net, attrname='daily_average', displaylabels=TRUE, thresh=10, usearrows=T, mode='fruchtermanreingold', displayisolates = F)


detach(package:statnet)

library(igraph)

i_scl_net <- graph_from_data_frame(scl_net.edge)

summary(i_scl_net)

i_scl_net_pared <- delete.edges(i_scl_net, which(E(i_scl_net)$daily_average<10))
i_scl_net_pared <- delete.vertices(i_scl_net_pared, degree(i_scl_net_pared)==0 )

#set_edge_attr(i_scl_net_pared, "color", value = ifelse(E(i_scl_net_pared)$daily_average >= 20, "blue", "grey") )


E(i_scl_net_pared)$color <- ifelse( E(i_scl_net_pared)$daily_average >= 20, "blue", "grey")

plot(i_scl_net_pared, layout=layout_in_circle, color=E(i_scl_net_pared)$color)
