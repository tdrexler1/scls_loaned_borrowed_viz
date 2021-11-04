library(circlize)
library(dplyr)

scls_flow_edges <- read.csv("loaned_borrowed_data.csv")

scls_flow_edges <- 
  scls_flow_edges[scls_flow_edges$from_library != scls_flow_edges$to_library, ]


sector_codes <- unique( c(scls_flow_edges$from_library, scls_flow_edges$to_library ) )

sector_codes_df <- 
  as.data.frame(sector_codes) %>% 
  mutate(
    county = case_when (
      sector_codes %in% c('ACL', 'ROM') ~ 'Adams',
      sector_codes %in% c('CIA', 'COL', 'LDI', 'PAR', 'POR', 'POY', 'RAN', 'RIO', 'WID', 'WYO') ~ 'Columbia',
      sector_codes %in% c('BLV', 'BER', 'CBR', 'CSP', 'DCL', 'MRS', 'DEE', 'DFT', 'FCH', 'MAR', 'MAZ', 'MCF', 'MID', 'MOO', 'MTH', 'ORE', 'STO', 'SUN', 'VER', 'WAU') ~ 'Dane',
      sector_codes %in% c('MAD', 'HPB', 'HAW', 'LAK', 'MEA', 'MSB', 'PIN', 'SEQ', 'SMB') ~ 'Madison PL',
      sector_codes %in% c('ALB', 'BRD', 'MRO', 'MNT', 'NGL') ~ 'Green',
      sector_codes %in% c('AMH', 'ALM', 'PLO', 'ROS', 'STP') ~ 'Portage',
      sector_codes %in% c('BAR', 'LAV', 'NOF', 'PLA', 'PDS', 'REE', 'RKS', 'SKC', 'SGR') ~ 'Sauk',
      sector_codes %in% c('ARP', 'MFD', 'NEK', 'PIT', 'VES', 'MCM') ~ 'Wood',
      TRUE ~ 'other'
    )
  )

county_grouping <- 
  structure(
    sector_codes_df$county, 
    names = sector_codes_df$sector_codes
    )

scls_flow_edges_avg20 <- scls_flow_edges[scls_flow_edges$daily_average>=20.0, ]




par(bg='gray85')

circos.par(
  track.margin = c(0.01, 0.01)
  )

sector_colors = c(
  WAU = "#8175AA", 
  VER = "#638B66", 
  SUN = "#E07972", 
  STO = "#DB9E68", 
  ORE = "#4F6980", 
  MTH = "#BB3E03", 
  MOO = "#7A306C", 
  MID = "#659E2A", 
  MCF = "#9E2A2B", 
  FCH = "#E09F3E",
  DFT = "#0F4C81",
  HAW = "#F47942",
  HPB = "#849DB1",
  LAK = "#BFBB60",
  MAD = "#C23D49",
  MEA = "#005500",
  MSB = "#3F1CC3",
  PIN = "#F45909",
  SEQ = "#030A8C",
  SMB = "#FCC30B",
  STP = "#DC3080",
  MCM = "#743023")

circos.clear()

chordDiagram(
  scls_flow_edges_avg20[ , c(1,2,4)],
  grid.col = sector_colors,
  annotationTrack = c('grid'),
  annotationTrackHeight = 0.05,
  #preAllocateTracks = list(track.height = max( strwidth( sector_codes_df$sector_codes) ) ), 
  #preAllocateTracks = 1,
  preAllocateTracks = list(list(track.height = mm_h(5) ), list(track.height = mm_h(10)) ),
  directional = T, 
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.decreasing = TRUE,
  group = county_grouping
  
  )



circos.track(track.index = 2, panel.fun = function(x, y) {
  circos.text(
    CELL_META$xcenter, 
    CELL_META$ylim[1], 
    CELL_META$sector.index, 
    facing = 'clockwise', 
    niceFacing = T, adj = c(0, 0.5),
    #family = 'Helvetica'
    )
  }, bg.border = NA
  )

highlight.sector(
  c('DFT', 'FCH', 'MCF', 'MID', 'MOO', 'MTH', 'ORE', 'STO', 'SUN', 'VER', 'WAU'),
  track.index = 1,
  col = "#F6AE2D",
  text = 'DANE COUNTY',
  text.col = '#000000',
  facing = 'bending.outside',
  niceFacing = T
)

highlight.sector(
  c('HAW', 'HPB', 'LAK', 'MAD', 'MEA', 'MSB', 'PIN', 'SEQ', 'SMB'),
  track.index = 1,
  col = "#005A9C",
  text = 'MADISON PL',
  text.col = '#CCCCCC',
  facing = 'bending.inside',
  niceFacing = T
)

highlight.sector(
  c('STP', 'MCM'),
  track.index = 1,
  col = "#b3b7b8",
  text = 'OTHER',
  text.col = '#000000',
  facing = 'bending.inside',
  niceFacing = T
)


circos.info()
circos.clear()



for(si in get.all.sector.index()){
  circos.axis(
    h = "bottom",
    sector.index = si,
    track.index = 1,
    labels = F,
    major.tick = F
  )
}



# TODO: change sector/link colors
# TODO: label county groupings
# TODO: change order of sectors w/in groups
# TODO: orient labels w/ spacing; horizontal/vertical?
# TODO: add plot title, caption
# TODO: add interactivity with Shiny? tootips on hover would be really helpful



############################################################################
# install.packages("extrafont")
library(extrafont)

# Auto detect the available fonts in your computer
# This can take several minutes to run
font_import()

# Font family names
fonts()

# Data frame containing the font family names
fonttable()
############################################################################

# scls_flow_edges <- 
#   scls_flow_edges %>% 
#   mutate(county = case_when(
#     from_library %in% c('ACL', 'ROM') ~ 'Adams',
#     from_library %in% c('CIA', 'COL', 'LDI', 'PAR', 'POR', 'POY', 'RAN', 'RIO', 'WID', 'WYO') ~ 'Columbia',
#     from_library %in% c('BLV', 'BER', 'CBR', 'CSP', 'DCL', 'MRS', 'DEE', 'DFT', 'FCH', 'MAR', 'MAZ', 'MCF', 'MID', 'MOO', 'MTH', 'ORE', 'STO', 'SUN', 'VER', 'WAU') ~ 'Dane',
#     from_library %in% c('MAD', 'HPB', 'HAW', 'LAK', 'MEA', 'MSB', 'PIN', 'SEQ', 'SMB') ~ 'Madison PL',
#     from_library %in% c('MCM', 'STP') ~ "thingo",
#     TRUE ~ 'other'
#     
#   )
#     
#   )


# chordDiagram(
#   scls_flow_edges_avg20[ ,c(1,2,4)], 
#   directional = T, 
#   direction.type = c("diffHeight", "arrows"),
#   link.arr.type = "big.arrow",
#   link.sort = TRUE,
#   link.decreasing = TRUE,
#   group = county_grouping
#   )

############################################################################

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
