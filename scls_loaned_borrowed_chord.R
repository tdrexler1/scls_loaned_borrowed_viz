# Setup ------------------------------------------------------------------------

library(circlize)
library(dplyr)

# https://jokergoo.github.io/circlize_book/book/
# http://jokergoo.github.io/circlize/reference/index.html

# Function: get_county  ---------------------------------------------------
# return county label from library code

get_county <- function(letter_code) {
  if(letter_code %in% c('ACL', 'ROM') ){
    return('Adams')
  }
  
  else if(letter_code %in% c('CIA', 'COL', 'LDI', 'PAR', 'POR', 'POY', 'RAN', 
                             'RIO', 'WID', 'WYO') ){
    return('Columbia')
  }
  
  else if(letter_code %in% c('BLV', 'BER', 'CBR', 'CSP', 'DCL', 'MRS', 'DEE', 
                             'DFT', 'FCH', 'MAR', 'MAZ', 'MCF', 'MID', 'MOO', 
                             'MTH', 'ORE', 'STO', 'SUN', 'VER', 'WAU') ){
    return('Dane')
  }
  
  else if(letter_code %in% c('MAD', 'HPB', 'HAW', 'LAK', 'MEA', 'MSB', 'PIN', 
                             'SEQ', 'SMB') ){
    return('Madison PL')
  }
  
  else if(letter_code %in% c('ALB', 'BRD', 'MRO', 'MNT', 'NGL') ){
    return('Green')
  }
  
  else if(letter_code %in% c('AMH', 'ALM', 'PLO', 'ROS', 'STP') ){
    return('Portage')
  }
  
  else if(letter_code %in% c('BAR', 'LAV', 'NOF', 'PLA', 'PDS', 'REE', 'RKS', 
                             'SKC', 'SGR') ){ 
    return('Sauk')
  }
  
  else if(letter_code %in% c('ARP', 'MFD', 'NEK', 'PIT', 'VES', 'MCM') ){
    return('Wood')
  }
  
  else {
    'other'
    }
}


# Load & Prepare Data ----------------------------------------------------------

scls_flow_edges <- read.csv("loaned_borrowed_data.csv")

# filter out network loops (sender == receiver)
scls_flow_edges <- 
  scls_flow_edges[scls_flow_edges$from_library != scls_flow_edges$to_library, ]

# select network edges with daily item averages above
scls_flow_edges_avg20 <- scls_flow_edges[scls_flow_edges$daily_average>=20.0, ]

# data frame w/ receivers listed as senders
rcvrs_df <- 
  cbind.data.frame(
    scls_flow_edges_avg20$to_library,
    rep("-", length(scls_flow_edges_avg20$to_library)),
    scls_flow_edges_avg20$count,
    rep(0, length(scls_flow_edges_avg20$to_library))
  )

# add matching column names
colnames(rcvrs_df) <- colnames(scls_flow_edges_avg20)

# combine data frames, group by senders, sort by county & total item count
scls_flow_edges_grouped <- 
  scls_flow_edges_avg20 %>% 
  bind_rows(rcvrs_df) %>% 
  group_by(from_library) %>% 
  summarise(total_count = sum(count)) %>% 
  mutate(county = sapply(from_library, get_county)) %>% 
  arrange(county, desc(total_count) )

# county labels for sending libraries after grouping
county_grouping <-
  structure(
    scls_flow_edges_grouped$county,
    names = scls_flow_edges_grouped$from_library
    )


# Plot Formatting Setup --------------------------------------------------------

# color sequence for library sectors (22 colors)
# sub-sequences run blue, yellow, red, green, purple, orange
sector_colors = 
  structure( 
    c("#0F4C81", "#E09F3E", "#9E2A2B", "#659E2A", "#7A306C", "#BB3E03", 
      "#4F6980", "#DB9E68", "#E07972", "#638B66", "#8175AA", "#F47942", 
      "#849DB1", "#BFBB60", "#C23D49", "#005500", "#3F1CC3", "#F45909", 
      "#030A8C", "#FCC30B", "#DC3080", "#743023"
    ),
    names = scls_flow_edges_grouped$from_library
  )

# formatting for county sectors
county_sector_formats <- list(
  Adams =        c(list(bg="#000000", txt="#000000")),
  Columbia =     c(list(bg="#000000", txt="#000000")),
  Dane =         c(list(bg="#F6AE2D", txt="#000000")),
  Green =        c(list(bg="#000000", txt="#000000")),
  "Madison PL" = c(list(bg="#005A9C", txt="#FFFFFF")),
  Portage =      c(list(bg="#000000", txt="#000000")),
  Sauk =         c(list(bg="#000000", txt="#000000")),
  Wood =         c(list(bg="#000000", txt="#000000")),
  Other =        c(list(bg="#3C3B3B", txt="#FFFFFF"))
)

# Plot Chord Diagram -----------------------------------------------------------

# setup
par(bg='gray90', mar=c(0, 0, 0, 0), oma=c(0, 0, 2, 0))

# chord diagram set up
circos.clear()

circos.par(
  track.margin = c(0.01, 0.01)
  )

# draw chord diagram, group libraries by county
chordDiagram(
  scls_flow_edges_avg20[ , c(1,2,4)],
  grid.col = sector_colors,
  annotationTrack = c('grid'),
  annotationTrackHeight = 0.05,
  preAllocateTracks = list(
    list(track.height = mm_h(5)), 
    list(track.height = mm_h(10)) 
    ),
  directional = T,
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.decreasing = TRUE,
  group = county_grouping,
  order = sort(scls_flow_edges_grouped$from_library, decreasing = T)
  )

# sector text labels (3-letter codes)
circos.track(
  track.index = 2, panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1], 
      CELL_META$sector.index, 
      facing = 'clockwise', 
      niceFacing = T, adj = c(0, 0.5)
      )
    }, bg.border = NA
  )

# Outer Sectors: County Names --------------------------------------------------

# calculate span of county grouping sectors in degrees
county_spans <- 
  sapply(unique(county_grouping), function(county){
    max(sapply(names(which(county_grouping==county)), function(library){
      if(get.cell.meta.data("cell.start.degree", library)!=0){
        get.cell.meta.data("cell.start.degree", library)
      }else{
        360.0
      }
    })
    )-min(sapply(names(which(county_grouping==county)), function(library){
      get.cell.meta.data("cell.end.degree", library)
    })
    )
  }
)

# highlight counties with enough libraries to fit county name on outer sector
highlight_counties <- names(which(county_spans > 20))

# all other libraries
other_sect <- names(county_grouping[which(!county_grouping %in% highlight_counties)])

# county highlight sectors
for (sect in highlight_counties){
  highlight.sector(
    names(which(county_grouping==sect)),
    track.index = 1,
    col = county_sector_formats[[sect]]$bg,
    text = toupper(sect),
    text.col = county_sector_formats[[sect]]$txt,
    cex = 0.8,
    facing = 'bending.inside',
    niceFacing = T,
    text.vjust = 0.3,
    font = 2
  )
}

# other library highlight sector
highlight.sector(
  other_sect,
  track.index = 1,
  col = county_sector_formats$Other$bg,
  text = "OTHER",
  text.col = county_sector_formats$Other$txt,
  cex = 0.8,
  facing = 'bending.inside',
  niceFacing = T,
  text.vjust = 0.3,
  font = 2
)

# plot title & subtitle
# https://stackoverflow.com/a/55059687
# https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet.html
mtext(
  "Daily Average Items Loaned", 
  side=3, line=1, at=-1, adj=0, cex=1.5, font=2
  )
mtext(
  "Libraries lending more than 20 items/day", 
  side=3, line=0, at=-1, adj=0, cex=1
)


#' Notes & Misc. -----------------------------------------------------------

#circos.info()

#get.all.sector.index()


# DONE: change sector/link colors - automatic selection
# DONE: label county groupings
# DONE: change order of sectors w/in groups - ? dplyr sort from_lib by sum of counts ?
# DONE: change color sequence
# DONE: orient labels w/ spacing; horizontal/vertical?
# DONE: add plot title, subtitle (no caption?)
# skip: add interactivity with Shiny? tootips on hover would be really helpful
# DONE: create highlight sectors automatically with loop? use "sapply" instead
# TODO: assign bg and txt colors to county sectors
