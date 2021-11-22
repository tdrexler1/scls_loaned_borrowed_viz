county_sector_formats <- list(
  Dane = c(list(bg="#F6AE2D", txt="#000000")),
  "Madison PL" = c(list(bg="#005A9C", txt="#CCCCCC")),
  Other = c(list(bg="#3C3B3B", txt="#FFFFFF"))
)
county_sector_formats
unique(county_grouping)

# highlight_counties <- c("Dane", "Madison PL")

highlight_counties <- names(which(table(county_grouping) > 5))
other_sect <- names(county_grouping[which(!county_grouping %in% highlight_counties)])

#other_group <- which((county_grouping!="Madison PL" & county_grouping!="Dane"))
# 
# highlight_groups <- 
#   unique(
#     replace(
#       county_grouping, 
#       other_group,
#       "Other"
#       )
#   )

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


# 
# for (sect in unique(county_grouping)){
#   print(sect)
#   ifelse ((sect!="Dane" & sect!="Madison PL"),
#     print(county_sector_formats$Other$bg)
#   ,
#     print(county_sector_formats[[sect]]$bg)
#   )
# }
# 
# county_sector_formats$Dane$bg

# ORIGINAL CODE

highlight.sector(
  names(which(county_grouping=='Dane')),
  track.index = 1,
  col = "#F6AE2D",
  text = 'DANE COUNTY',
  text.col = '#000000',
  cex = 0.8,
  facing = 'bending.inside',
  niceFacing = T,
  text.vjust = 0.3,
  font = 2
)

highlight.sector(
  names(which(county_grouping=='Madison PL')),
  track.index = 1,
  col = "#005A9C",
  text = 'MADISON PL',
  text.col = '#CCCCCC',
  cex = 0.8,
  facing = 'bending.inside',
  niceFacing = T,
  text.vjust = 0.3,
  font = 2
)

highlight.sector(
  names(which(county_grouping!='Dane' & county_grouping!='Madison PL')),
  track.index = 1,
  col = "#3C3B3B",
  text = 'OTHER',
  text.col = '#FFFFFF',
  cex = 0.8,
  facing = 'bending.inside',
  niceFacing = T,
  text.vjust = 0.3,
  font = 2
)