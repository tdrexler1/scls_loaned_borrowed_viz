county_sector_formats <- list(
  Dane = c(list(bg="#F6AE2D", txt="#000000")),
  "Madison PL" = c(list(bg="#005A9C", txt="#CCCCCC")),
  Other = c(list(bg="#3C3B3B", txt="#FFFFFF"))
)
county_sector_formats
unique(county_grouping)

highlight_counties <- c("Dane", "Madison PL")
other_lib <- names(county_grouping[which(!county_grouping %in% highlight_counties)])

#other_group <- which((county_grouping!="Madison PL" & county_grouping!="Dane"))

highlight_groups <- 
  unique(
    replace(
      county_grouping, 
      other_group,
      "Other"
      )
  )

for (sect in highlight_groups){
  print(sect)
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

for (sect in unique(county_grouping)){
  print(sect)
  ifelse ((sect!="Dane" & sect!="Madison PL"),
    print(county_sector_formats$Other$bg)
  ,
    print(county_sector_formats[[sect]]$bg)
  )
}

county_sector_formats$Dane$bg

