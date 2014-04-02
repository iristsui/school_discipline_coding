SaveImage <-
function() {
  root1 <- "/Users/HumanImpactPartners/Documents/hias/school_discipline_hia"
  root2 <- "/data/workspaces/"
  save.image(paste(root1, root2, "pbis_uncleaned_data.RData", sep=""))
}

