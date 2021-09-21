# River structure RData
rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))

# Read in basin name - endcell mapping
#read.csv(system.file("extdata/RiverBasinNames.csv", package = "mrwaterPlots"))
basinMap <- read.csv("C:\\Users\\beier\\Documents\\Modelle\\PIK_PIAM\\mrwaterPlots\\inst\\extdata\\RiverBasinNames.csv")

# Create object with selected basins
rs$basins <- numeric(67420)

p <- NULL
k <- NULL
i <- 0
for (b in basinMap$Basin.Name) {
  i <- i + 1
  c <- basinMap$Endcell[basinMap$Basin.Name == b]
  rs$basins[rs$endcell == c] <- i
  tmp       <- numeric(67420)
  tmp[rs$endcell == c] <- 1
  p[[i]] <- plotmap2(toolLPJcell2MAgPIEcell(as.magpie(rs$basins, spatial = 1)))
  k[[i]] <- plotmap2(toolLPJcell2MAgPIEcell(as.magpie(tmp, spatial = 1)))

}

