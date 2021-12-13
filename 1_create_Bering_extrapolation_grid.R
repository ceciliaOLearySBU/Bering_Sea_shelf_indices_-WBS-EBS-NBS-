library(FishStatsUtils)
####this code now runs with FishStatusUtils-2.

working_directory <- getwd()
##load file with lat/lon of extrapolation area for WBS to combine with EBS & NBS
grid2 <- readRDS(file = paste0(working_directory,"/grid2_WBS.rds"))

zone <- 31

# Extract strata boundaries by region for WBS
WBS_boundaries = c(range(grid2$Lon), range(grid2$Lat))
grid2$STRATA <- "WBS"

grid2 <- data.frame(Lon=grid2$Lon,Lat=grid2$Lat,Area_km2=grid2$Area_km2,STRATA = grid2$X1.nrow.grid1.) 
Region="User"
wbs.strata.limits <- data.frame(
  'STRATA' = c("All_areas"), #c("WBS"), 
  'west_border' = c(WBS_boundaries[1]),
  'east_border' = c(WBS_boundaries[2]),
  'north_border' = c(WBS_boundaries[4]),
  'south_border' = c(WBS_boundaries[3]) )

strata.limits <- data.frame('STRATA'="All_areas")
WBS_extrap =  FishStatsUtils::make_extrapolation_info(Region=Region,
                                                strata.limits=wbs.strata.limits,
                                                observations_LL = grid2[,c("Lon","Lat")],
                                                flip_around_dateline=TRUE,
                                                #zone = 32, #2
                                                input_grid = grid2, 
                                                #origargs = "+proj=longlat +ellps=WGS84",
                                                projargs="+proj=longlat +ellps=WGS84",
                                                max_cells = 4000,  #previously 5000
                                                knot_method = "grid")


## combining multiple extrapolation grids in the bering sea
#Eastern Bering Sea
EBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Eastern_Bering_Sea", 
                                                      #zone = zone,
                                                      strata.limits=strata.limits,
                                                      #origargs = "+proj=longlat +ellps=WGS84",
                                                      #zone = 32, #2
                                                      projargs="+proj=longlat +ellps=WGS84",
                                                      max_cells = 4000, #previously 5000
                                                      knot_method = "grid")
#northern Bering Sea
NBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Northern_Bering_Sea", 
                                                      flip_around_dateline=TRUE,
                                                      #zone = 32, #2
                                                      strata.limits=strata.limits,
                                                      #origargs = "+proj=longlat +ellps=WGS84",
                                                      projargs="+proj=longlat +ellps=WGS84",
                                                      max_cells = 4000,  #previously 5000
                                                      knot_method = "grid" )
Extrapolation_List = FishStatsUtils::combine_extrapolation_info( "EBS"=EBS_extrap, "NBS"=NBS_extrap, "WBS" = WBS_extrap)

# Add strata together for list of total areas in each subregion
Extrapolation_List$a_el = cbind( "All"=Extrapolation_List$a_el[,1], 
                                 "EBS"=c(EBS_extrap$a_el[,1],rep(0,nrow(NBS_extrap$a_el)),rep(0,nrow(WBS_extrap$a_el))), 
                                 "NBS"=c(rep(0,nrow(EBS_extrap$a_el)),NBS_extrap$a_el[,1],rep(0,nrow(WBS_extrap$a_el))), 
                                 "WBS" =c(rep(0,nrow(EBS_extrap$a_el)),rep(0,nrow(NBS_extrap$a_el)),WBS_extrap$a_el[,1]) )

# Double check each extrapolation subregion sums to the correct total area
colSums( Extrapolation_List$a_el )
saveRDS(Extrapolation_List, file = paste0(working_directory,"/Extrapolation_List.rds"))
