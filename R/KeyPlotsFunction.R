
KeyPlots <- function(source.dsn, mlra, sitekey, keypolyset, shapefile,
                     keystateset, keystate, weights, species, lpi.tall) {

  if(isTRUE(keypolyset) & isTRUE(keystateset)) stop("Select a single plot subset method, set other to FALSE")

# Add option for, if site key hasn't been read in from csv, build site key
  options(timeout = 4000000)
  # Read in plot info
  plots <- sf::st_read(dsn = source.dsn, layer = "tblPlots")

  if(isTRUE(keypolyset) & isFALSE(keystateset)) {
    shapefile <- sf::st_read(shapefile)
    plots <- sf::st_transform(plots, sf::st_crs(shapefile)) # Check and reproject study area
    modplots <- sf::st_intersection(shapefile, plots) # Clip plots to shapefile
  } else {
      if(isTRUE(keystateset) & isFALSE(keypolyset)) {
        modplots <- subset(plots, plots$State %in% keystate) # Subset by state name
      }
  }


  modplots <- dplyr::select(modplots, PrimaryKey, State,
                            AvgPrecip, AvgPrecipUOM, Elevation, ElevationType) # Keep desired variables
  # Transform UOM for precip (mm to inches) and for elevation (m to ft)
  modplots <- modplots %>%
    dplyr::mutate(AvgPrecip = ifelse(AvgPrecipUOM == "mm", AvgPrecip/25.4, AvgPrecip)) %>%
    dplyr::mutate(Elevation = ifelse(ElevationType == 1, Elevation*3.281, Elevation))
  # Remove geometry
  modplots$SHAPE <- NULL

  # Read in soils
  soil <- terradactyl::gather_soil_horizon(source.dsn, source = "TerrADat")
  soilmod <- subset(soil, soil$PrimaryKey %in% modplots$PrimaryKey)
  # Generate soil depth (in)
  depth <- soilmod %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::slice(which.max(HorizonDepthLower)) %>%
    dplyr::select(PrimaryKey, SoilDepth = HorizonDepthLower) %>%
    dplyr::mutate(SoilDepth = SoilDepth/2.54) # Convert from cm to inches
  depth[2] <- round(depth[2], 1)
  # Surface texture
  plotsurftext <- soilmod %>%
    dplyr::filter(HorizonNumber == 1) %>%
    dplyr::select(PrimaryKey, Texture, TextureModifier)
  # Standardize modifiers (evenually will not need, when they become standardized in terradactyl)
  plotsurftext$TextureModifier <- tolower(plotsurftext$TextureModifier)
  unique(plotsurftext$TextureModifier)
  plotsurftext <- dplyr::mutate(plotsurftext, ModCode = ifelse(TextureModifier == "gravelly" | TextureModifier == "gr", "GR",
                                                                                      ifelse(TextureModifier  == "fine gravelly", "GR",
                                                                                             ifelse(TextureModifier  == "medium gravelly", "GR",
                                                                                                    ifelse(TextureModifier  == "coarse gravelly", "GR",
                                                                                                           ifelse(TextureModifier  == "very gravelly" | TextureModifier == "grv" | TextureModifier == "vgr", "VGR",
                                                                                                                  ifelse(TextureModifier  == "extremely gravelly" | TextureModifier == "grx" | TextureModifier == "xgr", "XGR",
                                                                                                                         ifelse(TextureModifier  == "cobbly" | TextureModifier == "cb", "CB",
                                                                                                                                ifelse(TextureModifier  == "very cobbly", "VCB",
                                                                                                                                       ifelse(TextureModifier  == "extremely cobbly" | TextureModifier == "extrememly cobb", "XCB",
                                                                                                                                              ifelse(TextureModifier == "stony" | TextureModifier == "gravelly/stony", "ST",
                                                                                                                                                     ifelse(TextureModifier  == "very stony", "VST",
                                                                                                                                                            ifelse(TextureModifier  == "extremely stony", "XST",
                                                                                                                                                                   ifelse(TextureModifier  == "bouldery", "BY",
                                                                                                                                                                          ifelse(TextureModifier  == "extremely bouldery", "XBY",
                                                                                                                                                                                 ifelse(TextureModifier  == "channery", "CN",
                                                                                                                                                                                        ifelse(TextureModifier  == "very channery", "VCN",
                                                                                                                                                                                               ifelse(TextureModifier  == "extrememly channery", "XCN",
                                                                                                                                                                                                      ifelse(TextureModifier  == "flaggy", "FL",
                                                                                                                                                                                                             ifelse(TextureModifier  == "very flaggy", "VFL",
                                                                                                                                                                                                                    ifelse(TextureModifier  == "extremely flaggy", "XFL", NA)))))))))))))))))))))
  plotsurftext <- tidyr::unite(plotsurftext, SurfaceTextures, ModCode, Texture, sep = " ", remove = TRUE, na.rm = TRUE)
  plotsurftext <- plotsurftext[, c(1, 2)]
  plotsurftext$SurfaceTextures <- toupper(plotsurftext$SurfaceTextures)
  # Surface gravels and larger fragments
  plotsurffrags <- soilmod %>%
    dplyr::filter(HorizonNumber == 1) %>%
    dplyr::select(PrimaryKey, SurfaceFrags = RockFragments)

  # Subsurface fragments
  plotssfrags <- soilmod %>%
    dplyr::filter(HorizonNumber > 1) %>% # Plots will drop if there's only one HZ present
    dplyr::select(PrimaryKey, HorizonNumber, HorizonDepthUpper, HorizonDepthLower, RockFragments)

  # Convert to ss weighted fragments
  plotssfrags$hzdepb_in <- plotssfrags$HorizonDepthLower/2.54
  plotssfrags$hzdept_in <- plotssfrags$HorizonDepthUpper/2.54
  # All fragments
  plotssfr <- plotssfrags %>%
    dplyr::left_join(depth) %>%
    dplyr::mutate(HZthickness = hzdepb_in - hzdept_in) %>%
    dplyr::mutate(ssfragmult = RockFragments * HZthickness) %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::mutate(SubsurfFrags = sum(ssfragmult)/SoilDepth) %>%
    dplyr::select(PrimaryKey, SubsurfFrags)
  plotssfr <- dplyr::distinct(plotssfr)
  plotssfr[2] <- round(plotssfr[2], 0)
  plotssfr <- dplyr::mutate_if(plotssfr, is.numeric, ~replace(., is.na(.), 0))
  # Calculate simplified PSCs
  # First QC ClayPct and replace incorrect ClayPct with ClayEst (midpoint of texture triangle space)
  particlepct <- soilmod %>%
    dplyr::select(PrimaryKey, HorizonDepthUpper, HorizonDepthLower, Texture, ClayPct, RockFragments) %>%
    dplyr::mutate(ClayQC = ifelse(Texture=="COS" | Texture=="FS" | Texture=="S" | Texture=="VFS" & ClayPct < 11, "Yes",
                                  ifelse(Texture=="LCOS" | Texture=="LFS" | Texture=="LS" | Texture=="LVFS" & ClayPct > 9 & ClayPct < 16, "Yes",
                                         ifelse(Texture=="COSL" | Texture=="FSL" | Texture=="SL" | Texture=="VFSL" & ClayPct > 14 & ClayPct < 21, "Yes",
                                                ifelse(Texture=="SCL" & ClayPct > 19 & ClayPct < 36, "Yes",
                                                       ifelse(Texture=="SC" & ClayPct > 34 & ClayPct < 56, "Yes",
                                                              ifelse(Texture=="L" & ClayPct > 6 & ClayPct < 29, "Yes",
                                                                     ifelse(Texture=="CL" | Texture=="SICL" & ClayPct > 26 & ClayPct < 41, "Yes",
                                                                            ifelse(Texture=="SIL" & ClayPct < 29, "Yes",
                                                                                   ifelse(Texture=="C" & ClayPct > 39 & ClayPct < 101, "Yes",
                                                                                          ifelse(Texture=="SI" & ClayPct < 13, "Yes",
                                                                                                 ifelse(Texture=="SIC" & ClayPct > 39 & ClayPct < 61, "Yes", "No"))))))))))))
 # Replace incorrect ClayPct with NA
  particlepct$ClayPct <- ifelse(particlepct$ClayQC %in% c("No", NA), NA, particlepct$ClayPct)
 # Replace NA with ClayEst, add SandEst, and multiply sand/clay by horizon thickness
  particlepct <- dplyr::mutate(particlepct, ClayEst = ifelse(Texture =="COS" | Texture=="FS" | Texture=="S" | Texture=="VFS", 4,
                                    ifelse(Texture=="LCOS" | Texture=="LFS" | Texture=="LS" | Texture=="LVFS" | Texture=="SI", 6,
                                           ifelse(Texture=="COSL" | Texture=="FSL" | Texture=="SL" | Texture=="VFSL", 10,
                                                  ifelse(Texture=="SIL", 14,
                                                         ifelse(Texture=="L", 19,
                                                                ifelse(Texture=="SCL", 27,
                                                                       ifelse(Texture=="CL" | Texture=="SICL", 34,
                                                                              ifelse(Texture=="SC", 42,
                                                                                     ifelse(Texture=="SIC", 47,
                                                                                            ifelse(Texture=="C", 63, NA))))))))))) %>%
    dplyr::mutate(ClayPct = ifelse(is.na(ClayPct), ClayEst, ClayPct)) %>%
    dplyr::mutate(SandEst = ifelse(Texture=="SI" | Texture=="SIC", 7,
                                                              ifelse(Texture=="SICL", 10,
                                                                     ifelse(Texture=="C", 20,
                                                                            ifelse(Texture=="SIL", 21,
                                                                                   ifelse(Texture=="CL", 32,
                                                                                          ifelse(Texture=="L", 41,
                                                                                                 ifelse(Texture=="SC", 51,
                                                                                                        ifelse(Texture=="SCL", 59,
                                                                                                               ifelse(Texture=="COSL" | Texture=="FSL" | Texture=="SL" | Texture=="VFSL", 65,
                                                                                                                      ifelse(Texture=="LCOS" | Texture=="LFS" | Texture=="LS" | Texture=="LVFS", 82,
                                                                                                                             ifelse(Texture=="COS" | Texture=="FS" | Texture=="S" | Texture=="VFS", 91, NA)))))))))))) %>%
    dplyr::mutate(HorizonDepthLower = HorizonDepthLower/2.54) %>%
    dplyr::mutate(HorizonDepthUpper = HorizonDepthUpper/2.54) %>%
    dplyr::mutate(HorizonThickness = HorizonDepthLower - HorizonDepthUpper)

  particlepct <- particlepct %>%
    dplyr::mutate(ClayMult = ClayPct * HorizonThickness,
                                 SandMult = SandEst * HorizonThickness,
                                 FragMult = RockFragments * HorizonThickness) %>%
    dplyr::select(PrimaryKey, HorizonThickness, RockFragments, ClayPct, SandEst, ClayMult, SandMult, FragMult)
 # Join table with total soil pit depth
  particlepct <- dplyr::left_join(particlepct, depth)
  # Calculate weighted clay, sand, and fragments
   particlepct <- particlepct %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::mutate(ClaySum = sum(ClayMult),
                  SandSum = sum(SandMult),
                  FragSum = sum(FragMult))
  particlepct <- particlepct %>%
    dplyr::mutate(WeightedClay = ClaySum/SoilDepth,
                  WeightedSand = SandSum/SoilDepth,
                  WeightedFrags = FragSum/SoilDepth)
   # Round up to whole number
   particlepct[13:15] <- round(particlepct[13:15], 0)
   # Subset to important variables, and collapse...we want just one value per plot
   psc.esds <- dplyr::select(particlepct, PrimaryKey, WeightedClay, WeightedSand, WeightedFrags)
   psc.esds <- dplyr::distinct(psc.esds)
   # Assign simple PSCs
   psc.esds <- psc.esds %>%
     dplyr::mutate(PSC.ESD = ifelse(WeightedClay < 16 & WeightedSand > 69, "sandy",
                             ifelse(WeightedClay < 36 & WeightedSand < 70, "loamy",
                                    ifelse(WeightedClay > 35, "clayey", "unclassified"))))
   # Some are unclassified due to clay/sand estimates throwing off boundaries of PSCs
   # Replace with NA
   psc.esds <- dplyr::mutate(psc.esds, PSC.ESD = ifelse(PSC.ESD == "unclassified", NA, PSC.ESD))
   # Keep desired variables
   psc.esds <- dplyr::select(psc.esds, PrimaryKey, PSC = PSC.ESD)

   # Read in species data from tall table
   if(isTRUE(species)) {
   lpi.tall <- subset(lpi.tall, lpi.tall$PrimaryKey %in% modplots$PrimaryKey)
   lpi.species <- terradactyl::pct_cover_species(lpi_tall = lpi.tall, tall = TRUE, by_line = FALSE, hit = "any")
   lpi.species <- dplyr::filter(lpi.species, percent > 0)
   lpi.species.slice <- lpi.species %>%
     dplyr::group_by(PrimaryKey) %>%
     dplyr::arrange(desc(percent)) %>%
     dplyr::slice(1:5) %>%
     dplyr::mutate(id = row_number()) %>%
     dplyr::ungroup()
   rm(lpi.tall)
   lpi.wide <- lpi.species.slice %>%
     dplyr::select(1, 2, 4) %>%
     tidyr::spread(key = id, value = Species)
   rm(lpi.species.slice)
   lpi.wide <- tidyr::unite(lpi.wide, col = "Species", 2:6, sep = ",")
   lpi.wide$Property <- "Species"
   lpi.wide <- dplyr::select(lpi.wide, PrimaryKey, Property, PlotValue = Species)
   } else {print("Species set to FALSE")}


  # Format quantitative plot data
  quantplots <- dplyr::select(modplots, PrimaryKey, AvgPrecip, Elevation)
  quantplots <- quantplots %>%
    dplyr::left_join(depth) %>%
    dplyr::left_join(plotssfr) %>%
    dplyr::left_join(plotsurffrags)

  # Format nominal plot data
  nomplots <- dplyr::left_join(psc.esds, plotsurftext)

  # Gather
  quantplots <- tidyr::gather(quantplots, key = "Property", value = "PlotValue", 2:6)
  nomplots <- tidyr::gather(nomplots, key = "Property", value = "PlotValue", 2:3)

  # Filter out NAs
  quantplotsclean <- na.omit(quantplots)
  nomplotsclean <- na.omit(nomplots)
  # Plot value should be character type to accomodate quantitative and nominal attributes
  quantplotsclean$PlotValue <- as.character(quantplotsclean$PlotValue)

  # Sum sitekey fragments by size classes into total rock fragments by surface and subsurface
  # Because not all AIM projects differentiate fragment size classes
  # Surface fragments
  surfacefrags <- dplyr::filter(sitekey, Property == "SurfaceGravel" | Property == "SurfaceLGFrags")
  surfacefrags$representativeLow <- as.numeric(surfacefrags$representativeLow)
  surfacefrags$representativeHigh <- as.numeric(surfacefrags$representativeHigh)
  surfacefragslow <- surfacefrags %>%
    dplyr::group_by(siteid) %>%
    dplyr::select(siteid, representativeLow) %>%
    dplyr::summarise(representativeLow = sum(representativeLow))
  surfacefragshigh <- surfacefrags %>%
    dplyr::group_by(siteid) %>%
    dplyr::summarise(representativeHigh = sum(representativeHigh))
  surfacefrags <- dplyr::left_join(surfacefragslow, surfacefragshigh)
  surfacefrags$PropertyLow <- "SurfaceFragsLow"
  surfacefrags$Lower.Relation <- ">="
  surfacefrags$PropertyHigh <- "SurfaceFragsHigh"
  surfacefrags$Upper.Relation <- "<="
  surfacefrags$Property <- "SurfaceFrags"
  surfacefrags <- dplyr::select(surfacefrags, siteid, PropertyLow, Lower.Relation, representativeLow,
                                PropertyHigh, Upper.Relation, representativeHigh, Property)
  # Subsurfacefragments
  subsurfacefrags <- dplyr::filter(sitekey, Property == "SubsurfGR" | Property == "SubsurfLGFrags")
  subsurfacefrags$representativeLow <- as.numeric(subsurfacefrags$representativeLow)
  subsurfacefrags$representativeHigh <- as.numeric(subsurfacefrags$representativeHigh)
  subsurfacefragslow <- subsurfacefrags %>%
    dplyr::group_by(siteid) %>%
    dplyr::select(siteid, representativeLow) %>%
    dplyr::summarise(representativeLow = sum(representativeLow))
  subsurfacefragshigh <- subsurfacefrags %>%
    dplyr::group_by(siteid) %>%
    dplyr::summarise(representativeHigh = sum(representativeHigh))
  subsurfacefrags <- dplyr::left_join(subsurfacefragslow, subsurfacefragshigh)
  subsurfacefrags$PropertyLow <- "SubsurfFragsLow"
  subsurfacefrags$Lower.Relation <- ">="
  subsurfacefrags$PropertyHigh <- "SubsurfFragsHigh"
  subsurfacefrags$Upper.Relation <- "<="
  subsurfacefrags$Property <- "SubsurfFrags"
  subsurfacefrags <- dplyr::select(subsurfacefrags, siteid, PropertyLow, Lower.Relation, representativeLow,
                                PropertyHigh, Upper.Relation, representativeHigh, Property)
  # Join surface and subsurface fragment sitekey tables
  frags <- rbind(surfacefrags, subsurfacefrags)
  # Remove fragments by size classes from key and summed rock fragments
  sitekey <- dplyr::filter(sitekey, Property != "SubsurfGravel" & Property!= "SubsurfLGFrags" &
                             Property != "SurfaceGravel" & Property != "SurfaceLGFrags")
  sitekey <- rbind(sitekey, frags)


  # Run quantkeying
  QSiteKey <- dplyr::filter(sitekey, Property != "SurfaceTextures" & Property != "PSC")
  quantjoined <- dplyr::full_join(quantplotsclean, QSiteKey) # Many NAs form during this step
  # Not sure why...the quantplotsclean columns are occupied, but they don't join with the key table
  # Pull unique values from Key (the upper and lower limits that define key logic)
  uppervals <- unique(quantjoined$representativeLow)
  lowervals <- unique(quantjoined$representativeHigh)
  # Paste plot values into evaluation criteria columns
  QKey_Conditional_Paste <- quantjoined %>% dplyr::mutate(Eval_Lower = paste(PlotValue, Lower.Relation, representativeLow), Eval_Upper = paste(PlotValue, Upper.Relation, representativeHigh))
  # Remove NAs formed in join
  # This also removes nominal site key entires
  QKey_Conditional_Paste <- na.omit(QKey_Conditional_Paste)
  # Make vector of evaluation types (not sure if this step is necessary...)
  Qkey_eval_vars <- names(QKey_Conditional_Paste)[grep(names(QKey_Conditional_Paste) , pattern = "^Eval_")]
  # Apply key logic (if doesn't run, there might be an NA in eva_lower or eval_upper where should be a > or <)
  Qkey_benchmark_vector <- sapply(X = 1:nrow(QKey_Conditional_Paste),
                                 data = QKey_Conditional_Paste,
                                 Qkey_eval_vars = Qkey_eval_vars,
                                 FUN = function(X, data, Qkey_eval_vars){
                                   all(sapply(X = Qkey_eval_vars,
                                              data = data[X, ],
                                              FUN = function(X, data){
                                                evalstring <- data[[X]]
                                                eval(parse(text = evalstring))
                                              }))
                                 })
  QKey_Conditional_Paste$benchmark_vector <- Qkey_benchmark_vector

  # This summary will rank the likelihood of a plot belonging to a certain state based on proportion of criteria met
  # This is where we want to incorporate fuzzy sets
  QSummary2 <- QKey_Conditional_Paste %>%
    dplyr::group_by(PrimaryKey, siteid) %>%
    dplyr::summarize(ProportionCriteriaMet = sum(benchmark_vector)/length(benchmark_vector))

  # Incorporate nominal key results for more criteria (though we actually want this to be before
  # QSummary2 so it can be included)
  NomSiteKey <- SiteKey %>%
    dplyr::select(siteid:representativeLow, Property) %>%
    dplyr::filter(Property == "SurfaceTextures" | Property == "PSC")
  # Look for matches in PSCs and textures
  match_list <- apply(X = nomplotsclean,
                         strings = NomSiteKey,
                         MARGIN = 1,
                         FUN = function(X, strings){
                           current_row <- X
                           current_string1 <- current_row[["PlotValue"]]

                           st_matches <- sapply(X = strings$representativeLow,
                                                current_string1 = current_string1,

                                                FUN = function(X, current_string1) {

                                                  current_string1 <- trimws(unlist(current_string1))
                                                  stringies <- trimws(unlist(stringr::str_split(X, pattern = ",")))

                                                  any(current_string1 %in% stringies)
                                                })
                           data.frame(PrimaryKey = current_row[["PrimaryKey"]],
                                      Property = current_row[["Property"]],
                                      PlotValue = current_row[["PlotValue"]],
                                      siteid = strings$siteid,
                                      PropertyLow = strings$PropertyLow,
                                      Lower.Relation = strings$Lower.Relation,
                                      representativeLow = strings$representativeLow,
                                      PropertyHigh = NA,
                                      Upper.Relation = NA,
                                      representativeHigh = NA,
                                      Eval_Lower = NA,
                                      Eval_Upper = NA,
                                      benchmark_vector = st_matches,
                                      stringsAsFactors = FALSE)
                         })


  results <- do.call(rbind,
                        match_list)


  # Filter for mismatches between Property and PropertyLow
  results <- dplyr::filter(results, Property == PropertyLow)
  results <- results %>%
    tidyr::unite(Eval_Lower, PlotValue, Lower.Relation, representativeLow, sep = "", remove = FALSE) %>%
    dplyr::select(PrimaryKey, Property, PlotValue, siteid, PropertyLow, Lower.Relation,
                  representativeLow, PropertyHigh, Upper.Relation, representativeHigh,
                  Eval_Lower, Eval_Upper, benchmark_vector)


  # Match species
  if(isTRUE(species)) {
  speciesmatch <- SiteKey %>%
    dplyr::select(siteid:representativeLow, Property) %>%
    dplyr::filter(Property == "Species")
  spmatch_list <- apply(X = lpi.wide,
                      strings = speciesmatch,
                      MARGIN = 1,
                      FUN = function(X, strings){
                        current_row <- X
                        current_string1 <- current_row[["PlotValue"]]

                        st_matches <- sapply(X = strings$representativeLow,
                                             current_string1 = current_string1,

                                             FUN = function(X, current_string1) {

                                               current_string1 <- trimws(unlist(stringr::str_split(current_string1, pattern = ",")))
                                               stringies <- trimws(unlist(stringr::str_split(X, pattern = ",")))

                                               any(current_string1 %in% stringies)
                                             })
                        data.frame(PrimaryKey = current_row[["PrimaryKey"]],
                                   Property = current_row[["Property"]],
                                   PlotValue = current_row[["PlotValue"]],
                                   siteid = strings$siteid,
                                   PropertyLow = strings$PropertyLow,
                                   Lower.Relation = strings$Lower.Relation,
                                   representativeLow = strings$representativeLow,
                                   PropertyHigh = NA,
                                   Upper.Relation = NA,
                                   representativeHigh = NA,
                                   Eval_Lower = NA,
                                   Eval_Upper = NA,
                                   benchmark_vector = st_matches,
                                   stringsAsFactors = FALSE)
                      })


  spresults <- do.call(rbind,
                     spmatch_list)
  rownames(spresults) <- NULL

  # Filter for mismatches between Property and PropertyLow
  spresults <- dplyr::filter(spresults, Property == PropertyLow)
  spresults <- spresults %>%
    tidyr::unite(Eval_Lower, PlotValue, Lower.Relation, representativeLow, sep = "", remove = FALSE) %>%
    dplyr::select(PrimaryKey, Property, PlotValue, siteid, PropertyLow, Lower.Relation,
                  representativeLow, PropertyHigh, Upper.Relation, representativeHigh,
                  Eval_Lower, Eval_Upper, benchmark_vector)
  }




  # Join results of quantitative and nominal keys
  if(isTRUE(species)) {
  Key_Conditional_Paste <- rbind(QKey_Conditional_Paste, results)
  Key_Conditional_Paste <- rbind(Key_Conditional_Paste, spresults)
  } else {
    if(isFALSE(species)) {
      Key_Conditional_Paste <- rbind(QKey_Conditional_Paste, results)
    }
  }

  # Add variable weights
  # Convert T/F to integers
  Key_Conditional_Paste$benchmark_vector_int <- as.integer(Key_Conditional_Paste$benchmark_vector)
  # Weight by properties in pre-specified groups
  if(isTRUE(weights)) {
  Key_Conditional_Paste <- Key_Conditional_Paste %>%
    dplyr::mutate(benchmark_weighted = ifelse(Property %in% two, benchmark_vector_int * 2,
                                              ifelse(Property %in% three, benchmark_vector_int * 3,
                                                     benchmark_vector_int)))
  QSummary <- Key_Conditional_Paste %>%
    dplyr::group_by(PrimaryKey, siteid) %>%
    dplyr::summarize(ProportionCriteriaMetUn = sum(benchmark_weighted)/length(benchmark_weighted)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ProportionCriteriaMet = ProportionCriteriaMetUn/max(ProportionCriteriaMetUn)) %>%
    dplyr::select(-ProportionCriteriaMetUn)
  } else {
    if(isFALSE(weights)) {
  QSummary <- Key_Conditional_Paste %>%
        dplyr::group_by(PrimaryKey, siteid) %>%
        dplyr::summarize(ProportionCriteriaMet = sum(benchmark_vector_int)/length(benchmark_vector_int)) %>%
        dplyr::ungroup()
    }
  }

  # A ranking of the most likely sites
  # This is sort of the final output
  Summary_Rank_Top_Slice <- QSummary %>%
    dplyr::group_by(PrimaryKey) %>%
    dplyr::slice(which.max(ProportionCriteriaMet))
  # Keep distinct
  Summary_Rank_Top_Slice <- dplyr::distinct(Summary_Rank_Top_Slice)

  # What criteria were met for each best match of plot and site?
  CriteriaSummary <- Summary_Rank_Top_Slice %>%
    dplyr::left_join(Key_Conditional_Paste) %>%
    dplyr::select(PrimaryKey, siteid, ProportionCriteriaMet, Property, PlotValue,
                  Eval_Lower, Eval_Upper, benchmark_vector)

  # Write outputs to csvs
    write.csv(Summary_Rank_Top_Slice, paste(Sys.Date(), "PlotsKeyed_MLRA", mlra, ".csv", sep = ""), row.names = FALSE)


    return(CriteriaSummary)

}


