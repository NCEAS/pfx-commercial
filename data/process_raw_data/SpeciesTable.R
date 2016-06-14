load("cfec.Rdata")

cfec$spec[cfec$spec=="REX "] <- "REX"
cfec$spec[cfec$spec=="POP "] <- "POP"
cfec$spec[cfec$spec==" POP"] <- "POP"

spec.tbl <- distinct(cfec,specn,spec) %>% dplyr::select(specn,spec) %>% arrange(specn)
spec.tbl$spec <- as.character(spec.tbl$spec)

#  For those species with total landed values greater than $1000
#  and for which a spec code is missing, create one.
#  Species letter codes that I've created start with "a"
spec.tbl$spec[spec.tbl$specn==161] <- "aSCLP1"  # Coastrange sculpin
spec.tbl$spec[spec.tbl$specn==163] <- "aSCLP2"  # Fourhorn sculpin
spec.tbl$spec[spec.tbl$specn==207] <- "aGUNN"   # Gunnel
spec.tbl$spec[spec.tbl$specn==208] <- "aWARB"   # Warbonnet, prickleback, etc.
spec.tbl$spec[spec.tbl$specn==214] <- "aGREN"   # Giant grenadier
spec.tbl$spec[spec.tbl$specn==216] <- "aLUMP"   # Smooth lumpsucker
spec.tbl$spec[spec.tbl$specn==250] <- "aTOMC"   # Tomcod
spec.tbl$spec[spec.tbl$specn==260] <- "aFLAT"   # Pacific flatnose
spec.tbl$spec[spec.tbl$specn==400] <- "aUROE"   # Unknown salmon roe
spec.tbl$spec[spec.tbl$specn==401] <- "aKROE"   # Chinook roe
spec.tbl$spec[spec.tbl$specn==402] <- "aSROE"   # Sockeye roe
spec.tbl$spec[spec.tbl$specn==403] <- "aCROE"   # Coho roe
spec.tbl$spec[spec.tbl$specn==404] <- "aPROE"   # Pink roe
spec.tbl$spec[spec.tbl$specn==405] <- "aHROE"   # Chum roe
spec.tbl$spec[spec.tbl$specn==500] <- "aPIKE"   # Northern pike
spec.tbl$spec[spec.tbl$specn==516] <- "CPLN"    # Capelin 
spec.tbl$spec[spec.tbl$specn==520] <- "aCHAR"   # Arctic char
spec.tbl$spec[spec.tbl$specn==530] <- "aDOLL1"  # Dolly varden
spec.tbl$spec[spec.tbl$specn==531] <- "aDOLL2"  # Dolly varden anadromous
spec.tbl$spec[spec.tbl$specn==540] <- "aSTEE"   # Steelhead
spec.tbl$spec[spec.tbl$specn==560] <- "aCUTT"   # Cutthroat
spec.tbl$spec[spec.tbl$specn==570] <- "aINCO"   # Inconnu
spec.tbl$spec[spec.tbl$specn==580] <- "aWHIT"  # Whitefish
spec.tbl$spec[spec.tbl$specn==583] <- "aWHIT2"  # Least cisco
spec.tbl$spec[spec.tbl$specn==584] <- "aWHIT3"  # Arctic cisco
spec.tbl$spec[spec.tbl$specn==585] <- "aWHIT4"  # Bering cisco
spec.tbl$spec[spec.tbl$specn==589] <- "aWHIT5"  # Humpback whitefish
spec.tbl$spec[spec.tbl$specn==590] <- "aGBURB"  # Burbot
spec.tbl$spec[spec.tbl$specn==601] <- "aLAMP"   # Arctic lamprey
spec.tbl$spec[spec.tbl$specn==610] <- "aGRAY"   # Arctic grayling
spec.tbl$spec[spec.tbl$specn==625] <- "aJELL"   # Jellyfish
spec.tbl$spec[spec.tbl$specn==690] <- "aSALM"   # Salmon shark
spec.tbl$spec[spec.tbl$specn==692] <- "aSLEP"   # Sleeper shark
spec.tbl$spec[spec.tbl$specn==700] <- "aOSKT"   # Other skate
spec.tbl$spec[spec.tbl$specn==812] <- "aCLAM"   # Arctic surf clam
spec.tbl$spec[spec.tbl$specn==850] <- "aWSCL"   # Weathervane scallop
spec.tbl$spec[spec.tbl$specn==851] <- "aPSCL"   # Pink scallop
spec.tbl$spec[spec.tbl$specn==890] <- "aSNAL"   # Snail
spec.tbl$spec[spec.tbl$specn==893] <- "aGURC"   # Green urchin
spec.tbl$spec[spec.tbl$specn==933] <- "aTCGR"   # Grooved tanner crab
spec.tbl$spec[spec.tbl$specn==934] <- "aTCTR"   # Triangle tanner crab
spec.tbl$spec[spec.tbl$specn==940] <- "aKHRS"   # Korean horsehair crab
spec.tbl$spec[spec.tbl$specn==953] <- "aVCRB"   # Verrilli crab
spec.tbl$spec[spec.tbl$specn==962] <- "aSSHP"   # Sidestriped shrimp
spec.tbl$spec[spec.tbl$specn==964] <- "aCOOS"   # Coonstriped shrimp

save(list = ls()[which(ls() %in% c("spec.tbl"))], file = "SpeciesTable.RData", envir = .GlobalEnv)
