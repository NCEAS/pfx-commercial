
setwd("/users/eric.ward/downloads/cfec_early")

for(y in 1975:1984) {

early = haven::read_sas(paste0("gross_earnings_",y,".sas7bdat"))

early$F_PERMIT = paste0(early$ADFG_H_PERMIT_FISHERY,early$ADFG_H_PERMIT_SERIAL_NUMBER,early$ADFG_H_PERMIT_CHECK_DIGIT)

early$startdt = paste0(substr(early$ADFG_H_DATE_FISHING_BEGAN,1,4), "-",
  substr(early$ADFG_H_DATE_FISHING_BEGAN,5,6), "-",
  substr(early$ADFG_H_DATE_FISHING_BEGAN,7,8))

early$landdate = paste0(substr(early$ADFG_H_DATE_LANDED,1,4), "-",
  substr(early$ADFG_H_DATE_LANDED,5,6), "-",
  substr(early$ADFG_H_DATE_LANDED,7,8))

early = rename(early, F_INPFC=ADFG_H_CDQ_CODE,
  F_GEAR = ADFG_H_GEAR_CODE,
  P_TYPE = CFEC_PMT_TYPE,
  P_STATUS = CFEC_PMT_ID_STATUS,
  A_RES = CFEC_ADR_RESIDENCY,
  C_LANDST = CFEC_LANDING_STATUS,
  F_DISPOS = ADFG_I_DISPOSITION_CODE,
  F_MPGMID = ADFG_H_MGT_PROGRAM_ID,
  F_DELIVR = ADFG_I_DELIVERY_CODE)

early = early[,c("ADFG_B_BATCH_YEAR","ADFG_H_PORT_CODE","CFEC_FILE_NUMBER",
  "CFEC_PMT_SERIAL","CFEC_VALUE","ADFG_I_POUNDS","ADFG_H_SEQ_TICKET_NUMBER",
  "ADFG_H_ADFG_NUMBER","CFEC_CORRECTED_ADFG","ADFG_I_HARVEST_CODE","ADFG_H_PROCESSOR_CODE",
  "CFEC_SPECIES_CODE","CFEC_STAT_AREA","ADFG_I_SPECIES_CODE","CFEC_PMT_FSHY",
  "CFEC_PMT_CHECK","F_INPFC","F_GEAR","P_TYPE","P_STATUS","A_RES","C_LANDST",
  "F_DISPOS","F_MPGMID","F_PERMIT","startdt","landdate","F_DELIVR")]

early = rename(early, year=ADFG_B_BATCH_YEAR,
  port = ADFG_H_PORT_CODE,
  p_holder = CFEC_FILE_NUMBER,
  p_serial = CFEC_PMT_SERIAL,
  g_earn = CFEC_VALUE,
  g_pounds = ADFG_I_POUNDS,
  f_ticket = ADFG_H_SEQ_TICKET_NUMBER,
  adfg = ADFG_H_ADFG_NUMBER,
  cadfg = CFEC_CORRECTED_ADFG,
  harvest = ADFG_I_HARVEST_CODE,
  procid = ADFG_H_PROCESSOR_CODE,
  g_spcs = CFEC_SPECIES_CODE,
  stat6 = CFEC_STAT_AREA,
  specn = ADFG_I_SPECIES_CODE,
  p_fshy = CFEC_PMT_FSHY,
  p_check = CFEC_PMT_CHECK)

early$g_price=ifelse(early$g_pounds>0,early$g_earn/early$g_pounds,0)

#early = early[early$p_fshy%in%c("S 01E", "S 03A", "S 01A", "S 03E", "S 01K", "S 01M"),]

#early = group_by(early, p_holder) %>%
#  mutate(npermit = length(unique(p_fshy))) %>%
#  filter(npermit == 1) %>%
#  select(-npermit) %>%
#  filter(p_fshy %in% c("S 01E", "S 03A", "S 01A", "S 03E", "S 01K", "S 01M"))

saveRDS(early, paste0("processed",y,".rds"))

}

# combine years into single file
dat = readRDS(paste0("processed",1975,".rds"))
for(y in 1976:1984) {
  tmp = readRDS(paste0("processed",y,".rds"))

  dat = rbind(dat, tmp)
}
saveRDS(dat, paste0("processed","_allYears",".rds"))

# Process 2015 data
#dat_2015 = haven::read_sas(paste0("/users/eric.ward/documents/CFEC/data/ge_short_column_names_2015.sas7bdat"))
dat_2015 = haven::read_sas(paste0("/users/eric.ward/documents/CFEC/data/ge_long_column_names_2015.sas7bdat"))

dat_2015$F_PERMIT = paste0(dat_2015$ADFG_H_PERMIT_FISHERY,dat_2015$ADFG_H_PERMIT_SERIAL_NUMBER,dat_2015$ADFG_H_PERMIT_CHECK_DIGIT)

dat_2015$startdt = paste0(substr(dat_2015$ADFG_H_DATE_FISHING_BEGAN,1,4), "-",
  substr(dat_2015$ADFG_H_DATE_FISHING_BEGAN,5,6), "-",
  substr(dat_2015$ADFG_H_DATE_FISHING_BEGAN,7,8))

dat_2015$landdate = paste0(substr(dat_2015$ADFG_H_DATE_LANDED,1,4), "-",
  substr(dat_2015$ADFG_H_DATE_LANDED,5,6), "-",
  substr(dat_2015$ADFG_H_DATE_LANDED,7,8))

dat_2015 = dplyr::rename(dat_2015, F_INPFC=ADFG_H_CDQ_CODE,
  F_GEAR = ADFG_H_GEAR_CODE,
  P_TYPE = CFEC_PMT_TYPE,
  P_STATUS = CFEC_PMT_ID_STATUS,
  A_RES = CFEC_ADR_RESIDENCY,
  C_LANDST = CFEC_LANDING_STATUS,
  F_DISPOS = ADFG_I_DISPOSITION_CODE,
  F_MPGMID = ADFG_H_MGT_PROGRAM_ID,
  F_DELIVR = ADFG_I_DELIVERY_CODE)

dat_2015 = dat_2015[,c("ADFG_B_BATCH_YEAR","ADFG_H_PORT_CODE","CFEC_FILE_NUMBER",
  "CFEC_PMT_SERIAL","CFEC_VALUE","ADFG_I_POUNDS","ADFG_H_SEQ_TICKET_NUMBER",
  "ADFG_H_ADFG_NUMBER","CFEC_CORRECTED_ADFG","ADFG_I_HARVEST_CODE","ADFG_H_PROCESSOR_CODE",
  "CFEC_SPECIES_CODE","CFEC_STAT_AREA","ADFG_I_SPECIES_CODE","CFEC_PMT_FSHY",
  "CFEC_PMT_CHECK","F_INPFC","F_GEAR","P_TYPE","P_STATUS","A_RES","C_LANDST",
  "F_DISPOS","F_MPGMID","F_PERMIT","startdt","landdate","F_DELIVR")]

dat_2015 = dplyr::rename(dat_2015, year=ADFG_B_BATCH_YEAR,
  port = ADFG_H_PORT_CODE,
  p_holder = CFEC_FILE_NUMBER,
  p_serial = CFEC_PMT_SERIAL,
  g_earn = CFEC_VALUE,
  g_pounds = ADFG_I_POUNDS,
  f_ticket = ADFG_H_SEQ_TICKET_NUMBER,
  adfg = ADFG_H_ADFG_NUMBER,
  cadfg = CFEC_CORRECTED_ADFG,
  harvest = ADFG_I_HARVEST_CODE,
  procid = ADFG_H_PROCESSOR_CODE,
  g_spcs = CFEC_SPECIES_CODE,
  stat6 = CFEC_STAT_AREA,
  specn = ADFG_I_SPECIES_CODE,
  p_fshy = CFEC_PMT_FSHY,
  p_check = CFEC_PMT_CHECK)

dat_2015$g_price=ifelse(dat_2015$g_pounds>0,dat_2015$g_earn/dat_2015$g_pounds,0)

saveRDS(dat_2015, paste0("/users/eric.ward/documents/CFEC/data/processed",2015,".rds"))


# If loading in from processed files, script can start here


load("/users/eric.ward/documents/CFEC/data/cfec_w_IDs")
# this is cfecnew object

dat1 = readRDS(paste0("/users/eric.ward/documents/CFEC/data/processed",2015,".rds"))
dat2 = readRDS(paste0("/users/eric.ward/documents/CFEC/data/processed_allYears.rds"))

# subset only names of cfec data in dat1, dat2
cfec_combined = rbind(dat2, cfecnew[, match(names(dat1), names(cfecnew))])
cfec_combined = rbind(cfec_combined, dat1)

###################################################################################
#
# Now run Jordan / Eric's code to do some preliminary filtering
###################################################################################
library(dplyr)
cfec = cfec_combined
remove(cfec_combined)

# Remove A80 and CP vessels
A80<-c(68870,29923,22011,39798,59870,8500,41219,62545,56965,54693,1119,57228,69038,61081,61083,55921,57211,56964,36202,43260,54392,51873,48183,55767)
CP <-c(56987,59378,57621,59170,55767,57450,54886,56991,60407,60795,60202,60660,56789,56618,59503)
cfec <- filter(cfec, !adfg %in% as.character(c(A80,CP)))
rm(A80,CP)

cfec <- group_by(cfec,year) %>%
  mutate(ft=paste0(gsub("19","",year),f_ticket,p_holder)) %>%
  dplyr::select(-f_ticket) %>%
  rename(f_ticket=ft)

cfec = cfec[which(cfec$C_LANDST == "C"), ]
cfec = cfec[-which(cfec$P_STATUS%in%c("", "O")), ]

cfec$gearn = as.numeric(cfec$F_GEAR)
cfec = cfec[, -which(names(cfec)=="F_GEAR")]

cfec = cfec[-which(cfec$gearn%in%c(8, 13, 23, 27, 37, 41, 85, 90, 99)),]
cfec$gearn[which(cfec$gearn==61)] = 6
cfec$gearn[which(cfec$gearn%in%c(9, 19, 29, 39, 49, 59, 69, 91))] = 9

cfec$specn.orig = cfec$specn

# Lump all skates together
cfec$specn[cfec$specn==700] = 700
cfec$specn[cfec$specn==701] = 700
cfec$specn[cfec$specn==702] = 700
cfec$specn[cfec$specn==703] = 700
cfec$specn[cfec$specn==704] = 700
cfec$specn[cfec$specn==705] = 700

# Lump all sharks together
cfec$specn[cfec$specn==689] = 689
cfec$specn[cfec$specn==690] = 689
cfec$specn[cfec$specn==691] = 689
cfec$specn[cfec$specn==692] = 689

# Lump flatfish (Shallow)
cfec$specn[cfec$specn==123] = 119 # rock
cfec$specn[cfec$specn==129] = 119 # starry flounder
cfec$specn[cfec$specn==127] = 119 # yellowfin
cfec$specn[cfec$specn==126] = 119 # butter
cfec$specn[cfec$specn==122] = 119 # flathead
cfec$specn[cfec$specn==125] = 119 # rex
cfec$specn[cfec$specn==128] = 119 # English
cfec$specn[cfec$specn==131] = 119 # petrale
cfec$specn[cfec$specn==132] = 119 # sand
cfec$specn[cfec$specn==133] = 119 # Alaska plaice
cfec$specn[cfec$specn==116] = 119 # Bering flounder
cfec$specn[cfec$specn==117] = 119 # Kamchatka flounder

# 5, 6, 7, 15, 26, 47, 61,
# Lump flatfish (deep)
cfec$specn[cfec$specn==124] = 118 # dover
cfec$specn[cfec$specn==134] = 118 # greenland turbot

# Lump rockfish, codes from here:
# Slope rockfish
cfec$specn[cfec$specn==136] = 144 # northern
cfec$specn[cfec$specn==137] = 144 # bocaccio
cfec$specn[cfec$specn==141] = 144 # POP
cfec$specn[cfec$specn==143] = 144 # thornyhead
cfec$specn[cfec$specn==144] = 144 # unid
cfec$specn[cfec$specn==151] = 144 # rougheye
cfec$specn[cfec$specn==152] = 144 # shortraker
cfec$specn[cfec$specn==153] = 144 # redbanded
cfec$specn[cfec$specn==157] = 144 # silvergray
cfec$specn[cfec$specn==158] = 144 # redstripe
cfec$specn[cfec$specn==159] = 144 # darkblotched
cfec$specn[cfec$specn==166] = 144 # sharpchin
cfec$specn[cfec$specn==176] = 144 # harlequin
cfec$specn[cfec$specn==177] = 144 # blackgill
cfec$specn[cfec$specn==179] = 144 # pygmy
cfec$specn[cfec$specn==181] = 144 # shortbelly
cfec$specn[cfec$specn==182] = 144 # splitnose
cfec$specn[cfec$specn==183] = 144 # stripe tail
cfec$specn[cfec$specn==184] = 144 # vermillion
cfec$specn[cfec$specn==185] = 144 # aurora

# Demersal rockfish
cfec$specn[cfec$specn==135] = 168 # greenstripe
cfec$specn[cfec$specn==138] = 168 # copper
cfec$specn[cfec$specn==139] = 168 # unid
cfec$specn[cfec$specn==140] = 168 # red
cfec$specn[cfec$specn==145] = 168 # yelloweye
cfec$specn[cfec$specn==146] = 168 # Canary
cfec$specn[cfec$specn==147] = 168 # quillback
cfec$specn[cfec$specn==148] = 168 # tiger
cfec$specn[cfec$specn==149] = 168 # China
cfec$specn[cfec$specn==150] = 168 # rosethorn
cfec$specn[cfec$specn==171] = 168 # shortraker/rougheye
cfec$specn[cfec$specn==175] = 168 # yellowmouth
cfec$specn[cfec$specn==178] = 168 # chilipepper

# Pelagic rockfish
cfec$specn[cfec$specn==142] = 169 # black
cfec$specn[cfec$specn==154] = 169 # dusky
cfec$specn[cfec$specn==155] = 169 # yellowtail
cfec$specn[cfec$specn==156] = 169 # widow
cfec$specn[cfec$specn==167] = 169 # blue
cfec$specn[cfec$specn==172] = 169 # dusky
cfec$specn[cfec$specn==173] = 169 # dark

# Group sculpins
cfec$specn[cfec$specn %in% c(160:165)] = 160

# Group greenlings (not AMCK or lingcod)
cfec$specn[cfec$specn %in% c(190,191,192,194)] = 190

# Group all osmerids together
cfec$specn[cfec$specn %in% c(510:516)] <- 510

# Group all char
cfec$specn[cfec$specn %in% c(520:532)] <- 520  # char

# Group all trout
cfec$specn[cfec$specn%in%c(540:562)] <- 540  # trout

#  Whitefish and trout did not have text labels in the species database.
#  We created our own for easier reference but given this lack of detail
#  it seems reasonable to lump these species groups for the sake of clarity
# Whitefish
cfec$specn[cfec$specn==580] <- 580  # Whitefish
cfec$specn[cfec$specn==583] <- 580  # Least cisco
cfec$specn[cfec$specn==584] <- 580  # Arctic cisco
cfec$specn[cfec$specn==585] <- 580  # Bering cisco
cfec$specn[cfec$specn==589] <- 580  # Humpback whitefish

# Halibut - appear under several different historic species codes so we consolidate these
cfec$specn[cfec$specn %in% c(200:204)] <- 200

# Chinook have two species codes. Consolidate.
cfec$specn[cfec$specn==411] <- 410

# Salmon roe (by species) get lumped into a single salmon roe category. This was a topic of much discussion but ultimately, since much of the roe is unidentified, any other assignments would end up omitting a large percentage of the roe landings.
cfec$specn[cfec$specn %in% c(401:405)] <- 400
cfec$specn[cfec$specn%in%c(410:450) & cfec$F_DELIVR==14] <- 400

# Herring are listed under 5 different species codes but 231:235 only account for a few thousand bucks. They are mostly bycatch or non-commercial.
# We'll lump them into the sac-roe species code because they will have no effect on the numbers. But this way we are consolidating codes instead
# of making a bunch of extra ones.
cfec$specn[cfec$specn %in% c(231:235)] <- 23043

# Most herring landings occur under species 230, with different F_DELIVR codes
# Create a species code for the sac-roe fishery
cfec$specn[cfec$specn==230 & cfec$F_DELIVR==43] <- 23043
# Create a species code for the bait/food fishery
cfec$specn[cfec$specn==230 & cfec$F_DELIVR==44] <- 23044
# Create a species code for the pound fishery
cfec$specn[cfec$specn==230 & cfec$F_DELIVR==45] <- 23045
# There may be 1 or 2 records with other codes, and which probably have no dollar value.
# Lump them into the sac roe fishery to avoid having extra codes
cfec$specn[cfec$specn==230] <- 23043

# Lump all king crabs
cfec$specn[cfec$specn %in% c(920:925)] <- 920

# Lump all king crabs
cfec$specn[cfec$specn %in% c(930:934)] <- 930

# Lump all remaining crabs (not Dungeness)
cfec$specn[cfec$specn %in% c(900,940:953)] <- 940

# Lump all shrimp
cfec$specn[cfec$specn %in% c(960:965)] <- 960

# Lump non-geoduck and non-razor clams together
cfec$specn[cfec$specn %in% c(810,812,820,825,840,842)] <- 810

# Lump weathervane and pink scallops
cfec$specn[cfec$specn %in% c(850:851)] <- 850

# Lump urchins
cfec$specn[cfec$specn %in% c(892:893,896)] <- 892

# Lump sea cucumbers
cfec$specn[cfec$specn %in% c(894:895)] <- 895

# Create an "OTHER" category that includes misc freshwater and a bunch of random trawl caught spec
cfec$specn[cfec$specn %in% c(100,101,112,170,180,190,206:220,250,260,310:380,490,500,520,540,570,580,590,600:683,714:800,880,890,855,899,998,999)] <- 206

# We no longer need the F_DELIVR column and can remove it
cfec <- dplyr::select(cfec,-F_DELIVR)

load("/users/eric.ward/documents/CFEC/data/SpeciesTable.RData")

#  000,033 and 095 don't show up in the species codes (they account for only 7 records)
#  Remove them
cfec <- filter(cfec,!specn %in% c("000","033","095"))
#  To avoid issues with different factor levels, convert specn to numeric for now.
cfec$specn <- as.numeric(as.character(cfec$specn))

#  Join the cfec dataset with the "spec"
cfec <- left_join(cfec,spec.tbl)

#  Rename some of the groups
cfec$spec <- as.character(cfec$spec)

#  Rename some of the groups
cfec$spec <- as.character(cfec$spec)
cfec$spec[cfec$specn==118] = "FLAT.deep"
cfec$spec[cfec$specn==119] = "FLAT.shallow"
cfec$spec[cfec$specn==144] = "ROCK.slope"
cfec$spec[cfec$specn==168] = "ROCK.demersal"
cfec$spec[cfec$specn==169] = "ROCK.pelagic"
cfec$spec[cfec$specn==206] = "OTHER"
cfec$spec[cfec$specn==400] = "aUROE"
cfec$spec[cfec$specn==689] = "SHARK"
cfec$spec[cfec$specn==700] = "SKATE"
cfec$spec[cfec$specn==810] = "OCLM"
cfec$spec[cfec$specn==850] = "SCAL"
cfec$spec[cfec$specn==892] = "URCH"
cfec$spec[cfec$specn==920] = "KCRB"
cfec$spec[cfec$specn==930] = "TCRB"
cfec$spec[cfec$specn==940] = "OCRB"
cfec$spec[cfec$specn==960] = "USRM"
cfec$spec[cfec$specn==23043] = "SAC"
cfec$spec[cfec$specn==23044] = "BAIT"
cfec$spec[cfec$specn==23045] = "POUND"

cfec$spec[cfec$spec==""] <- NA

# drop records with no spp
cfec = filter(cfec, !is.na(spec))
rm(spec.tbl)

cfec$area = NA
cfec <- as.data.frame(cfec)

cfec$INDEX <- 1:nrow(cfec)

cfec$p_holder = as.numeric(as.factor(cfec$p_holder))
cfec$cadfg = as.numeric(as.factor(cfec$cadfg))
cfec$p_serial = as.numeric(as.factor(cfec$p_serial))

# number fish tickets sequentially
cfec$f_ticket = as.numeric(as.factor(cfec$f_ticket))
cfec$F_PERMIT = as.numeric(as.factor(cfec$F_PERMIT))

save(cfec, file = "/users/eric.ward/documents/pfx-commercial/data/cfecCleaned2_salmon.Rdata")
