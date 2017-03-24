# load in original data
load("/users/eric.ward/documents/cfec/cfec_w_IDs")

cfec = cfecnew
rm(cfecnew) # remove the old named object

#idx = grep("S 04T", cfec$p_fshy)
#group_by(cfec[idx,], year) %>%
#  summarize(nSetnet = length(unique(p_holder)))
# Remove A80 and CP vessels
A80<-c(68870,29923,22011,39798,59870,8500,41219,62545,56965,54693,1119,57228,69038,61081,61083,55921,57211,56964,36202,43260,54392,51873,48183,55767)
CP <-c(56987,59378,57621,59170,55767,57450,54886,56991,60407,60795,60202,60660,56789,56618,59503)
cfec <- filter(cfec,!adfg %in% c(A80,CP))
rm(A80,CP)

isNull = c("F_FG_MGT","C_CFACT", "F_DISTRC", "F_ERRFLG", "F_AYK", "G_FED_AR","F_BTAREA",
  "A_CITY", "A_STATE", "A_ZIP", "P_YEAR", "I_NAME", "I_NAMTYP", "F_POT_DR", "F_PRCENT",
  "C_ADFGST", "F_CTDATE", "F_LNDATE", "F_NUMBER", "F_VALUE", "F_POUNDS", "F_TNUMB",
  "F_BATCH", "F_PERIOD",  "F_EMBSEQ", "F_PROCCK", "F_WEEK",  "F_TTYPE",
  "F_ITEM", "F_FSHERY",  "G_GEAR", "G_AREA", "G_PRC_AR",  "P_YEAR", "P_PMTSEQ",
  "p_check", "P_ID", "P_FEETYP", "P_ADFG", "P_FEEFLG", "P_FSHBLE",
  "F_MPGMNM", "D_OPER", "D_O_FSHY", "D_O_PNUM", "D_O_PCHK", "D_O_YSEQ", "F_SIZE",
  "D_O_YCHK", "adfg")
cfec = cfec[,-which(names(cfec)%in%isNull)]
rm(isNull)

# pull in halibut permit holder info from jen
library(haven)
d = read_sas("/users/eric.ward/downloads/Halibut.sas7bdat")
names(d) = tolower(names(d))
names(d)[which(names(d)=="serial")] = "p_serial"
names(d)[which(names(d)=="a_resid")] = "a_res"
names(cfec) = tolower(names(cfec))

cfec = left_join(cfec, d)

# probabilistically assign sex
library(gender)
names = data.frame("name"=unique(cfec$name),
  "first_name" = unlist(lapply(strsplit(unique(cfec$name)," "), getElement, 1)))
names$first_name = as.character(names$first_name)

pred_gender = gender(unique(as.character(names$first_name)))
names(pred_gender)[which(names(pred_gender)=="name")] = "first_name"
pred_gender = pred_gender[,c("first_name","gender")]
names$gender = pred_gender$gender[match(names$first_name, pred_gender$first_name)]

cfec = left_join(cfec, names)

cfec = cfec[,-which(names(cfec)%in%c("first_name"))]

cfec <- group_by(cfec,year) %>% mutate(ft=paste0(gsub("19","",year),f_ticket,p_holder)) %>% dplyr::select(-f_ticket) %>% rename(f_ticket=ft)

cfec = cfec[which(cfec$c_landst == "C"), ]
cfec = cfec[-which(cfec$p_status%in%c("", "O")), ]

cfec$gearn = as.numeric(cfec$f_gear)
cfec = cfec[, -which(names(cfec)=="f_gear")]
cfec = cfec[-which(cfec$gearn%in%c(8, 13, 23, 27, 37, 41, 85, 90, 99)),]
# Merge 6 and 61 into longline
cfec$gearn[which(cfec$gearn==61)] = 6
# Merge all pot gears together -- note only 9 and 91 are reported
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
cfec$specn[cfec$specn%in%c(410:450) & cfec$f_delivr==14] <- 400

cfec$specn[cfec$specn %in% c(231:235)] <- 23043

# Most herring landings occur under species 230, with different F_DELIVR codes
# Create a species code for the sac-roe fishery
cfec$specn[cfec$specn==230 & cfec$f_delivr==43] <- 23043
# Create a species code for the bait/food fishery
cfec$specn[cfec$specn==230 & cfec$f_delivr==44] <- 23044
# Create a species code for the pound fishery
cfec$specn[cfec$specn==230 & cfec$f_delivr==45] <- 23045
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
cfec <- dplyr::select(cfec,-f_delivr)

#------------------------------------------
# To add text species codes, load and join with Species lookup table
load("/users/eric.ward/documents/cfec/SpeciesTable.RData")

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
cfec$stat6 = as.numeric(cfec$stat6)

cfec$area[cfec$stat6==485900] = "PWS"
cfec$area[cfec$stat6==475900] = "PWS"
cfec$area[cfec$stat6==465900] = "PWS"
cfec$area[cfec$stat6==455900] = "PWS"
cfec$area[cfec$stat6==445900] = "PWS"
cfec$area[cfec$stat6==495930] = "PWS"
cfec$area[cfec$stat6==485930] = "PWS"
cfec$area[cfec$stat6==475930] = "PWS"
cfec$area[cfec$stat6==465930] = "PWS"
cfec$area[cfec$stat6==455930] = "PWS"
cfec$area[cfec$stat6==445930] = "PWS"
cfec$area[cfec$stat6==486000] = "PWS"
cfec$area[cfec$stat6==476000] = "PWS"
cfec$area[cfec$stat6==466000] = "PWS"
cfec$area[cfec$stat6==456000] = "PWS"
cfec$area[cfec$stat6==446000] = "PWS"
cfec$area[cfec$stat6==486030] = "PWS"
cfec$area[cfec$stat6==476030] = "PWS"
cfec$area[cfec$stat6==466030] = "PWS"
cfec$area[cfec$stat6==456030] = "PWS"
cfec$area[cfec$stat6==486100] = "PWS"
cfec$area[cfec$stat6==476100] = "PWS"
cfec$area[cfec$stat6==466100] = "PWS"
cfec$area[which(cfec$stat6 < 20099 & cfec$stat6 >= 20000)] = "PWS"
cfec$area[which(cfec$stat6 < 21299 & cfec$stat6 >= 21200)] = "PWS"
cfec$area[which(cfec$stat6 < 22799 & cfec$stat6 >= 22700)] = "PWS"
cfec$area[which(cfec$stat6 < 22699 & cfec$stat6 >= 22600)] = "PWS"
cfec$area[which(cfec$stat6 < 22899 & cfec$stat6 >= 22800)] = "PWS"
cfec$area[which(cfec$stat6 < 22199 & cfec$stat6 >= 22100)] = "PWS"
cfec$area[which(cfec$stat6 < 22299 & cfec$stat6 >= 22200)] = "PWS"
cfec$area[which(cfec$stat6 < 22599 & cfec$stat6 >= 22500)] = "PWS"
cfec$area[which(cfec$stat6 < 22499 & cfec$stat6 >= 22400)] = "PWS"
cfec$area[which(cfec$stat6 < 22399 & cfec$stat6 >= 22300)] = "PWS"
# Add halibut areas
cfec$area[cfec$stat6 %in% c(242,232,220,230,240)] = "PWS"

cfec$area[cfec$stat6==525900] = "CookInlet"
cfec$area[cfec$stat6==535900] = "CookInlet"
cfec$area[cfec$stat6==515930] = "CookInlet"
cfec$area[cfec$stat6==525930] = "CookInlet"
cfec$area[cfec$stat6==535930] = "CookInlet"
cfec$area[cfec$stat6==516000] = "CookInlet"
cfec$area[cfec$stat6==526000] = "CookInlet"
cfec$area[cfec$stat6==496030] = "CookInlet"
cfec$area[cfec$stat6==506030] = "CookInlet"
cfec$area[cfec$stat6==516030] = "CookInlet"
cfec$area[cfec$stat6==526030] = "CookInlet"
cfec$area[cfec$stat6==506100] = "CookInlet"
cfec$area[cfec$stat6==516100] = "CookInlet"
cfec$area[which(cfec$stat6 < 24799 & cfec$stat6 >= 24400)] = "CookInlet"
cfec$area[which(cfec$stat6%in%c(261, 272))] = "CookInlet"

cfec$area[which(cfec$stat6 < 11599 & cfec$stat6 >= 11100)] = "Southeast"
cfec$area[which(cfec$stat6 < 10499 & cfec$stat6 >= 10100)] = "Southeast"
cfec$area[which(cfec$stat6 < 11099 & cfec$stat6 >= 10500)] = "Southeast"
cfec$area[which(cfec$stat6 >= 305430 & cfec$stat6 <= 305530)] = "Southeast"
cfec$area[which(cfec$stat6 >= 315430 & cfec$stat6 <= 315600)] = "Southeast"
cfec$area[which(cfec$stat6 >= 325430 & cfec$stat6 <= 325700)] = "Southeast"
cfec$area[which(cfec$stat6 >= 335430 & cfec$stat6 <= 335800)] = "Southeast"
cfec$area[which(cfec$stat6 >= 345400 & cfec$stat6 <= 345830)] = "Southeast"
cfec$area[which(cfec$stat6 >= 355400 & cfec$stat6 <= 355900)] = "Southeast"
cfec$area[which(cfec$stat6 > 365400 & cfec$stat6 < 365830)] = "Southeast"
# For IPHC see Fig. 20 here: http://www.iphc.int/publications/techrep/tech0049.pdf
cfec$area[which(cfec$stat6%in%c(140, 141, 142, 143, 144, 150, 151, 152, 153, 160, 161,
  162, 163, 170, 171, 172, 174, 174, 181, 182, 183, 184, 185))] = "Southeast"

# Kodiak east area is defined by halibut areas 270/280, includes ADFG salmon /
# shellfish on E side of kodiak, and groundfish areas extending to NMFS Area 630
# boundary
cfec$area[cfec$stat6%in%c(270,280)] = "Kodiak.east"
cfec$area[which(cfec$stat6 < 25899 & cfec$stat6 >= 25800)] = "Kodiak.east"
cfec$area[which(cfec$stat6 < 25999 & cfec$stat6 >= 25900)] = "Kodiak.east"
cfec$area[cfec$stat6==535630] = "Kodiak.east"
cfec$area[cfec$stat6==525630] = "Kodiak.east"
cfec$area[cfec$stat6==515630] = "Kodiak.east"
cfec$area[cfec$stat6==505630] = "Kodiak.east"
cfec$area[cfec$stat6==495630] = "Kodiak.east"
cfec$area[cfec$stat6==535700] = "Kodiak.east"
cfec$area[cfec$stat6==525700] = "Kodiak.east"
cfec$area[cfec$stat6==515700] = "Kodiak.east"
cfec$area[cfec$stat6==505700] = "Kodiak.east"
cfec$area[cfec$stat6==495700] = "Kodiak.east"
cfec$area[cfec$stat6==525730] = "Kodiak.east"
cfec$area[cfec$stat6==515730] = "Kodiak.east"
cfec$area[cfec$stat6==505730] = "Kodiak.east"
cfec$area[cfec$stat6==495730] = "Kodiak.east"
cfec$area[cfec$stat6==525800] = "Kodiak.east"
cfec$area[cfec$stat6==515800] = "Kodiak.east"
cfec$area[cfec$stat6==505800] = "Kodiak.east"
cfec$area[cfec$stat6==495800] = "Kodiak.east"

cfec$area[cfec$stat6%in%c(271,281)] = "Kodiak.west"
cfec$area[cfec$stat6==535730] = "Kodiak.west"
cfec$area[cfec$stat6==545730] = "Kodiak.west"
cfec$area[cfec$stat6==525800] = "Kodiak.west"
cfec$area[cfec$stat6==535800] = "Kodiak.west"
cfec$area[cfec$stat6==545800] = "Kodiak.west"
cfec$area[cfec$stat6==535830] = "Kodiak.west"
cfec$area[cfec$stat6==525830] = "Kodiak.west"
cfec$area[which(cfec$stat6 < 25699 & cfec$stat6 >= 25500)] = "Kodiak.west"
cfec$area[which(cfec$stat6 < 25499 & cfec$stat6 >= 25300)] = "Kodiak.west"
cfec$area[which(cfec$stat6 < 26299 & cfec$stat6 >= 26000)] = "Kodiak.west"

# Bristol Bay, defined as going west to the boundary between NMFS areas 512 / 516
cfec$area[cfec$stat6%in%c(31600, 31700, 31800, 32000, 32100,
  32200, 32400, 32500, 32600)] = "BristolBay"
cfec$area[cfec$stat6%in%c(575730, 575800, 575830)] = "BristolBay"
cfec$area[cfec$stat6%in%c(585700, 585730, 585800, 585830)] = "BristolBay"
cfec$area[cfec$stat6%in%c(595630, 595700, 595730, 595800, 595830)] = "BristolBay"
cfec$area[cfec$stat6%in%c(605600, 605630, 605700, 605730, 605800, 605830)] = "BristolBay"
cfec$area[cfec$stat6%in%c(615600, 615630, 615700, 615730, 615800, 615830)] = "BristolBay"

cfec <- as.data.frame(cfec)


cfec$INDEX <- 1:nrow(cfec)
names(cfec)[which(names(cfec)=="INDEX")]="index"

cfec$p_holder = as.numeric(as.factor(cfec$p_holder))
cfec$cadfg = as.numeric(as.factor(cfec$cadfg))
cfec$p_serial = as.numeric(as.factor(cfec$p_serial))

# number fish tickets sequentially
cfec$f_ticket = as.numeric(as.factor(cfec$f_ticket))
cfec$f_permit = as.numeric(as.factor(cfec$f_permit))

saveRDS(cfec, file="cfec_w_halibut_permit_info.rds")

