
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

early = early[early$p_fshy%in%c("S 01E", "S 03A", "S 01A", "S 03E", "S 01K", "S 01M"),]

saveRDS(early, paste0("processed",y,".rds"))

}

# combine years into single file
dat = readRDS(paste0("processed",1975,".rds"))
for(y in 1976:1984) {
  tmp = readRDS(paste0("processed",y,".rds"))

  dat = rbind(dat, tmp)
}
saveRDS(dat, paste0("processed","_allYears",".rds"))

# merge in the 1985+ data
recent = readRDS("salmon_data.rds")
recent = recent[,-1]
recent$startdt = as.character(recent$startdt)
recent$landdate = as.character(recent$landdate)
dat = dat[,which(names(dat)%in%names(recent))]
dat$f_ticket = as.character(dat$f_ticket)

cfec = bind_rows(dat, recent)

