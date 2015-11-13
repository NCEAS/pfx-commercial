########################################################
# Process raw data files, first merging them together
# with common field names. Directory structure is 3 folders,
# cfecft75_89
# cfecft90_99
# cfecft00_09
########################################################

# Data are separately loaded by year. Create an .Rdata file that merges them all together
cfec = read.csv("cfecft75_89/cfecft1975.csv")

indx2drop = which(names(cfec)%in%c("corradfg","origadfg","f_fshery"))# drop names not present after 1990
cfec = cfec[,-c(indx2drop)]

# Merge the other years
for(yr in 76:89) {
  newdata = read.csv(paste("cfecft75_89/cfecft19",yr,".csv",sep=""), stringsAsFactors=FALSE)
  indx2drop = which(names(newdata)%in%c("corradfg","origadfg","f_fshery"))# drop names not present after 1990
  newdata = newdata[,-c(indx2drop)]
  
  cfec = rbind(cfec,newdata)
}
for(yr in 90:90) {
  newdata = read.csv(paste("cfecft90_99/cfecft19",yr,".csv",sep=""), stringsAsFactors=FALSE)
  indx2drop = which(names(newdata)%in%c("corradfg","origadfg","f_fshery"))# drop names not present after 1990
  newdata = newdata[,-c(indx2drop)]
  
  cfec = rbind(cfec,newdata)
}

cfec = cfec[,-which(names(cfec)%in%c("f_pounds","f_netlbs","f_value","c_landst"))]

# These years have more field codes, and the names change relative to above
for(yr in 91:99) {
  newdata = read.csv(paste("cfecft90_99/cfecft19",yr,".csv",sep=""), stringsAsFactors=FALSE)
  names(newdata) = tolower(names(newdata))
  # only include GOA data - get rid of BSAI and INTL
  #newdata = newdata[-which(newdata$fmp_area%in%c("BSAI","INTL")),]
  newsub = newdata[,c("year","startdt","lnddate","landdate","procid","port","stat6","adfg","f_cdq","gearn",
                      "specn","prod","size","g_pounds","g_earn","g_price","p_fshy","p_serial",
                      "p_check","spec","g_spcs","harvest")]
  cfec = rbind(cfec,newsub)
}

# These years have more field codes, and the names change relative to above
for(yr in 0:9) {
  newdata = read.csv(paste("cfecft00_10/cfecft200",yr,".csv",sep=""), stringsAsFactors=FALSE)
  names(newdata) = tolower(names(newdata))
  # only include GOA data - get rid of BSAI and INTL
  #newdata = newdata[-which(newdata$fmp_area%in%c("BSAI","INTL")),]  
  newsub = newdata[,c("year","startdt","lnddate","landdate","procid","port","stat6","adfg","f_cdq","gearn",
                      "specn","prod","size","g_pounds","g_earn","g_price","p_fshy","p_serial",
                      "p_check","spec","g_spcs","harvest")]
  cfec = rbind(cfec,newsub)
}

# Add in the 2010 data
newdata = read.csv(paste("cfecft00_10/cfecft2010",".csv",sep=""), stringsAsFactors=FALSE)
names(newdata) = tolower(names(newdata))
# only include GOA data - get rid of BSAI and INTL
#newdata = newdata[-which(newdata$fmp_area%in%c("BSAI","INTL")),]  
newsub = newdata[,c("year","startdt","lnddate","landdate","procid","port","stat6","adfg","f_cdq","gearn",
                    "specn","prod","size","g_pounds","g_earn","g_price","p_fshy","p_serial",
                    "p_check","spec","g_spcs","harvest")]
cfec = rbind(cfec,newsub)

# Add in the 2011-2014 data
# - First, drop out the "lndddate", which is redundant with "landdate"
cfec = cfec[,-which(names(cfec)%in%c("lnddate","size"))]

for(yr in 11:14) {
newdata = read.csv(paste("cfecft11_14/CFECFT_20",yr,".csv",sep=""), stringsAsFactors=FALSE)
names(newdata) = tolower(names(newdata))
names(newdata)[which(names(newdata)=="f_adfg")] = "adfg"
names(newdata)[which(names(newdata)=="yeara")] = "year"
newsub = newdata[,c("year","startdt","landdate","procid","port","stat6","adfg","f_cdq","gearn",
                    "specn","prod","g_pounds","g_earn","g_price","p_fshy","p_serial",
                    "p_check","spec","g_spcs","harvest")]
# convert date to match                    
newsub$startdt = paste(as.numeric(substr(newsub$startdt,5,6)), as.numeric(substr(newsub$startdt,7,8)), substr(newsub$startdt,1,4),sep="/")                   
newsub$landdate = paste(as.numeric(substr(newsub$landdate,5,6)), as.numeric(substr(newsub$landdate,7,8)), substr(newsub$landdate,1,4),sep="/")                   
                         
cfec = rbind(cfec,newsub)
}


# Save the raw data, 
save(cfec,file="cfec.Rdata")

# extract julian day
# delete 56 records with no date
cfec = cfec[-which(cfec$landdate==""),]
dates = as.character(cfec$landdate)
dates = strsplit(dates,"/")
cfec$land_day = lapply(dates,getElement,2)
cfec$land_month = lapply(dates,getElement,1)
cfec$land_month = as.numeric(unlist(cfec$land_month))
cfec$land_day = as.numeric(unlist(cfec$land_day))


save(cfec,file="cfec_clean.Rdata")

#set.seed(127)
for(i in 1:dim(cfec)[2]) {
	cfec[,i] = sample(cfec[,i], size=dim(cfec)[1], replace=T)
}

cfec$g_pounds = cfec$g_pounds + runif(dim(cfec)[1],0,0.3)
cfec$g_earn = cfec$g_earn + runif(dim(cfec)[1],0,0.3)
save(cfec,file="cfec_jitter.Rdata")

