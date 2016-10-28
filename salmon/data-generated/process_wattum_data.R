
# this processes the kodiak data
dat = read_excel("salmon/data-generated/Table39_Wattum2015.xlsx", col_names = FALSE)
kodiak = matrix(0, nrow(dat), 7)
for(i in 1:nrow(dat)) {
  kodiak[i,] = unlist(strsplit(gsub(",","",dat[i,]), " "))
}
kodiak[20,1]=1989
kodiak = as.data.frame(as.matrix(kodiak))
names(kodiak) = c("Year","Chinook","Sockeye","Coho","Pink","Chum","Total")
write.table(kodiak, "salmon/data-generated/kodiak_totalHarvest.csv",sep=",",row.names=F,col.names=T)

