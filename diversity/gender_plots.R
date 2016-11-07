
# Look at time series of gender diversity, by permit
library(gender)
library(dplyr)
df = load(file.choose())

# 1:16 last name
# 17-28 first name
# 29 middle initial

cfecnew$firstname = substr(cfecnew$I_NAME, 17, 28)

cfecnew = filter(cfecnew, firstname!="")

names = as.character(unique(cfecnew$firstname))
newname = ""
for(i in 1:length(names)) {
  newname[i] = getElement(strsplit(names[i], " "), 1)
}
gender = gender(newname, countries="United States")
names(gender)[which(names(gender)=="name")] = "name_short"
gender$firstname = names[match(gender$name_short, newname)]

cfecnew = left_join(cfecnew, gender, by="firstname")

# calcualte average across permit / year
g = group_by(cfecnew, p_fshy, year) %>%
  summarize(nPeople = length(unique(p_holder)),
    nFemale = length(unique(p_holder[which(gender=="female")])))
g$pFemale = g$nFemale/g$nPeople

totals = group_by(g, p_fshy) %>%
  summarize(n = sum(nPeople)) %>%
  arrange(-n)


# subset out totals of < 100 people per year
group_by(g, p_fshy) %>%
  mutate(totals = sum(nPeople)) %>%
  filter(totals >= 100) %>%

library(ggplot2)
pdf("proportion female.pdf")
indx = 1:16
for(j in 1:10) {
ggplot(data = g[g$p_fshy%in%totals$p_fshy[indx],], aes(year, pFemale, group=p_fshy)) +
  geom_line() + facet_wrap(~p_fshy) + xlab("Year") + ylab("Percent female")
indx = indx + 16
}
dev.off()

