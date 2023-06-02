setwd("/Users/SofiaCLima/Box/BirbSongs")
getwd()

library(warbleR)
TufTitRecs <- querxc("Baeolophus bicolor",download=FALSE)
TufTitSongs <- TufTitRecs[grep("song",TufTitRecs$Vocalization_type,ignore.case=TRUE),]

BlacapChicRecs <- querxc("Poecile atricapillus",download=FALSE)
BlacapChicSongs <- BlacapChicRecs[grep("song",BlacapChicRecs$Vocalization_type,ignore.case=TRUE),]


querxc(X=TufTitSongs[1:111,],download=TRUE)
querxc(X=BlacapChicSongs[1:98,],download=TRUE)
