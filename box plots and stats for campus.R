setwd("/Users/creanzn/_your_working_directory") ##put your working directory here

#Download your files from box -- analysis output in your folder, and metadata from the main folder

#goal: compare xeno-canto analysis of a species to our on-campus recordings of that species

#Load in xeno-canto data from the species you chose -- change filename accordingly
BigData<-read.table("AnalysisOutput_20190225_T222242.txt", sep = '\t',header = TRUE, fileEncoding = "UTF-8")

#Load big metadata
BigMetaData<-read.csv('BSCI3965_AllSpeciesMetaData_wNearest.csv')

#Load in campus data
CampusData=read.table("AnalysisOutput_Campus_Recordings.txt", sep = '\t',header = TRUE)
CampusDataFrame=as.data.frame(CampusData)

#Load compus metadata
CampusMetaData=read.table("Campus_recordings_metadata.txt", sep = '\t',header = TRUE, fileEncoding = "UTF-8")

#Change "Tufted Titmouse" to your species of interest!!
FilenameList=CampusMetaData$File.Name[which(CampusMetaData$Species=="Tufted Titmouse")]

FilenameList
FilenameDataframe=as.data.frame(FilenameList)

#Find the number of records for your species of interest
dim(FilenameDataframe)
dim(FilenameDataframe)[1]

CampusOneSpeceisSubset=seq(1:40)
for (i in 1:dim(FilenameDataframe)[1]){
  CampusOneSpeceisSubset=rbind(CampusOneSpeceisSubset,CampusDataFrame[which(CampusDataFrame$FileName==as.character(FilenameDataframe[i,])),1:40])
}


#what do we need to do to match the meta data to our analyis output?

#make a new column in the dataframe that has the xeno canto numbers -- how should we do this?
#make another new column that has the bout number
BigData$FileName[1]
as.character(BigData$FileName[1])


teststringsplit = strsplit(as.character(BigData$FileName[1]),"[.bt_-]")  #you might need to add things here if you used other characters to delineate your bouts in the file name


TestOutput=seq(1,lengths(teststringsplit))#########  lengths() gives the number of elements in the stringsplit
for (i in 1:length(BigData$FileName)){
  TestOutput=rbind(TestOutput,unlist(strsplit(as.character(BigData$FileName[i]),"[.bt_-]")))########### change this if you changed above
}


hist(BigData$avg_notes_lower_freq.Hz.)
hist(log(BigData$avg_notes_lower_freq.Hz))


class(TestOutput)

TestOutputDataFrame=as.data.frame(TestOutput[2:dim(TestOutput)[1],])########### starts with 2 because the first row is an index

LogBigData=log(BigData[,2:40])

###be careful here -- V10 and V12 probably need to be changed to suit your stringsplit!
BigDataWithXCCity=cbind(BigData[,1],LogBigData,TestOutputDataFrame$V10,TestOutputDataFrame$V12)

NearDist=seq(1:length(BigData$FileName))
for (i in 1:length(BigData$FileName)){
  print(TestOutputDataFrame$V10[i]) ##change V10 if necessary
  if (length(which(TestOutputDataFrame$V10[i]==BigMetaData$Recording))>0){ ##change V10 if necessary
    print(which(TestOutputDataFrame$V10[i]==BigMetaData$Recording)) ##change V10 if necessary
    print(BigMetaData$NEAR_DIST[which(TestOutputDataFrame$V10[i]==BigMetaData$Recording)]) ##change V10 if necessary
    NearDist[i]=BigMetaData$NEAR_DIST[which(TestOutputDataFrame$V10[i]==BigMetaData$Recording)]} ##change V10 if necessary
  else{
    NearDist[i]=NA}
}

BigDataWithXCCity=cbind(BigDataWithXCCity,NearDist)
which(NearDist>10000) 
which(NearDist<10000) 


pdf("test rural v urban v campus all.pdf")
par(mfrow=c(4,4),  mar=c(3,3,3,1))
for (i in 2:40){
  print(i)
  wilcoxresults=wilcox.test(BigDataWithXCCity[which(NearDist>10000),i], BigDataWithXCCity[which(NearDist<10000),i])
  boxplot(BigDataWithXCCity[which(NearDist>10000),i], BigDataWithXCCity[which(NearDist<10000),i],log(CampusOneSpeceisSubset[2:9,i]), names=c("Rural", "Urban", "Campus" ))
  title(paste(" ",names(BigDataWithXCCity)[i],"\n rank-sum p =",round(wilcoxresults$p.value,8),"\n"),cex.main=0.8)
}
dev.off()
