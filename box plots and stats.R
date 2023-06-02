setwd("/Users/creanzn/Downloads/BirbClass-master")

#Download your files from box -- analysis output in your folder, and metadata from the main folder

BigData<-read.table("AnalysisOutput_20190225_T222242.txt", sep = '\t',header = TRUE, fileEncoding = "UTF-8")


BigMetaData<-read.csv('BSCI3965_AllSpeciesMetaData_wNearest.csv')

BigData$FileName[1]



#How are these files organized?


#make some plots

#what do we need to do to match the meta data to our analyis output?

#make a new column in the dataframe that has the xeno canto numbers -- how should we do this?
#make another new column that has the bout number
BigData$FileName[1]
as.character(BigData$FileName[1])

strsplit(as.character(BigData$FileName[1]),"[.bt_-]")  #you might need to add things here if you used other characters to delineate your bouts in the file name


TestOutput=seq(1,13)  #13 is the number of elements in my stringsplit, you might need to change
for (i in 1:length(BigData$FileName)){
  TestOutput=rbind(TestOutput,unlist(strsplit(as.character(BigData$FileName[i]),"[.bt_-]")))
}

class(TestOutput)

TestOutputDataFrame=as.data.frame(TestOutput[2:68,])

LogBigData=log(BigData[,2:40])

BigDataWithXCCity=cbind(BigData[,1],LogBigData,TestOutputDataFrame$V10,TestOutputDataFrame$V12)

NearDist=seq(1:length(BigData$FileName))
for (i in 1:length(BigData$FileName)){
  print(which(TestOutputDataFrame$V10[i]==BigMetaData$Recording))
  print(BigMetaData$NEAR_DIST[which(TestOutputDataFrame$V10[i]==BigMetaData$Recording)])
  NearDist[i]=BigMetaData$NEAR_DIST[which(TestOutputDataFrame$V10[i]==BigMetaData$Recording)]
}

BigDataWithXCCity=cbind(BigDataWithXCCity,NearDist)
which(NearDist>10000) 
which(NearDist<10000) 


pdf("boxplot rural v urban all.pdf")
par(mfrow=c(4,4),  mar=c(3,3,3,1))
for (i in 2:40){
  print(i)
  wilcoxresults=wilcox.test(BigDataWithXCCity[which(NearDist>10000),i], BigDataWithXCCity[which(NearDist<10000),i])
  boxplot(BigDataWithXCCity[which(NearDist>10000),i], BigDataWithXCCity[which(NearDist<10000),i], names=c("Rural", "Urban" ))
  title(paste(" ",names(BigDataWithXCCity)[i],"\n rank-sum p =",round(wilcoxresults$p.value,8),"\n"),cex.main=0.8)
}
dev.off()