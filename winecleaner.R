winedata <- read.csv("C:/Users/Admin/Documents/Wine_Data_Unclean.csv", stringsAsFactors = FALSE)

wine_region<-regmatches(winedata$variety_and_region, regexpr("/.+",winedata$variety_and_region))

regionfixed<-sub("/ ", "", wine_region)
winedata$Region <- regionfixed

replacement<-sub("/.+", "", winedata$variety_and_region)
winedata$variety_and_region<-replacement


wine_region2<-regmatches(winedata$Region, regexpr("/.+",winedata$Region))

regionfixed2<-sub("/ ", "", wine_region2)
winedata$SubRegion <- regionfixed2

regionfixed3<-sub("/.+", "", winedata$Region)
winedata$Region <- regionfixed3

colnames(winedata)[10]<-"Variety"

uncleandata <- read.csv("C:/Users/Admin/Documents/Wine_Data_Unclean.csv", stringsAsFactors = FALSE)
datacleaner <- function(tablename, column1, column2, column3){
  
  newcolumndata<-regmatches(tablename[,column1], regexpr("/.+",tablename[,column1]))
  
  replacer<-sub("/.+", "", tablename[,column1])
  tablename[,column1]<-replacer
  
  newcolumnfix<-sub("/ ", "", newcolumndata)
  tablename[,column2] <- newcolumnfix
  
  newcolumn2data<-regmatches(tablename[,column2], regexpr("/.+",tablename[,column2]))
  
  replacer2<-sub("/.+", "", newcolumnfix)
  tablename[,column2]<-replacer2
  
  newcolumnfix2<-sub("/ ", "", newcolumn2data)
  tablename[,column3] <- newcolumnfix2
  
  return(tablename)
}
cleandata<-datacleaner(uncleandata,"variety_and_region","Region","Subregion")
