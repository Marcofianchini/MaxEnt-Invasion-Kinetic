
# load raw datasets
rcmed.folder <- paste0(wd,'data/','dwca-rcmed_2001-2020-v1.1/')
extendedmeasurementorfact <- read.delim(paste0(rcmed.folder,"extendedmeasurementorfact.txt"))
occurrence <- read.delim(paste0(rcmed.folder,"occurrence.txt"))
event <- read.delim(paste0(rcmed.folder,"event.txt"))

extendedmeasurementorfact<-remove_nas(extendedmeasurementorfact)$df
occurrence<-remove_nas(occurrence)$df
event<-remove_nas(event)$df


for (i in 1:nrow(extendedmeasurementorfact)) {
  value <- extendedmeasurementorfact[i, "measurementValue"]
  #print(value)
  
  if (!is.na(as.numeric(value)) ) {
    value <- as.numeric(value)
    #print(paste0('now value is ',value))
    
    if (value == 0) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  0"
    } else if (value == 1) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  1"
    } else if (value == 2) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  2"
    } else if (value > 2 & value < 6) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  3"
    } else if (value > 5 & value < 11) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  4"
    } else if (value > 10 & value < 51) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  5"
    } else if (value > 50) {
      extendedmeasurementorfact[i, "measurementValue"] <- "Category  6"
    }
  }
}
extendedmeasurementorfact$measurementValue <- as.factor(extendedmeasurementorfact$measurementValue)

#extendedmeasurementorfact<-extendedmeasurementorfact %>% mutate(measurementValue = as.numeric(gsub("Category ", "", measurementValue)))
extendedmeasurementorfact$eventID<-extendedmeasurementorfact$id

occurrence_filtered <- occurrence %>% 
  filter(scientificName == "Caulerpa cylindracea")

xx<-intersect(names(occurrence),names(event))
# Merge merged_df1 and occurrence
merged_df1 <- merge(occurrence, event, all = TRUE,by=xx)

xx<-intersect(names(merged_df1),names(extendedmeasurementorfact))
# Merge merged_df2 and extendedmeasurementorfact
rcmed.occ.df <- merge(merged_df1, extendedmeasurementorfact, all = TRUE,by=xx) 


rcmed.occ.df<- rcmed.occ.df %>%
  select(c(decimalLongitude,decimalLatitude,year,month,eventID,occurrenceID,identifiedBy,occurrenceStatus,measurementValue,minimumDepthInMeters,maximumDepthInMeters,scientificName))

rcmed.occ.df<-as.data.frame(subset(rcmed.occ.df,rcmed.occ.df$scientificName=='Caulerpa cylindracea'))

for(i in 1:nrow(rcmed.occ.df)){
  if(is.na(rcmed.occ.df[i,"measurementValue"])) {
    if(isTRUE(rcmed.occ.df[i,"occurrenceStatus"]=='present')) {
      rcmed.occ.df[i,'measurementValue'] <- "Category  1" 
    } else if(isTRUE(rcmed.occ.df[i,"occurrenceStatus"]=='absent')){
      rcmed.occ.df[i,'measurementValue'] <- "Category  0" 
    }
  }
}
occ.old<-rcmed.occ.df
rcmed.occ.df<-rcmed.occ.df %>% mutate(measurementValue = as.factor(as.numeric(gsub("Category  ", "", measurementValue))))


# Loop through each row in df1

cint<-intersect(names(rcmed.occ.df),names(event))
for (i in 1:nrow(rcmed.occ.df)) {
  # Find the matching row in df2 for the current eventID
  matching_row_index <- which(event$eventID == rcmed.occ.df$eventID[i], arr.ind = T)
  
  # If a matching row is found in df2
  if (length(matching_row_index) > 0) {
    # Fill missing values in df1 with values from the matching row in df2
    for (col in cint) {
      if (is.na(rcmed.occ.df[i, col])) {
        rcmed.occ.df[i, col] <- event[matching_row_index, col]
      }
    }
  } else { matching_row_index<-NULL}
}


rcmed.occ.df<-rcmed.occ.df[!is.na(rcmed.occ.df$year),]
occ.old<-rcmed.occ.df
#occ.rcmed<-rcmed.occ.df[,c("decimalLongitude","decimalLatitude","year","measurementValue")]

rcmed.occ.df<-rcmed.occ.df %>% select('decimalLongitude','decimalLatitude','year','measurementValue')
rcmed.sp <- vect(rcmed.occ.df,geom = c('decimalLongitude','decimalLatitude'),crs = 'EPSG:4326')
rcmed.p <- project(rcmed.sp,eckertIV)
rcmed.df <- terra::extract(envs.final,rcmed.p, bind=T)
rcmed.df <- na.omit(as.data.frame(rcmed.df,geom='XY'))[,c('x','y','year','measurementValue')]
# flatten the presence and absences to sites, retaining the max value 
write.csv(rcmed.df,paste0(wd,'data/',expname,'_',"rcmed_abundance.csv"),row.names = F)
rcmed.sp <- vect(rcmed.df,geom = c('x','y'),crs = eckertIV)
rcmed.r <- terra::rasterize(rcmed.sp,envs.final,field='measurementValue',fun='max')
rcmed.df <- as.data.frame(rcmed.r,xy=TRUE)
colnames(rcmed.df)<-c('x','y','measurementValue')
write.csv(rcmed.df,paste0(wd,'data/',expname,'_',"rcmed_abundance_compact.csv"),row.names = F)

rcmed.points<-rcmed.df
