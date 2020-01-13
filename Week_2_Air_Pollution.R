#Link for the assignment details
#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

pollutantmean<-function(directory,pollutant,id=1:332){
  path_directory<-paste("XXXXXXX",directory,sep="/") #Put your path details in the first argument
  files_list<-list.files(path_directory)
  id_count=length(id)
  i=id[1]
  air_pollution<-data.frame()
  while (id_count!=0) {
    path_directory_temp<-paste(path_directory,files_list[i],sep="/")
    read_data_temp<-read.csv(path_directory_temp,header=T)
    air_pollution<-rbind(air_pollution,read_data_temp)
    id_count=id_count-1
    i=i+1
  }
  mean(air_pollution[[pollutant]],na.rm = TRUE)
  
}
