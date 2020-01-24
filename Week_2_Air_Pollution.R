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

##The below logic is not able to satisfy one of the cases,therfore needs to be looked at

complete<-function(directory,id=1:332){
  path_directory<-paste("XXXXXXX",directory,sep="/") #Put your path details in the first argument
  files_list<-list.files(path_directory)
  id_count<-length(id)
  i=1
  complete_cases<-data.frame()
  while(id_count!=0){
    path_directory_temp<-paste(path_directory,files_list[id[i]],sep="/")
    read_data_temp<-read.csv(path_directory_temp,header=T)
    complete_cases_temp<-as.data.frame(table(complete.cases(read_data_temp)))
    complete_cases_temp['ID']<-unique(read_data_temp$ID)
    complete_cases<-rbind(complete_cases,complete_cases_temp)
    id_count<-id_count-1
    i<-i+1
  }
  complete_cases_subset<-subset(complete_cases,Var1=='TRUE',select=c(Freq,ID))
  complete_cases_subset['nobs']<-complete_cases_subset$Freq
  final_result<-as.data.frame(complete_cases_subset[,c("ID","nobs")])
  final_result
}


##Second logic for complete function 

complete<-function(directory,id=1:332){
  path_directory<-paste("XXXXXXX",directory,sep="/") #Put your path details in the first argument
  files_list<-list.files(path_directory)
  id_count<-length(id)
  #print(id_count)
  i=1
  complete_cases<-data.frame()
  while(id_count!=0){
    path_directory_temp<-paste(path_directory,files_list[id[i]],sep="/")
    #print(path_directory_temp)
    read_data_temp<-read.csv(path_directory_temp,header=T)
    #print(read_data_temp)
    complete_cases_temp<-data.frame(id=id[i],nobs=sum(complete.cases(read_data_temp)))
    #print(sum(complete.cases(read_data_temp)))
    complete_cases<-rbind(complete_cases,complete_cases_temp)
    id_count<-id_count-1
    i<-i+1
  }
  complete_cases
}
