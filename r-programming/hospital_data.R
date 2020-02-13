path_outcome<-'XXXXXXXX/outcome-of-care-measures.csv'#Data read


best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome_data<-read.csv(path_outcome, colClasses = "character")
  if (!(state %in% unique(outcome_data$State))) stop("invalid state")
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome")
  #print('Checks completed')
  subset_data<-outcome_data[,c(2,7,11,17,23)]
  if (outcome=='heart attack'){
    #print('in here')
    data_HA<-subset_data[,c(1,2,3)]
    state_HA<-data_HA[data_HA$State==state,]
    #print(state_HA)
    min_value<-min(as.numeric(state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),na.rm = TRUE)
    print(state_HA[state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min_value,]$Hospital.Name)
  }
  if (outcome=='heart failure'){
    data_HF<-subset_data[,c(1,2,4)]
    state_HF<-data_HF[data_HF$State==state,]
    min_value<-min(as.numeric(state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),na.rm = TRUE)
    print(state_HF[state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min_value,]$Hospital.Name)
  }
  if (outcome=='pneumonia'){
    data_PN<-subset_data[,c(1,2,5)]
    state_PN<-data_PN[data_PN$State==state,]
    min_value<-min(as.numeric(state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),na.rm = TRUE)
    print(state_PN[state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min_value,]$Hospital.Name)
  }
  
}

rankhospital<-function(state,outcome,num_value){
  outcome_data<-read.csv(path_outcome, colClasses = "character")
  if (!(state %in% unique(outcome_data$State))) stop("invalid state")
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome")
  #print('checks completed')
  subset_data<-outcome_data[,c(2,7,11,17,23)]
  if (outcome=='heart attack'){
  data_HA<-subset_data[,c(1,2,3)]
  state_HA<-data_HA[data_HA$State==state,]
  state_HA<-state_HA[order(as.numeric(state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HA$Hospital.Name),]
  #View(state_HA)
  #state_HA[1,1]
  if (num_value=="best"){
    #print('here')
    print(state_HA[1,1])
  }
  else if (num_value=="worst"){
    state_HA<-state_HA[order(-as.numeric(state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HA$Hospital.Name),]
    #print('now here')
    print(state_HA[1,1])
  }
  else{
  print(state_HA[num_value,1])
  }
  }
  if (outcome=='heart failure'){
    data_HF<-subset_data[,c(1,2,4)]
    state_HF<-data_HF[data_HF$State==state,]
    state_HF<-state_HF[order(as.numeric(state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),state_HF$Hospital.Name),]
    #state_HA[1,1]
    #View(state_HF)
    if (num_value=="best"){
      #print('here')
      print(state_HF[1,1])
    }
    else if (num_value=="worst"){
      state_HF<-state_HF[order(-as.numeric(state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HF$Hospital.Name),]
      #print('now here')
      print(state_HF[1,1])
    }
    else{
    print(state_HF[num_value,1])
    }
  }
  if (outcome=='pneumonia'){
    data_PN<-subset_data[,c(1,2,5)]
    state_PN<-data_PN[data_PN$State==state,]
    state_PN<-state_PN[order(as.numeric(state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),state_PN$Hospital.Name),]
    #state_HA[1,1]
    #View(state_PN)
    if (num_value=="best"){
      #print('here')
      print(state_PN[1,1])
    }
    else if (num_value=="worst"){
      state_PN<-state_PN[order(-as.numeric(state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),state_PN$Hospital.Name),]
      #print('now here')
      print(state_PN[1,1])
    }
    else{
    print(state_PN[num_value,1])
    }
  }
}


rankall<-function(outcome,num_value="best"){
  ## Read outcome data
  outcome_data<-read.csv(path_outcome, colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% unique(outcome_data$State))) stop("invalid state")
  if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  subset_data<-outcome_data[,c(2,7,11,17,23)]
  total_states_count<-length(unique(outcome_data$State))
  #order(unique(outcome_data$State)
  i=1
  result_df=data.frame()
  while(total_states_count!=0){
    state=sort(unique(outcome_data$State))[i]
  if (outcome=='heart attack'){
    data_HA<-subset_data[,c(1,2,3)]
    state_HA<-data_HA[data_HA$State==state,]
    state_HA<-state_HA[order(as.numeric(state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HA$Hospital.Name),]
    if (num_value=="best"){
      #print(state_HA[1,c(1,2)])
      result_df<-rbind(result_df,state_HA[1,c(1,2)])
    }
    else if (num_value=="worst"){
      state_HA<-state_HA[order(-as.numeric(state_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HA$Hospital.Name),]
      result_df<-rbind(result_df,state_HA[1,c(1,2)])
      #print(state_HA[1,1])
    }
    else{
      result_df<-rbind(result_df,state_HA[num_value,c(1,2)])
      #print(state_HA[num_value,1])
    }
  }
  if (outcome=='heart failure'){
    data_HF<-subset_data[,c(1,2,4)]
    state_HF<-data_HF[data_HF$State==state,]
    state_HF<-state_HF[order(as.numeric(state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),state_HF$Hospital.Name),]
    if (num_value=="best"){
      result_df<-rbind(result_df,state_HF[1,c(1,2)])
      #print(state_HF[1,1])
    }
    else if (num_value=="worst"){
      state_HF<-state_HF[order(-as.numeric(state_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),state_HF$Hospital.Name),]
      result_df<-rbind(result_df,state_HF[1,c(1,2)])
      #print(state_HF[1,1])
    }
    else{
      result_df<-rbind(result_df,state_HF[num_value,c(1,2)])
      #print(state_HF[num_value,1])
    }
  }
  if (outcome=='pneumonia'){
    data_PN<-subset_data[,c(1,2,5)]
    state_PN<-data_PN[data_PN$State==state,]
    state_PN<-state_PN[order(as.numeric(state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),state_PN$Hospital.Name),]
    if (num_value=="best"){
      result_df<-rbind(result_df,state_PN[1,c(1,2)])
      #print(state_PN[1,1])
    }
    else if (num_value=="worst"){
      state_PN<-state_PN[order(-as.numeric(state_PN$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),state_PN$Hospital.Name),]
      result_df<-rbind(result_df,state_PN[1,c(1,2)])
      #print(state_PN[1,1])
    }
    else{
      result_df<-rbind(result_df,state_PN[num_value,c(1,2)])
      #print(state_PN[num_value,1])
    }
  }
    total_states_count=total_states_count-1
    i=i+1
  }
  colnames(result_df) <- c('hospital','state')
  result_df
  
}
