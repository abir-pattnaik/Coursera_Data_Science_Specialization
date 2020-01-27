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
