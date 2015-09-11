## Functions used in the data extraction and analysis of Pavlovian Conditioned Inhibition
#
# Create vectors for each participant
# Each ID Vector consists of:
#   - Path to data file
#   - ID code
#   - participant number (to insert data into appropriate row when looping through participants for analysis)

CI101 <- c("R/data/CI101.log", "CI101", 1)

#find times of text strings
findTime <- function(x){
  data$time[data$text == x]
}

#find position of an element in a vector
findPos <- function(x){
  which(data$text == x)
}

#calculates CS end time (5s after CSonset)
endTime <- function(x){
  x + 5
}

#calculates preCS start time (5s before CSonset)
preTime <- function(x){
  x - 5
}

#timepoints for each CStrial
csPoints <- function(x){
  assign(paste0(x,".ends"), lapply(get(paste0(x,".times")), endTime)) #get end times of each cs(5s after onset)
  assign(paste0(x, ".pres"), lapply(get(paste0(x, ".times")), preTime)) #get precs start times (5s before onset)
  assign(paste0(x, ".points"), mapply(get(paste0(x, ".pres")), get(paste0(x, ".times")), get(paste0(x, ".ends")), FUN = list, SIMPLIFY=FALSE))
}

#Counts the number of responses made in during the PreCS and CS periods
countResp <- function(x, y){
  c(sum(y > x[1] & y < x[2]), sum(y > x[2] & y < x[3]))
}
