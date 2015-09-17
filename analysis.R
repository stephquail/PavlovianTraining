## Data extraction and analysis from the feature negative conditioned inhibition training. 

# Extract:
#   - Magazine Checks: Participant checking to see if any snacks dropped by vending machine
#   - Outcome Collected: Participant collecting dropped snack
#   - Excitatory Cues Onsets
#   - Compound Inhibitorhy Cues Onsets

# CS durations 5s (Outcome onset 2s after CS onset, duration 3s (or until collected))
# ITI duration 30s


# Load libraries needed for this analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)
library(stringr)

# Load functions for this analysis
source("R/functions.R")

#Vectors of participant data paths, and IDs created in functions
#Create a list of participant info vectors

ID <- list(CI101, CI102, CI103, CI104, CI105, CI106, CI107)

#Create Empty Vectors
#These empty vectors will be filled in with individual participant information as the analysis loops through each participant
#The third item in each participant vector corresponds to the row that partcipants' data will be inserted into the empty vectors. 

participant <- character(length = length(ID)) # creates vectors with as many elements in them as the number of participantin in the ID vector

exc.ITI.sec <- createDF(y=ID, z=14)
in.ITI.sec <- createDF(y=ID, z=14)

exc.cs.sec <- createDF(y=ID, z=14)
in.cs.sec <- createDF(y=ID, z=14)

### EXTRACT DATA FROM EACH PARTICIPANTS LOGFILE
for(i in ID){
  data <- read.delim(i[[1]], header = FALSE) # read the log files stored in the data folder
  
  #give the data meaingful column headings
  colnames(data) <- c("time", "type", "text")
  
  #Extract the TIME value for each time a magazine check response is made
  magCheckTimes <- findTime("magazine check")
  
  # Extract the TIME value for each time the participant collects the 
  oCollectTimes <- findTime("outcome collected")
  
  #Extract the TIME values for the onset of the cues
  #Excitor onsets
  
  v1.times<- findTime("PatchStim: tex = vend_v1.png")
  v2.times <- findTime("PatchStim: tex = vend_v2.png")
  
  #Inhibitor onsets
  
  v1v3.times <- findTime("PatchStim: tex = vend_v1v3.png")
  v2v4.times <- findTime("PatchStim: tex = vend_v2v4.png")
  
  #Find the PreCS-start and CS-end points
  v1.points <- csPoints("v1")
  v2.points <- csPoints("v2")
  
  v1v3.points <- csPoints("v1v3")
  v2v4.points <- csPoints("v2v4")
  
  #Count the number of magazine checks in the Pre-CS and CS periods 
  
  v1.checks <- lapply(v1.points, countResp, y = magCheckTimes)
  v2.checks <- lapply(v2.points, countResp, y = magCheckTimes)
  v1v3.checks <- lapply(v1v3.points, countResp, y = magCheckTimes)
  v2v4.checks <- lapply(v2v4.points, countResp, y = magCheckTimes)
  
  #Find the ITI start and CS-end points
  
  v1.long.points <- csLongPoints("v1")
  v2.long.points <- csLongPoints("v2")
  v1v3.long.points <- csLongPoints("v1v3")
  v2v4.long.points <- csLongPoints("v2v4")
  
  #Count number of magazine checks in the ITI and CS periods
  
  v1.long.checks <- lapply(v1.long.points, countResp, y = magCheckTimes)
  v2.long.checks <- lapply(v2.long.points, countResp, y = magCheckTimes)
  v1v3.long.checks <- lapply(v1v3.long.points, countResp, y = magCheckTimes)
  v2v4.long.checks <- lapply(v2v4.long.points, countResp, y = magCheckTimes)
  
  ##Collapse within cue types
  
  exc.longITI.checks <- colMeans( # find the column means
    rbind( # bind the two vectors together by their rows
      sapply(v1.long.checks, "[", c(1)), 
      sapply(v2.long.checks, "[", c(1)))) # extract the first element from each vector in the list (i.e. preCS responding) for each of the two excitatory cues (v1, v2)
  
  in.longITI.checks <- colMeans(
    rbind(
      sapply(v1v3.long.checks, "[", c(1)), 
      sapply(v2v4.long.checks, "[", c(1)))) # does the same extraction and averaging as for the excitors. With inhibtiory cues (v1v3, v2v4)
  
  exc.cs.checks <- colMeans(
    rbind(
      sapply(v1.long.checks, "[", c(2)), 
      sapply(v2.long.checks, "[", c(2)))) # extracts and averages the CS responding for the excitatory cues
  
  in.cs.checks <-  colMeans(
    rbind(
      sapply(v1v3.long.checks, "[", c(2)), 
      sapply(v2v4.long.checks, "[", c(2)))) # exctacts and averages the CS responding for the inhibtiory cues
  
  ## Convert checking into checks /s
  # For Long IT checking /30
  # For Short PreCS and CS checking /5
  
  i.exc.ITI.sec <- exc.longITI.checks/30
  i.in.ITI.sec <- in.longITI.checks/30
  
  i.exc.cs.sec <- exc.cs.checks/5
  i.in.cs.sec <- in.cs.checks/5
  
  # Insert adjusted checking rates into empty vectors
  participant[as.numeric(i[3])] <- i[[2]]
  exc.ITI.sec[as.numeric(i[3]),] <- i.exc.ITI.sec
  in.ITI.sec[as.numeric(i[3]),] <- i.in.ITI.sec
  
  exc.cs.sec[as.numeric(i[3]),] <- i.exc.cs.sec
  in.cs.sec[as.numeric(i[3]),] <- i.in.cs.sec
}

group.checks <- list(exc.ITI.sec, exc.cs.sec, in.ITI.sec, in.cs.sec)
group.names <- c("exc.ITI", "exc.cs", "in.ITI", "in.cs")
names(group.checks) <- group.names

#Wide data for ITI and CS checking for Excitors and Inhibitors

trial.ids <- c("ID", 1:14)
group.checks <- lapply(group.checks, addID, id = participant, cols = trial.ids)
wide.group.checks <- do.call("rbind", group.checks)
rowtype <- c(rownames(wide.group.checks))
wide.group.checks <- data.frame(wide.group.checks, rowtype, row.names = NULL)
colnames(wide.group.checks) <- c("ID", 1:14, "rowtype")

#Convert wide data to long data format (to allow ggplot graphing)

long.group.checks <- melt(wide.group.checks, 
                          id.vars = c("ID", "rowtype"),
                          variable.name = "trial",
                          value.name = "checks")
long.group.checks$factor <- str_split_fixed(long.group.checks$rowtype, "\\.",3)
long.group.checks$cues <- as.factor(long.group.checks$factor[,1])
long.group.checks$time <- as.factor(long.group.checks$factor[,2])
long.group.checks$end <- as.factor(long.group.checks$factor[,3])

#drop unneded columns
long.group.checks$factor <- NULL
long.group.checks$rowtype <- NULL
long.group.checks$end <- NULL

long.group.checks <- within(long.group.checks,{
  cue_time <- do.call(paste, c(long.group.checks[c("cues", "time")], sep = "."))
  cue_time <- as.factor(cue_time)
})

##GRAPH Trial DATA
#set up vectors for formatting fucntion
linecols <- c("red2", "red2", "dodgerblue", "dodgerblue", "green3")
shapecols <- c("red2", "white", "dodgerblue", "white", "green3")
shapes <- c(21, 21, 24, 24, 23)
lines <- c(1,5,1,5)

group.checks_WS <-  summarySEwithin(long.group.checks, measurevar="checks", withinvars= c("cues", "time", "cue_time", "trial"), idvar="ID", na.rm=FALSE, conf.interval=.95)


#Graph that actually (almost) does what I want.
#Split by colour on type of cue (excitor vs. inhibitor)
#Line type by time (ITI vs. CS)
group.checks.graph <- ggplot(data = group.checks_WS, aes(x=trial, y=checks, group= cue_time, colour = cues)) +
  geom_point(size = 2.5) + 
  geom_line(aes(linetype = time), size = 1.1) +
  scale_linetype_manual(values = lines) +
  xlab("Trial") + ylab("Magazine Checks (/s)")

