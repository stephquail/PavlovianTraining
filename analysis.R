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
library(grid)

# Load functions for this analysis
source("R/functions.R")

#Vectors of participant data paths, and IDs created in functions
#Create a list of participant info vectors

ID <- list(CI101)

#Create Empty Vectors
#These empty vectors will be filled in with individual participant information as the analysis loops through each participant
#The third item in each participant vector corresponds to the row that partcipants' data will be inserted into the empty vectors. 

participant <- character(length = length(ID)) # creates vectors with as many elements in them as the number of participantin in the ID vector

data <- read.delim("R/data/CI101.log", header = FALSE) # read the log files stored in the data folder

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

exc.longpre.checks <- colMeans( # find the column means
  rbind( # bind the two vectors together by their rows
    sapply(v1.long.checks, "[", c(1)), 
    sapply(v2.long.checks, "[", c(1)))) # extract the first element from each vector in the list (i.e. preCS responding) for each of the two excitatory cues (v1, v2)
in.longpre.checks <- colMeans(
  rbind(
    sapply(v1v3.long.checks, "[", c(1)), 
    sapply(v2v4.long.checks, "[", c(1)))) # does the same extraction and averaging as for the excitors. With inhibtiory cues (v1v3, v2v4)

exc.cs.checks <- colMeans(rbind(sapply(v1.long.checks, "[", c(2)), sapply(v2.long.checks, "[", c(2))))
in.cs.checks <-  colMeans(rbind(sapply(v1v3.long.checks, "[", c(2)), sapply(v2v4.long.checks, "[", c(2))))