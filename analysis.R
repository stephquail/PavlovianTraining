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

v1.checks <- countResp(v1.points, magCheckTimes)
