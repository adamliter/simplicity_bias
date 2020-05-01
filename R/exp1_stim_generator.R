#Creating All Stimuli for Experiment
#This script takes info from the folder "materials/all_stimuli", and selects the appropriate test stimuli
library(here)
library(dplyr)

directory = here("materials/vsh_exp1_stimuli/")

ExperimentStimuli = function(ListNum=1){
  Num_TrainingItems = 100     #Items for familiarisations
  Num_OldTestItems = 12       #Familiarised items tested
  Num_NewTestItems = 12       #Unfamiliar items following pattern
  Num_OneFeatureControls_Each = 12 #Controls with one feature harmony (the count is for each one feature Control)
  Num_NoPatternControls = 12  #Controls with no pattern at all

  #Opening relevant files
  VoicingStopHarmony = read.csv(here("materials/all_stimuli/VoicingStopHarmony.csv"))
  VoicingHarmony = read.csv(here("materials/all_stimuli/VoicingHarmony.csv"))
  StopHarmony = read.csv(here("materials/all_stimuli/StopHarmony.csv"))
  NoPattern = read.csv(here("materials/all_stimuli/NoPattern.csv"))
  head(VoicingStopHarmony)


  #Just for getting the other parts fixed - not the final list
  set.seed(123*ListNum)
  NewTestStimuli = VoicingStopHarmony %>%
    sample_n(size=Num_NewTestItems) %>%
    mutate(TYPE = "NewTestStimuli", GENERAL_TYPE="Test", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  #Getting Training Items
  set.seed(123*ListNum)
  TrainingStimuli = VoicingStopHarmony %>%
    filter(!(STIMULUS %in% NewTestStimuli$STIMULUS)) %>%
    sample_n(size=Num_TrainingItems) %>%
    mutate(TYPE = "TrainingItems", GENERAL_TYPE="Training", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))


  #Getting OldTestItems
  set.seed(123*ListNum)
  OldTestStimuli = TrainingStimuli %>%
    sample_n(size=Num_OldTestItems) %>%
    mutate(TYPE = "OldTestStimuli", GENERAL_TYPE="Test", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  #Getting No Pattern Items (Random Controls)
  set.seed(123*ListNum)
  NoPatternControls = NoPattern %>%
    sample_n(size=Num_NoPatternControls) %>%
    mutate(TYPE = "NoPatternControls", GENERAL_TYPE="Controls", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  #One-feature harmony controls
  set.seed(123*ListNum)
  OneFeatureVoicing = VoicingHarmony %>%
    filter(!(STIMULUS %in% VoicingStopHarmony$STIMULUS)) %>%
    sample_n(size=Num_OneFeatureControls_Each) %>%
    mutate(TYPE = "VoicingControls", GENERAL_TYPE="Controls", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  set.seed(123*ListNum)
  OneFeatureStop = StopHarmony %>%
    filter(!(STIMULUS %in% VoicingStopHarmony$STIMULUS)) %>%
    sample_n(size=Num_OneFeatureControls_Each) %>%
    mutate(TYPE = "StopControls", GENERAL_TYPE="Controls", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  AllTestStimuli = rbind(OldTestStimuli,NewTestStimuli,OneFeatureStop,OneFeatureVoicing,NoPatternControls)

  write.csv(TrainingStimuli, paste(directory,"TrainingStimuli_List",ListNum,".csv",sep=""), row.names=F)
  write.csv(AllTestStimuli, paste(directory,"TestStimuli_List",ListNum,".csv",sep=""), row.names=F)
}

#How many different training/testing stimuli files
for(i in (1:40))
  ExperimentStimuli(ListNum=i)

#*********************
#CleanUp
rm(list=ls())