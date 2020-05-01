#Creating All Stimuli for Experiment
#This script takes info from the folder "materials/all_stimuli", and selects the appropriate test stimuli
#Exp1 had a confound with NewTestStimuli; the NewTestStimuli had consonant combinations that might
#(not necessarily) have been seen by a participant during training
#To avoid this confound, in Exp 2, the NewTestStimuli had consonant combinations that weren't there during training.
#Furthermore, to avoid making it easy for the subject NewTestStimuli could not have identical consonants
#(since they would say yes to that pattern based on their experience with the training)
#As a result of these choices, the NewTestStimuli could only have consonant combinations that were the same "Type".
#For example, sVfV or fVsV (look at UniqueConsonantCombinations.csv)
#So, the TrainingStimuli systematically avoided a particular "Type"

library(dplyr)
library(here)

#Directory where files will be stored
directory = here("materials/vsh_exp2_stimuli/")

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
  UniqueConsonantCombinations = read.csv(here("materials/all_stimuli/UniqueConsonantCombinations.csv"))
  head(VoicingStopHarmony)

  #Choosing the consonants/stimuli that will be part of NewTestStimuli
  ConsonantType = UniqueConsonantCombinations[UniqueConsonantCombinations$Type == sample(unique(UniqueConsonantCombinations$Type),1),]
  PossibleNewTestStimuli = VoicingStopHarmony %>%
                            filter((SEGMENT == ConsonantType$SEGMENT[1] & SEGMENT3 == ConsonantType$SEGMENT3[1]) |
                                   (SEGMENT == ConsonantType$SEGMENT[2] & SEGMENT3 == ConsonantType$SEGMENT3[2]))

  #Just for getting the other parts fixed - not the final list
  set.seed(123*ListNum)
  NewTestStimuli = PossibleNewTestStimuli %>%
    sample_n(size=Num_NewTestItems) %>%
    mutate(TYPE = "NewTestStimuli", GENERAL_TYPE="Test", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))

  #Getting Training Items
  set.seed(123*ListNum)
  TrainingStimuli = VoicingStopHarmony %>%
    filter(!(STIMULUS %in% PossibleNewTestStimuli$STIMULUS)) %>%
    sample_n(size=Num_TrainingItems) %>%
    mutate(TYPE = "TrainingItems", GENERAL_TYPE="Training", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))


  #Getting OldTestItems
  set.seed(123*ListNum)
  OldTestStimuli = TrainingStimuli %>%
    sample_n(size=Num_OldTestItems) %>%
    mutate(TYPE = "OldTestStimuli", GENERAL_TYPE="Test", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))

  #Getting No Pattern Items (Random Controls)
  set.seed(123*ListNum)
  NoPatternControls = NoPattern %>%
    sample_n(size=Num_NoPatternControls) %>%
    mutate(TYPE = "NoPatternControls", GENERAL_TYPE="Controls", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))

  #One-feature harmony controls
  set.seed(123*ListNum)
  OneFeatureVoicing = VoicingHarmony %>%
    filter(!(STIMULUS %in% VoicingStopHarmony$STIMULUS)) %>%
    sample_n(size=Num_OneFeatureControls_Each) %>%
    mutate(TYPE = "VoicingControls", GENERAL_TYPE="Controls", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))

  set.seed(123*ListNum)
  OneFeatureStop = StopHarmony %>%
    filter(!(STIMULUS %in% VoicingStopHarmony$STIMULUS)) %>%
    sample_n(size=Num_OneFeatureControls_Each) %>%
    mutate(TYPE = "StopControls", GENERAL_TYPE="Controls", SOUND=paste("Recordings/",STIMULUS,".wav",sep=""))

  AllTestStimuli = rbind(OldTestStimuli,NewTestStimuli,OneFeatureStop,OneFeatureVoicing,NoPatternControls)

  write.csv(TrainingStimuli, paste(directory,"TrainingStimuli_Exp2_List",ListNum,".csv",sep=""), row.names=F)
  write.csv(AllTestStimuli, paste(directory,"TestStimuli_Exp2_List",ListNum,".csv",sep=""), row.names=F)
}

#How many different training/testing stimuli files
for(i in (71:150))
  ExperimentStimuli(ListNum=i)

#*********************
#CleanUp
rm(list=ls())