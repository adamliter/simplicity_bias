#Creating All Stimuli for Experiment
#This script takes info from the folder "materials/all_stimuli", and selects the appropriate test stimuli
#Exp1 had a confound with NewTestStimuli; the NewTestStimuli had consonant combinations that might
#(not necessarily) have been seen by a participant during training
#In Exp 2, the NewTestStimuli had consonant combinations that weren't there during training.
#In Exp 3, both NewWordStims and NewConsStims were there during test phase. That is, NewStims from both Exp1 and Exp 2.
#The TrainingStimuli (as in Exp. 2) still systematically avoided a particular "Type"

library(dplyr)
library(here)

directory = here("materials/vsh_exp3_stimuli/")

ExperimentStimuli = function(ListNum=1,TestNperType=12){
  Num_TrainingItems = 100                     #Items for familiarisations
  Num_OldTestItems = TestNperType             #Familiarised items tested
  Num_NewConsTestItems = TestNperType         #Unfamiliar consonant sequences following pattern
  Num_NewWordTestItems = TestNperType         #Unfamiliar words following pattern; This has previously seen C sequences
  Num_OneFeatureControls_Each = TestNperType  #Controls with one feature harmony (the count is for each one feature Control)
  Num_NoPatternControls = TestNperType        #Controls with no pattern at all

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
  #Getting new Cons combinations that follow the original pattern
  set.seed(123*ListNum)
  NewConsTestStimuli = PossibleNewTestStimuli %>%
    sample_n(size=Num_NewConsTestItems) %>%
    mutate(TYPE = "NewConsTestStimuli", GENERAL_TYPE="Test", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  #Getting Training Items
  set.seed(123*ListNum)
  TrainingStimuli = VoicingStopHarmony %>%
    filter(!(STIMULUS %in% PossibleNewTestStimuli$STIMULUS)) %>%
    sample_n(size=Num_TrainingItems) %>%
    mutate(TYPE = "TrainingItems", GENERAL_TYPE="Training", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))

  #Getting new Words (cons sequences heard during training) that follow the original pattern
  set.seed(123*ListNum)
  TrainingCombinations = unique(paste(TrainingStimuli$SEGMENT,TrainingStimuli$SEGMENT3,sep=""))
  NewWordTestStimuli = VoicingStopHarmony %>%
    mutate(ConsCombinations = paste(SEGMENT,SEGMENT3,sep="")) %>%
    filter(ConsCombinations %in% TrainingCombinations) %>%
    filter(!(STIMULUS %in% TrainingStimuli$STIMULUS)) %>%
    select(-ConsCombinations) %>%
    sample_n(size=Num_NewWordTestItems) %>%
    mutate(TYPE = "NewWordTestStimuli", GENERAL_TYPE="Test", SOUND=paste("recordings/",STIMULUS,".wav",sep=""))
  #TrainingStimuli %>% filter(TrainingStimuli$STIMULUS %in% NewWordTestStimuli$STIMULUS)
  #TrainingStimuli %>% filter(TrainingStimuli$STIMULUS %in% NewWordTestStimuli$STIMULUS)

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

  AllTestStimuli = rbind(OldTestStimuli,NewConsTestStimuli,NewWordTestStimuli,OneFeatureStop,OneFeatureVoicing,NoPatternControls)

  write.csv(TrainingStimuli, paste(directory,"TrainingStimuli_Exp3_List",ListNum,".csv",sep=""), row.names=F)
  write.csv(AllTestStimuli, paste(directory,"TestStimuli_Exp3_List",ListNum,".csv",sep=""), row.names=F)
}

#How many different training/testing stimuli files
for(i in (1:100))
  ExperimentStimuli(ListNum=i,TestNperType=10)

#*********************
#CleanUp
rm(list=ls())