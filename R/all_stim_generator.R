library(here)

vowels <- read.csv(here("materials/all_stimuli/Vowels.csv"))
consonants <- read.csv(here("materials/all_stimuli/Consonants.csv"))

#Combines segments into a string
combinations=function(x,y){
  return(paste(x,y,sep=""))
}

#Creates the whole word
Words=function(consonants,vowels){
  consonants=as.vector(outer(consonants,consonants,combinations))
  #outer gets you all combinations of the two vectors -  the function here being concatenation.
  vowels=as.vector(outer(vowels,vowels,combinations))
  lists=as.vector(outer(consonants,vowels,combinations))
  return(paste(substr(lists,1,1),substr(lists,3,3),substr(lists,2,2),substr(lists,4,4), sep=""))
}

#Creating Consonant combinations - without identical consonants
UniqueConsonant_Combiner=function(consonants){
  #consonants=consonants$SEGMENT
  consonants1=as.vector(outer(consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "no"],
                              consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "no"],combinations))
  consonants2=as.vector(outer(consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "yes"],
                              consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "yes"],combinations))
  consonants3=as.vector(outer(consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "yes"],
                              consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "yes"],combinations))
  consonants4=as.vector(outer(consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "no"],
                              consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "no"],combinations))
  consonants_all = c(consonants1, consonants2, consonants3, consonants4)
  #print(consonants_all)
  return(consonants_all)
}


#Voicing harmony
VoicingHarmony=sort(c(Words(consonants$SEGMENT[consonants$VOICING == "no"],vowels$SEGMENT),
  Words(consonants$SEGMENT[consonants$VOICING == "yes"],vowels$SEGMENT)))

#Stop harmony
StopHarmony=sort(c(Words(consonants$SEGMENT[consonants$STOP == "no"],vowels$SEGMENT),
                      Words(consonants$SEGMENT[consonants$STOP == "yes"],vowels$SEGMENT)))

#Voicing & Stop harmony
VoicingStopHarmony=sort(c(Words(consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "no"],vowels$SEGMENT),
                      Words(consonants$SEGMENT[consonants$VOICING == "no" & consonants$STOP == "yes"],vowels$SEGMENT),
                      Words(consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "no"],vowels$SEGMENT),
                      Words(consonants$SEGMENT[consonants$VOICING == "yes" & consonants$STOP == "yes"],vowels$SEGMENT)))

#All Words and then non-pattern words only
AllWords=Words(consonants$SEGMENT,vowels$SEGMENT)
NoPattern = AllWords[!(AllWords %in% VoicingHarmony) &
                     !(AllWords %in% StopHarmony)]

#Unique consonant combinations - For experiment 2
UniqueConsonantCombinations = UniqueConsonant_Combiner(consonants)
UniqueConsonantCombinations=data.frame(t(data.frame(sapply(UniqueConsonantCombinations,function(x)strsplit(x,"")))))
names(UniqueConsonantCombinations)=c("SEGMENT","SEGMENT3")
UniqueConsonantCombinations$Type = rep(1:4,each=4)
UniqueConsonantCombinations=filter(UniqueConsonantCombinations, SEGMENT != SEGMENT3)


#Splitting into segments
VoicingHarmony2=data.frame(t(data.frame(sapply(VoicingHarmony,function(x)strsplit(x,"")))), VoicingHarmony)
names(VoicingHarmony2)=c("SEGMENT","SEGMENT2","SEGMENT3","SEGMENT4","STIMULUS")
#head(VoicingHarmony2)
StopHarmony2=data.frame(t(data.frame(sapply(StopHarmony,function(x)strsplit(x,"")))), StopHarmony)
names(StopHarmony2)=c("SEGMENT","SEGMENT2","SEGMENT3","SEGMENT4","STIMULUS")
#head(VoicingHarmony2)
VoicingStopHarmony2=data.frame(t(data.frame(sapply(VoicingStopHarmony,function(x)strsplit(x,"")))), VoicingStopHarmony)
names(VoicingStopHarmony2)=c("SEGMENT","SEGMENT2","SEGMENT3","SEGMENT4","STIMULUS")
#head(VoicingStopHarmony2)
NoPattern2=data.frame(t(data.frame(sapply(NoPattern,function(x)strsplit(x,"")))), NoPattern)
names(NoPattern2)=c("SEGMENT","SEGMENT2","SEGMENT3","SEGMENT4","STIMULUS")
head(NoPattern2)
AllWords2=data.frame(t(data.frame(sapply(AllWords,function(x)strsplit(x,"")))), AllWords)
names(AllWords2)=c("SEGMENT","SEGMENT2","SEGMENT3","SEGMENT4","STIMULUS")
head(AllWords2)


#Merging first consonant info with stimuli
VoicingHarmony3=merge(consonants,VoicingHarmony2, id="SEGMENT")
StopHarmony3=merge(consonants,StopHarmony2, id="SEGMENT")
VoicingStopHarmony3=merge(consonants,VoicingStopHarmony2, id="SEGMENT")
NoPattern3=merge(consonants,NoPattern2, id="SEGMENT")
AllWords3=merge(consonants,AllWords2, id="SEGMENT")
#head(VoicingHarmony3)
#head(VoicingStopHarmony3)

#Writing the data.frames to files
write.csv(VoicingHarmony3, here("materials/all_stimuli/VoicingHarmony.csv"),row.names=F)
write.csv(StopHarmony3, here("materials/all_stimuli/StopHarmony.csv"),row.names=F)
write.csv(VoicingStopHarmony3, here("materials/all_stimuli/VoicingStopHarmony.csv"),row.names=F)
write.csv(NoPattern3, here("materials/all_stimuli/NoPattern.csv"),row.names=F)
write.csv(AllWords3, here("materials/all_stimuli/AllWords.csv"),row.names=F)
write.csv(UniqueConsonantCombinations, here("materials/all_stimuli/UniqueConsonantCombinations.csv"),row.names=F)

#Clean-up
rm(list=ls())