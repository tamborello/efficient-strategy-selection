# Author: Frank Tamborello
# CC-BY-SA 2011 Frank Tamborello
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of Creative Commons Attribute-ShareAlike 4.0 International License: http://creativecommons.org/licenses/by-sa/4.0/
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
# Description: This is a script to import human subject data from a set of Microsoft Excel files from the Efficient Strategy Selection Microworld Experiment. The script then parses the data into forms suitable for plotting and analyses.


# Analyze data from the Efficient Strategy Selection (ESS) behavioral study


# Note: to adhere to the sum-to-zero convention for effect weights, you should always do this before running anovas in R:

options(contrasts=c("contr.sum","contr.poly"))

# Put the code right at the top of your script, run it each time you start a new R session.
# Of course, if you have some principled reason why you do not want to adhere to the sum-to-zero convention for effects weights, then by all means, use some other convention. The important thing to remember is that R, by default, does NOT adhere to the sum-to-zero convention, so if you fail to use the above code snippet, your results may not look like what you get in SPSS or in common texts on ANOVA.
# http://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/
# Good to know! Thanks!

# import the data for machine = EGI

setwd("/Users/EGI/Documents/fpt")



"
Why are subject numbers four digit numbers, some with Roman numeral suffixes? Do the Roman numerals encode condition, and if so, what condition does each Roman numeral encode?
"
essNames <- c("subject", "condition", "exp_time", "notes", "command", "time_of_day", "date")
setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/raw data/eff/")
effFilesList <- lapply(list.files(), read.delim, header = FALSE)

# give the data subject numbers and condition names
accum<-1 # initialize an accumulator
scList<-data.frame()
for (i in 1:29) { # there are 29 subjects in the eff cond
	l<-length(effFilesList[[i]][,1]) # l is the length of the current subject's data
	scList[accum:(accum+l-1),1]<-1000+i # set scList's first column, for as many rows as l, to 1000 + the current iteration (the subject number)
	accum<-accum+l # now we've set the subject number for this many rows of data
	}
scList[,2]<-"eff" # this will become the condition column
length(scList[,1])




"
 error: inconsistent number of columns!
 effFilesList[[5]] only has 4 columns! It has no notes, which ordinarily would be the second column. Maybe cbind another column in the second position? This also applies to subjs 8, 12, 14, 28
 So how do I add a column in a specific position? Maybe something like "

s1005<-cbind(effFilesList[[5]][1], character(length(effFilesList[[5]]$V1)), effFilesList[[5]][2:4])
names(s1005)<-c("V1", "V2", "V3", "V4", "V5")

s1008<-cbind(effFilesList[[8]][1], character(length(effFilesList[[8]]$V1)), effFilesList[[8]][2:4])
names(s1008)<-c("V1", "V2", "V3", "V4", "V5")

s1012<-cbind(effFilesList[[12]][1], character(length(effFilesList[[12]]$V1)), effFilesList[[12]][2:4])
names(s1012)<-c("V1", "V2", "V3", "V4", "V5")

s1014<-cbind(effFilesList[[14]][1], character(length(effFilesList[[14]]$V1)), effFilesList[[14]][2:4])
names(s1014)<-c("V1", "V2", "V3", "V4", "V5")

s1028<-cbind(effFilesList[[28]][1], character(length(effFilesList[[28]]$V1)), effFilesList[[28]][2:4])
names(s1028)<-c("V1", "V2", "V3", "V4", "V5")


effData<-rbind(effFilesList[[1]], effFilesList[[2]], effFilesList[[3]], effFilesList[[4]], s1005, effFilesList[[6]], effFilesList[[7]], s1008, effFilesList[[9]], effFilesList[[10]], effFilesList[[11]], s1012, effFilesList[[13]], s1014, effFilesList[[15]], effFilesList[[16]], effFilesList[[17]], effFilesList[[18]], effFilesList[[19]], effFilesList[[20]], effFilesList[[21]], effFilesList[[22]], effFilesList[[23]], effFilesList[[24]], effFilesList[[25]], effFilesList[[26]], effFilesList[[27]], s1028, effFilesList[[29]])

"
There's got to be a much simpler way to do the above, assuming all the subjects' data are in the effFilesList, rather than some having to be in their own objects: lapply? for loop?
"

write.table(effData[,1], file="effData.txt")
write.table(scList[,1], file="scList.txt")


effData<-cbind(scList, effData)
essNames->names(effData)
write.table(effData, file="effData.txt", sep="\t")

# Cool, now import the ineff and control conditions' data, add them into a data frame with the eff data, compute elapsed time bins, and run linear ANOVAs on that.

setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/raw data/ineff/")
ineffFilesList <- lapply(list.files(), read.delim, header = FALSE)

# Check for missing notes columns
row(ineffFilesList[[27]])

# Why does subject 2007 have six columns?!? It simply has "marker" for the second column in phases 1 & 3, but in phase 2 it has a duplicate of the command column. Subject 2012 has it, too.

ineffFilesList[[1]]<-subset(ineffFilesList[[1]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[1]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[7]]<-subset(ineffFilesList[[7]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[7]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[11]]<-subset(ineffFilesList[[11]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[11]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[12]]<-subset(ineffFilesList[[12]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[12]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[13]]<-subset(ineffFilesList[[13]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[13]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[24]]<-subset(ineffFilesList[[24]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[24]])<-c("V1", "V2", "V3", "V4", "V5")
ineffFilesList[[25]]<-subset(ineffFilesList[[25]], select=c(V1, V3, V4, V5, V6))
names(ineffFilesList[[25]])<-c("V1", "V2", "V3", "V4", "V5")



# Subject 2008 is missing his notes column
ineffFilesList[[8]]<-cbind(ineffFilesList[[8]][1], character(length(ineffFilesList[[8]]$V1)), ineffFilesList[[8]][2:4])
names(ineffFilesList[[8]])<-c("V1", "V2", "V3", "V4", "V5")

# There's got to be a better way to do this… check back with r-help later (2011-04-10 12:38)
"
ineffData<-rbind(ineffFilesList[[1]], ineffFilesList[[2]], ineffFilesList[[3]], ineffFilesList[[4]], ineffFilesList[[5]], ineffFilesList[[6]], ineffFilesList[[7]], ineffFilesList[[8]], ineffFilesList[[9]], ineffFilesList[[10]], ineffFilesList[[11]], ineffFilesList[[12]], ineffFilesList[[13]], ineffFilesList[[14]], ineffFilesList[[15]], ineffFilesList[[16]], ineffFilesList[[17]], ineffFilesList[[18]], ineffFilesList[[19]], ineffFilesList[[20]], ineffFilesList[[21]], ineffFilesList[[22]], ineffFilesList[[23]], ineffFilesList[[24]], ineffFilesList[[25]], ineffFilesList[[26]], ineffFilesList[[27]])
length(ineffData[,1])
"

# Jorge Ivan Velez says do.call(rbind, ineffFilesList) 2011-04-10 13:06
ineffData<-do.call(rbind, ineffFilesList)

# Philipp Pagel wrote as.data.frame(ineffFilesList)


# give the data subject numbers and condition names
# Watch out because there's no subject 2010!
accum<-1 # initialize an accumulator
scList1<-data.frame()
for (i in 1:9) { # there are 27 subjects in the ineff cond, but #2010 is inexplicably missing
	l<-length(ineffFilesList[[i]][,1]) # l is the length of the current subject's data
	scList1[accum:(accum+l-1),1]<-2000+i # set scList's first column, for as many rows as l, to 2000 + the current iteration (the subject number)
	accum<-accum+l # now we've set the subject number for this many rows of data
	}


accum<-1 # initialize an accumulator
scList2<-data.frame()
for (i in 11:28) { # there are 27 subjects in the ineff cond, but #2010 is inexplicably missing
	l<-length(ineffFilesList[[i-1]][,1]) # l is the length of the current subject's data
	scList2[accum:(accum+l-1),1]<-2000+i # set scList's first column, for as many rows as l, to 2000 + the current iteration (the subject number)
	accum<-accum+l # now we've set the subject number for this many rows of data
	}

scList<-rbind(scList1, scList2)
scList[,2]<-"ineff" # this will become the condition column
length(scList[,1])

ineffData<-cbind(scList, ineffData)
essNames->names(ineffData)
write.table(ineffData, file="ineffData.txt", sep="\t")


# show just the first ten rows of subject 2008's data, all columns
# ineffData[ineffData$subject==2008,][1:10,]






setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/raw data/control/")
contFilesList <- lapply(list.files(), read.delim, header = FALSE)



# Check for missing notes columns
row(contFilesList[[14]])
# All fourteen control subjects' files have five columns! Amazing!



contData<-do.call(rbind, contFilesList)

# give the data subject numbers and condition names
accum<-1 # initialize an accumulator
scList<-data.frame()
for (i in 1:14) { # iterate for the length of contData
	l<-length(contFilesList[[i]][,1]) # l is the length of the current subject's data
	scList[accum:(accum+l-1),1]<-5000+i # set scList's first column, for as many rows as l, to 5000 + the current iteration (the subject number)
	accum<-accum+l # now we've set the subject number for this many rows of data
	}


scList[,2]<-"control" # this will become the condition column
length(scList[,1])

contData<-cbind(scList, contData)
essNames->names(contData)
write.table(contData, file="contData.txt", sep="\t")

Data<-rbind(effData, ineffData, contData)
setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/")
write.table(Data, file="Data.txt", sep="\t")




# compute phases
# The only clue I have as to phases is when the command column's value is "Start Phase" or "End Phase". Although some subjects, such as 5002, are missing an End Phase statement! So presumably I could iterate phase each time I encounter "Start Phase", then reset the phase count each time I encounter a new subject number.

phase<-0
Data<-data.frame(Data, phase)
write.table(Data, file="Data.txt", sep="\t")
rm(phase)


# parse for phase
phs<-0 # initialize phs
sbj<-Data$subject[1] # initialize sbj to the first subject number
for (i in 1:length(Data[,1])) { # do this for each row of the data
	if (Data$subject[i]!=sbj) # if the current subject number is not equal to sbj…
	{phs<-0 # then reset phs…
		sbj<-Data$subject[i]} # & set sbj to the new subject number
	Data[i,]$phase<-ifelse(Data[i,]$command=="Start Phase", # if the current command value is "Start Phase"…
	{phs<-phs+1 # then increment phs
		phs}, # and set the current phase to the new value of phs
	phs)} # else set the current phase to the old value of phs
rm(i, sbj, phs) # clean-up

	
data.frame(Data$command[1:196], Data$phase[1:196])->whatHappened
whatHappened
whatHappened5002<-data.frame(Data$command[Data$subject==5002], Data$phase[Data$subject==5002])
whatHappened5002


levels(factor(Data$phase))
[1] "0" "1" "2" "3" "4" "5" "6"
# uh, why did that happen? This is why:
899    1006       eff 0:29:33.87                  End Phase   13:26:16   Nov 01, 2006     2
900    1006       eff 0:29:38.28                Start Phase   13:26:21   Nov 01, 2006     3
901    1006       eff 0:29:45.07                Start Phase   13:26:28   Nov 01, 2006     4
# Extra experiment logger button press? I guess I'll just have to delete all such events. Let's say that only the one right before the data stays. In other words, row 900 should be deleted.
# bookmark 2011-04-16 03:09
Data[Data$phase>3|Data$phase<1,c(1,5,8)]




# Any other weirdnesses I should lookout for?
# Yes, make certain every subject has 3 phases coded 1, 2, and 3.

Data[Data$phase==0,c(1,5)] # Some of these command values look like they should actually be notes or times!

Data[,c(1,5,8)] # Find spots, probably entire subjects, where command doesn't have the values encoding what the participant did.
commandTrouble<-rbind(Data[Data$subject==2008,], Data[Data$subject==2026,])
# Columns subject, cond, & exp_time all seem fine.
# notes contains nothing for s2008, commands for s2026.
# command contains notes for s2008, time_of_day for s2026.
# time_of_day contains commands for s2008, dates, for s2026
# date contains date and time_of_day for s2008, NA for 2026
# phase contains phase for s2008 & s2026
# Also there's no "Start Phase" for phase 1 of s2026!
# Where did the notes go for s2026?
# The Excel sheet for s2008 lumps date and time_of_day into the same cells, in that order! I guess I don't really care about those two, but it's good to be aware of.
# Fix the column FUBARs for these two subjects, then insert a "Start Phase" row for phase 1 of subject 2026. This should be row 8688 in the new data frame.

# subject 2008: Move the current contents of command and time_of_day to the next lesser column. Except that R won't let me change the data between factors, so I'm going to have to re-import s2008's data (& s2026's, too), fix them, then rbind them in place of the old data.
setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/raw data/ineff/")
s2008<-read.delim("2008.txt", header = FALSE)
# add a dummy column to s2008 to make it work with phase parsing
s2008<-cbind(s2008, numeric(length(s2008[1,])))
# re-add the subject & cond data
s2008<-cbind(rep(2008, times=length(s2008[,1])), rep("ineff", times=length(s2008[,1])), s2008, numeric(length(s2008[,1])))
names(s2008)<-c(essNames, "phase")
Data2<-rbind(Data[Data$subject<2008,], s2008, Data[Data$subject>2008,])
"
==>
Warning message:
In `[<-.factor`(`*tmp*`, ri, value = c(0, 0, 0, 0, 0, 0, 0, 0, 0,  :
  invalid factor level, NAs generated
Does it not like how s2008 has time & date in its time_of_day factor and 0s in date?
"
s2008$date<-substr(s2008$time_of_day,1,7)
s2008$time_of_day<-substr(s2008$time_of_day,9,13)
Data2<-rbind(Data[Data$subject<2008,], s2008, Data[Data$subject>2008,]) # Now it works. Nice!


# subject 2026:
s2026<-read.delim("2026.txt", header = FALSE)
# Add the "Start Phase" row for phase 1 of subject 2026. 
s2026<-rbind(cbind(NA, NA, "Start Phase", NA, NA, NA), s2026)
# For whatever reason there's an extra, sixth, column. Convert it from NA to numeric so it can be used for the phase factor.
s2026[,6]<-numeric(length(s2026[,1]))
# Add subject & condition columns
s2026<-cbind(rep(2026, times=length(s2026[,1])), rep("ineff", times=length(s2026[,1])), s2026)
# names must match before I can join s2026 to the rest of the data
names(s2026)<-c(essNames, "phase")
# rbind s2026 to the rest of the data
Data3<-rbind(Data2[Data2$subject<2026,], s2026, Data2[Data2$subject>2026,])

# Change "exp_time" factor's name to "elapsed_time". I think that makes much more sense.
names(Data3)<-c("subject", "condition", "elapsed_time", "notes", "command", "time_of_day", "date", "phase")

setwd("/Users/frank/ESS/Manuscript/HCI ms Revision/analyses/")
write.table(Data3, "Data.txt", sep="\t")
Data<-Data3
rm(Data2, Data3)

# Re-parse for phase.
# Done, now check it.




Data[Data$subject==2026,c(1,5,8)] # Subject 2026 parsed into phases
Data[Data$subject==2008,c(1,5,8)] # Subject 2008 parsed into phases


Data[Data$phase<1,c(1,5,8)] # Subjects 2008, 2003, & 1010 seem to have this problem
length(Data[Data$phase<1 & Data$subject==1010,1])
Data[Data$subject==1010 | Data$subject==2003,c(1,5,8)]
# Subjects 1010 & 2003 parsed into phase values 0, 1, & 2, rather than 1, 2 & 3 because they have no start phase event at the start of phase 1.

# Add the "Start Phase" row for phase 1 of subjects 1010 & 2003. 
s1010<-data.frame(1010, "eff", NA, NA, "Start Phase", NA, NA, 0)
names(s1010)<-names(Data)
Data<-rbind(Data[Data$subject<1010,], s1010, Data[Data$subject>=1010,])
rm(s1010)
# tested that with the phase parser, it worked perfectly.

s2003<-data.frame(2003, "ineff", NA, NA, "Start Phase", NA, NA, 0)
names(s2003)<-names(Data)
Data<-rbind(Data[Data$subject<2003,], s2003, Data[Data$subject>=2003,])
rm(s2003)



Data[Data$subject==1009 | Data$subject==2002,c(1,5,8)]
# Subject 1009 seems to be missing his phase 3 start phase event!
# There's nothing unusual immediately apparent about subject 2002.
Data[Data$subject==1009, c(1,5,8) # s1009's phase 3 start event should be in the new row 1405 of Data
s1009<-data.frame(1009, "eff", NA, NA, "Start Phase", NA, NA, 0)
names(s1009)<-names(Data)
Data2<-rbind(Data[1:1404,], s1009, Data[1405:dim(Data)[1],])
Data<-Data2
rm(s1009, Data2)

> levels(factor(Data$subject[Data$phase>3]))
[1] "1006" "2009" "5003"
Data[Data$subject==1006,c(1,5,8)]
Data<-rbind(Data[1:899,], Data[901:dim(Data)[1],])

write.table(Data, file="Data.txt", sep="\t")

Data[Data$subject==2009, c(1,5,8)] # Not only does s2009 have three extra start phase events (rows 6092, 6095, & 6097), but there are no end phase events for any phase but the third!


s2009<-read.delim(file="raw data/ineff/2009.txt", sep="\t", header=FALSE)
s2009<-cbind(rep(2009, times=dim(s2009)[1]), rep("ineff", times=dim(s2009)[1]), s2009, numeric(dim(s2009)[1]))
names(s2009)<-names(Data)
dim(s2009[c(1,2,4,5,7,9:dim(s2009)[1]),])
Data<-rbind(Data[Data$subject<2009,], s2009[c(1,2,4,5,7,9:dim(s2009)[1]),], Data[Data$subject>2009,]) 


Data[Data$subject==5003,c(1,5,8)] # An extra start phase event
# But why is this subject's phase 3 only 10 events, all of which are "Delete (Efficient)?"
# Looking back through the 50XX Excel file, subjects in the control group have widely varying numbers of events in their phase 3s. Why?
Data2<-rbind(Data[1:9499,],Data[9501:dim(Data)[1],])
Data2[Data2$subject==5003,c(1,5,8)]
Data<-Data2
rm(s2009, Data2)

write.table(Data, file="Data.txt", sep="\t")

# parse for phase and test again
levels(factor(Data$phase))
Data[Data$phase==0,c(1,5,8)] # subject 2002, row 4976
Data[4970:4980,c(1,5,8)] # there's just some undefined event in subject 2002's data before phase 1 starts. Delete it.
Data2<-rbind(Data[1:4976,], Data[4978:dim(Data)[1],])
levels(factor(Data2$phase))
Data2[Data2$phase==0, c(1,5,8)]
Data<-Data2
rm(Data2)

write.table(Data, file="Data.txt", sep="\t")



# The command factor would probably be better named "event."
names(Data)[5]<-"event"


# "it's not a control group since they were not randomly assigned"


# compute bins (# trials in the phase ÷ 4) …and then what do I do about uneven divisions? Assign the remainders from first to third bins.
bin<-numeric() # "bin" will be the variable encoding bins of trials within a phase
for (s in levels(factor(Data$subject))) { # for each subject
	for (p in 1:3) { # for each phase, each subject has 3 phases
	l<-length(Data[,1][Data$phase==p & Data$subject==s]) # get the length of the phase
	q<-l%/%4 # get its quotient when divided by 4
	r<-l%%4 # get its remainder when divided by 4
	for (i in 1:4) { # for each bin, i
		ifelse(i<=r, # if i is less than the remainder
		bin<-c(bin, rep(i, times=q+1)), # …then the length of the current bin is the quotient + 1, so set that many values of bin to the current bin value
		bin<-c(bin, rep(i, times=q)))} # …else the length of the current bin is the quotient, so set that many values of bin to the current bin value
}}

# column-bind bin to Data, the ESS Data
binned<-cbind(Data[,c(1,5,8)], bin)
Data<-cbind(Data, bin)

write.table(Data, file="Data.txt", sep="\t")



### Parse for handedness of KICs
# Okay, so I know all the events and I know the handedness of each, so I can simply create a new variable, "handedness," to hold an "R" or "L" depending on the value of the event. It's got to handle all the "non-event" events, too, like "end phase".

handedness<-array(data=c(levels(factor(Data$event)), c(NA, "L", "L", "L", "L", "L", "L", "R", "R", NA, "L", "L", "R", "R", "L", "L", NA, NA, "R", "R")), dim=c(length(levels(factor(Data$event))),2))



Data$handedness<-character(dim(Data)[1])

# Now check events against the handedness array, filling in Data$handedness from the corresponding row of the 2nd column of the handedness array.
for (i in 1:dim(Data)[1]) {
	Data$handedness[i]<-handedness[,2][handedness[,1]==Data$event[i]]}

write.table(Data, file="Data.txt", sep="\t")


# Make a new data frame with 1 subject per row, columns are variables iterating event efficiency, bin, and phase.
EfficiencyData <- numeric()

# Efficiency = # efficient events / (# efficient events + # inefficient events); excluding delete events; per bin per phase per subject

for (s in levels(factor(Data$subject))) { # for each subject
	SED <- s # SED will hold the subject's efficiency data, starting with his number

	for (p in c(1,3)) { # for each phase that contains observations, each subject has 3 phases …except that I'm only going to extract data for phases 1 & 3!

		for (b in 1:4) { # for each bin, each phase has 4 bins
			
				# get count of boldEff
				boldEff <- length(Data$event[Data$event=="Bold (Ctrl + B)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of boldInf
				boldInf <- length(Data$event[Data$event=="Bold (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])

				# get count of copyEff
				copyEff <- length(Data$event[Data$event=="Copy (Ctrl + C)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of copyInf
				copyInf <- length(Data$event[Data$event=="Copy (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])

				# get count of cutEff
				cutEff <- length(Data$event[Data$event=="Cut (Ctrl + X)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of cutInf
				cutInf <- length(Data$event[Data$event=="Cut (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])

				# get count of findEff
				findEff <- length(Data$event[Data$event=="Find (Ctrl + F)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of findInf
				findInf <- length(Data$event[Data$event=="Find (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of italicsEff
				italicsEff <- length(Data$event[Data$event=="Italics (Ctrl + I)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of italicsInf
				italicsInf <- length(Data$event[Data$event=="Italics (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of pasteEff
				pasteEff <- length(Data$event[Data$event=="Paste (Ctrl + V)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of pasteInf
				pasteInf <- length(Data$event[Data$event=="Paste (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of underlineEff
				underlineEff <- length(Data$event[Data$event=="Underline (Ctrl + U)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of underlineInf
				underlineInf <- length(Data$event[Data$event=="Underline (Inefficient)" & Data$subject==s & Data$phase==p & Data$bin==b])
				
				boldEfficiency <- boldEff / (boldEff + boldInf)
				copyEfficiency <- copyEff / (copyEff + copyInf)
				cutEfficiency <- cutEff / (cutEff + cutInf)
				findEfficiency <- findEff / (findEff + findInf)
				italicsEfficiency <- italicsEff / (italicsEff + italicsInf)
				pasteEfficiency <- pasteEff / (pasteEff + pasteInf)
				underlineEfficiency <- underlineEff / (underlineEff + underlineInf)
				
				# add this bin's data to the subject's data
				SED <- c(SED, boldEfficiency, copyEfficiency, cutEfficiency, findEfficiency, italicsEfficiency, pasteEfficiency, underlineEfficiency)}}
				
				# Done with this subject, now add his data to the efficiency data array
				EfficiencyData <- rbind(EfficiencyData, SED)}
				
# Why is EfficiencyData all character strings instead of numeric?

# Some results are NaN instead of just 0 because some bins had no efficient nor inefficient instances, in which case 0/0 would be NaN.
				
# spot-check
EfficiencyData[1,2:57]
Data[Data$subject==1001 & (Data$phase==1 | Data$phase==3), c(5,8,9)] # Everything defined & not delete that subject 1001 did was inefficient. NaNs indicated at indices 9, 10, 13, 14, 15, 16, 19, 21, 23, 24, 26, 30, 33, 36, 37, 43, 47, 49, 50, 51, 53, 54, & 56… so of the last seven, that's 1, 2, 4, 5, & 7-bold, copy, find, italics, & underline. So they should only have done cutInf & pasteInf. Yup, that's what happened. Subject 1001, phase 3, bin 4 is correct.
length(Data$event[Data$subject==1001 & Data$event=="Bold (Ctrl + B)" & Data$phase==1 & Data$bin==1])
length(Data$event[Data$subject==1001 & Data$event=="Bold (Inefficient)" & Data$phase==1 & Data$bin==1])
# Subject 1001, phase 1, bin 1, boldEfficiency is also correct, as are the other six Efficiencies for s1001p1b1.

EfficiencyData[35,]
Data[Data$subject==2006 & (Data$phase==1 | Data$phase==3), c(5,8,9)]
# p1b1: no bold √
# p1b1: no copy √
# p1b1: cut, find, italics, paste, underline √
# p1b1: no Eff √
# EfficiencyData[35,33]==0.5, that's p3b1 find, which has one instance each of findEff and findInf. √

EfficiencyData[65,]
Data[Data$subject==5009 & (Data$phase==1 | Data$phase==3), c(5,8,9)]
# p1b1 is correct
# p1b4 is correct


# Make some names for the EfficiencyData	
EDnames<-"subject"; p<-NULL; b<-NULL
for (p in c(1,3)) {
	for (b in 1:4) {
	EDnames<-c(EDnames, paste("bold", "p", p, "b", b, sep=""), paste("copy", "p", p, "b", b, sep=""), paste("cut", "p", p, "b", b, sep=""), paste("find", "p", p, "b", b, sep=""), paste("italics", "p", p, "b", b, sep=""), paste("paste", "p", p, "b", b, sep=""), paste("underline", "p", p, "b", b, sep=""))}}
ED<-data.frame(EfficiencyData)
names(ED)<-EDnames

write.table(ED, file="EfficiencyData.txt", sep="\t")

ED<-read.delim(file="EfficiencyData.txt", sep="\t")

#  from gribblelab's blog
# The first step is to reorganize the data into a matrix form where rows are subjects, and columns are levels of the repeated measures factor:
# I already have that, except that I also have a column encoding subject, which I won't need, so get rid of that one column

EDmatrix<-cbind(ED[,2:57])

# my 70 x 56 matrix may be taking forever, so let's try lming a much smaller matrix first

EDmatrix<-cbind(ED[1:10,2:4])
mlmED<-lm(EDmatrix ~ 1)

# …all these NaNs are problematic. What to do? Collapse some levels of bin? No, the analysis of interest here is time, so collapse the command factor and just do phase * bin as Reviewer 1 clearly asked for change in efficiency over time. So make another for loop like the command * phase * bin efficiency calculator, but just take per phase & per bin: total eff of all commands / total commands

Data<-read.delim(file="Data.txt")

Data[(Data$event=="Bold (Inefficient)" | Data$event=="Copy (Inefficient)") & Data$subject==1001 & (Data$phase==1 | Data$phase==3),c(1,5,8,9)]
Data[Data$subject==1001,c(1,5,8,9)]

# Make a new data frame with 1 subject per row, columns are variables iterating event efficiency, bin, and phase.
EfficiencyData <- numeric()

# Efficiency = # efficient events / (# efficient events + # inefficient events); excluding delete events; per bin per phase per subject

for (s in levels(factor(Data$subject))) { # for each subject
	SED <- s # SED will hold the subject's efficiency data, starting with his number

	for (p in c(1,3)) { # for each phase that contains observations, each subject has 3 phases …except that I'm only going to extract data for phases 1 & 3!

		for (b in 1:4) { # for each bin, each phase has 4 bins
			
				# get count of Eff
				Eff <- length(Data$event[(Data$event=="Bold (Ctrl + B)" | Data$event=="Copy (Ctrl + C)" | Data$event=="Cut (Ctrl + X)" | Data$event=="Find (Ctrl + F)" | Data$event=="Italics (Ctrl + I)" | Data$event=="Paste (Ctrl + V)" | Data$event=="Underline (Ctrl + U)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of Inf
				Ineff <- length(Data$event[(Data$event=="Bold (Inefficient)" | Data$event=="Copy (Inefficient)" | Data$event=="Cut (Inefficient)" | Data$event=="Find (Inefficient)" | Data$event=="Italics (Inefficient)" | Data$event=="Paste (Inefficient)" | Data$event=="Underline (Inefficient)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				Efficiency <- Eff / (Eff + Ineff)
				
				# add this bin's data to the subject's data
				SED <- c(SED, Efficiency)}}
				
				# Done with this subject, now add his data to the efficiency data array
				EfficiencyData <- rbind(EfficiencyData, SED)}
				
# Make some names for the EfficiencyData	
EDnames<-"subject"; p<-NULL; b<-NULL
for (p in c(1,3)) {
	for (b in 1:4) {
	EDnames<-c(EDnames, paste("p", p, "b", b, sep=""))}}
ED<-data.frame(EfficiencyData)
names(ED)<-EDnames

write.table(ED, file="EfficiencyData.txt", sep="\t")

ED<-read.delim(file="EfficiencyData.txt", sep="\t")



# How does s1005 have a NaN for p1b4?
with(Data, Data[subject==1005 & phase==1 & bin==4,c(1,5,8,9)])
Data[Data$subject==1005,c(1,5,8,9)]
# The answer is that all they did in that bin was to Delete (Efficient).
# So what proportion of the efficiency data is now NaN and is it now low enough to run the repeated measures ANOVA?

EDdata<-c(ED[,2], ED[,3], ED[,4], ED[,5], ED[,6], ED[,7], ED[,8], ED[,9])
EDconds<-c("p1b1", "p1b2", "p1b3", "p1b4", "p3b1", "p3b2", "p3b3", "p3b4")
EDframe<-data.frame(EDdata, EDconds)
# ==> Nope

EDmatrix<-cbind(ED[1:10,2:9])
EDmatrix<-as(cbind(ED[1:10,2:9]), matrix)
mlmED<-lm(EDmatrix ~ 1) #this needs to actually be a matrix, not a data frame


attach(ED)
subject <- factor(subject)
p1b1 <- factor(p1b1)
p1b2 <- factor(p1b2)
p1b3 <- factor(p1b3)
p1b4 <- factor(p1b4)
p3b1 <- factor(p3b1)
p3b2 <- factor(p3b2)
p3b3 <- factor(p3b3)
p3b4 <- factor(p3b4)
detach(ED)

EDmatrix <- numeric(0)
for (c in 2:9) {
				for (r in 1:70) {
					 EDmatrix <- c(EDmatrix, ED[r,c])}}

dim(EDmatrix) <- c(70, 8)

mlmED<-lm(EDmatrix ~ 1)
bin <- factor(1:4)
phase <- factor(c(1,3))
time <- factor(1:8)

library(car)
mlmED.aov <- Anova(mlmED, idata = data.frame(time), idesign = ~ time, type = "III")
mlmED.aov <- Anova(mlmED, idata = data.frame(phase, bin), idesign = ~ phase + bin, type = "III")
summary(mlmED.aov)


# See "multivariate linear model for repeated-measures data" in the help file for Anova {car}.
?OBrienKaiser
bin<- numeric()
bin <- ordered(rep(1:4, 2))
phase <- factor(c(rep(1, 4), rep(3, 4)))
idata <- data.frame(phase, bin)
mod.ED <- lm(cbind(p1b1, p1b2, p1b3, p1b4, p3b1, p3b2, p3b3, p3b4) ~ 1, data = ED)
av.ED <- Anova(mod.ED, idata=idata, idesign=~phase*bin, type="III")
summary(av.ED)

# Awesome! That totally got it! Now do it again, but this time with the between-subjects factor, condition.

# Make a new data frame with 1 subject per row, columns are variables iterating event efficiency, bin, and phase.
EfficiencyData <- numeric()

# Efficiency = # efficient events / (# efficient events + # inefficient events); excluding delete events; per bin per phase per subject

for (s in levels(factor(Data$subject))) { # for each subject
	SED <- c(s, levels(factor(Data$condition[Data$subject==s]))) # SED will hold the subject's efficiency data, starting with his number and his condition

	for (p in c(1,3)) { # for each phase that contains observations, each subject has 3 phases …except that I'm only going to extract data for phases 1 & 3!

		for (b in 1:4) { # for each bin, each phase has 4 bins
			
				# get count of Eff
				Eff <- length(Data$event[(Data$event=="Bold (Ctrl + B)" | Data$event=="Copy (Ctrl + C)" | Data$event=="Cut (Ctrl + X)" | Data$event=="Find (Ctrl + F)" | Data$event=="Italics (Ctrl + I)" | Data$event=="Paste (Ctrl + V)" | Data$event=="Underline (Ctrl + U)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of Inf
				Ineff <- length(Data$event[(Data$event=="Bold (Inefficient)" | Data$event=="Copy (Inefficient)" | Data$event=="Cut (Inefficient)" | Data$event=="Find (Inefficient)" | Data$event=="Italics (Inefficient)" | Data$event=="Paste (Inefficient)" | Data$event=="Underline (Inefficient)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				Efficiency <- Eff / (Eff + Ineff)
				
				# add this bin's data to the subject's data
				SED <- c(SED, Efficiency)}}
				
				# Done with this subject, now add his data to the efficiency data array
				EfficiencyData <- rbind(EfficiencyData, SED)}
				
# Make some names for the EfficiencyData	
EDnames<-c("subject", "condition"); p<-NULL; b<-NULL
for (p in c(1,3)) {
	for (b in 1:4) {
	EDnames<-c(EDnames, paste("p", p, "b", b, sep=""))}}
ED<-data.frame(EfficiencyData)
names(ED)<-EDnames

ED.old <- read.delim("EfficiencyData.txt")

ED[32,]
ED.old[32,]

write.table(ED, file="EfficiencyData.txt", sep="\t", row.names=FALSE)

ED$condition <- factor(ED$condition)
mod.ED <- lm(cbind(p1b1, p1b2, p1b3, p1b4, p3b1, p3b2, p3b3, p3b4) ~ condition, data = ED)
av.ED <- Anova(mod.ED, idata=idata, idesign=~phase*bin, type="III")
summary(av.ED)
summary(av.ED, multivariate=FALSE)


# My error term DFs seem to differ between R & SPSS. It's apparently because SPSS is excluding cases with missing values while Anova {car} {R} is not. What are the consequences to including those values, aside from increased power relative to excluding them? I might have an issue because those NaNs might've been transmuted to strings during the repeated measures parsing process! Meanwhile I've learned that R does exclude NaNs (by default) and that I need to try these analyses again with 3 bins anyway. So use that to figure out how to parse numeric data into numeric vectors, not character vectors.


Data<-read.delim("Data.txt")

# compute bins (# trials in the phase ÷ 3) …and then what do I do about uneven divisions? Assign the remainders from first to third bins.
bin<-numeric() # "bin" will be the variable encoding bins of trials within a phase
for (s in levels(factor(Data$subject))) { # for each subject
	for (p in 1:3) { # for each phase, each subject has 3 phases
	l<-length(Data[,1][Data$phase==p & Data$subject==s]) # get the length of the phase
	q<-l%/%3 # get its quotient when divided by 3
	r<-l%%3 # get its remainder when divided by 3
	for (i in 1:3) { # for each bin, i
		ifelse(i<=r, # if i is less than the remainder
		bin<-c(bin, rep(i, times=q+1)), # …then the length of the current bin is the quotient + 1, so set that many values of bin to the current bin value
		bin<-c(bin, rep(i, times=q)))} # …else the length of the current bin is the quotient, so set that many values of bin to the current bin value
}}

# replace the old bin with the new
Data$bin<-bin


# Make a new data frame with 1 subject per row, columns are variables iterating event efficiency, bin, and phase.
EfficiencyData <- numeric()
cond <- character()


# Efficiency = # efficient events / (# efficient events + # inefficient events); excluding delete events; per bin per phase per subject

for (s in unique(Data$subject)) { # for each subject
	SED <- s # SED will hold the subject's efficiency data, starting with his number	
	for (p in c(1,3)) { # for each phase that contains observations, each subject has 3 phases …except that I'm only going to extract data for phases 1 & 3!

		for (b in 1:3) { # for each bin, each phase has 3 bins
			
				# get count of Eff
				Eff <- length(Data$event[(Data$event=="Bold (Ctrl + B)" | Data$event=="Copy (Ctrl + C)" | Data$event=="Cut (Ctrl + X)" | Data$event=="Find (Ctrl + F)" | Data$event=="Italics (Ctrl + I)" | Data$event=="Paste (Ctrl + V)" | Data$event=="Underline (Ctrl + U)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				# get count of Inf
				Ineff <- length(Data$event[(Data$event=="Bold (Inefficient)" | Data$event=="Copy (Inefficient)" | Data$event=="Cut (Inefficient)" | Data$event=="Find (Inefficient)" | Data$event=="Italics (Inefficient)" | Data$event=="Paste (Inefficient)" | Data$event=="Underline (Inefficient)") & Data$subject==s & Data$phase==p & Data$bin==b])
				
				Efficiency <- Eff / (Eff + Ineff)
				
				# add this bin's data to the subject's data
				SED <- c(SED, Efficiency)}}
				
				# Done with this subject, now add his data to the efficiency data array
				EfficiencyData <- rbind(EfficiencyData, SED)
				# and record the subject's condition
				cond <- c(cond, levels(factor(Data$condition[Data$subject==s])))}
				
EfficiencyData <- data.frame(EfficiencyData, cond)
				
# Make some names for the EfficiencyData	
EDnames<-c("subject"); p<-NULL; b<-NULL
for (p in c(1,3)) {
	for (b in 1:3) {
	EDnames<-c(EDnames, paste("p", p, "b", b, sep=""))}}
EDnames<-c(EDnames, "condition")
names(EfficiencyData)<-EDnames

# We're down to 5 cases with NaN's, which is 17 better than the 22 we had with 4 bins!


write.table(EfficiencyData, file="EfficiencyData.txt", sep="\t", row.names=FALSE)
ED<-EfficiencyData
ED$condition <- factor(ED$condition)
bin <- ordered(rep(1:3, 2))
phase <- factor(c(rep(1, 3), rep(3, 3)))
idata <- data.frame(phase, bin)

# Don't forget to sanity-check replications!
replications(ED)
replications(phase)
replications(bin)
replicaitons(idata)

mod.ED <- lm(cbind(p1b1, p1b2, p1b3, p3b1, p3b2, p3b3) ~ condition, data = ED)
(av.ED <- Anova(mod.ED, idata=idata, idesign=~phase*bin, type="III"))
summary(av.ED)
summary(av.ED, multivariate=FALSE)
