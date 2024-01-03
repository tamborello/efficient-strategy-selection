# Analysis of Efficient Strategy Selection Microworld Experiment
# Data collected by S. Camille Peres, University of Houston at Clear Lake
# Analyses by Frank Tamborello, Cogscent, LLC
# Author: Frank Tamborello, Cogscent, LLC
# CC-BY-SA 2013 Frank Tamborello, Cogscent, LLC
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of Creative Commons Attribute-ShareAlike 4.0 International License: http://creativecommons.org/licenses/by-sa/4.0/
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
# Description: This is a script for plotting and analyzing human subject and computational model data from the Efficient Strategy Selection Microworld Experiment.


setwd("~/Documents/ESS/Manuscript/r3")




# Parse event data for item & CmdStyle. See Import & Parse Raw ESS Data.R.
Data<-read.delim("~/Documents/ESS/Manuscript/CRBGH ms r2/analyses/Data.txt")
attach(Data)
item<-character()
CmdStyle<-integer() # 1 = efficient, 0 = inefficient
for (i in 1:dim(Data)[1]) {
	if (event[i] == "Bold (Ctrl + B)") {item[i] <- "bld"; CmdStyle[i] <- 1}
	if (event[i] == "Bold (Inefficient)") {item[i] <- "bld"; CmdStyle[i] <- 0}
	if (event[i] == "Copy (Ctrl + C)") {item[i] <- "cpy"; CmdStyle[i] <- 1}
	if (event[i] == "Copy (Inefficient)") {item[i] <- "cpy"; CmdStyle[i] <- 0}
	if (event[i] == "Cut (Ctrl + X)") {item[i] <- "cut"; CmdStyle[i] <- 1}
	if (event[i] == "Cut (Inefficient)") {item[i] <- "cut"; CmdStyle[i] <- 0}
	if (event[i] == "Delete (Efficient)") {item[i] <- "dlt"; CmdStyle[i] <- 1}
	if (event[i] == "Delete (Inefficient)") {item[i] <- "dlt"; CmdStyle[i] <- 0}
	if (event[i] == "Find (Ctrl + F)") {item[i] <- "fnd"; CmdStyle[i] <- 1}
	if (event[i] == "Find (Inefficient)") {item[i] <- "fnd"; CmdStyle[i] <- 0}
	if (event[i] == "Italics (Ctrl + I)") {item[i] <- "itl"; CmdStyle[i] <- 1}
	if (event[i] == "Italics (Inefficient)") {item[i] <- "itl"; CmdStyle[i] <- 0}
	if (event[i] == "Paste (Ctrl + V)") {item[i] <- "pst"; CmdStyle[i] <- 1}
	if (event[i] == "Paste (Inefficient)") {item[i] <- "pst"; CmdStyle[i] <- 0}
	if (event[i] == "Underline (Ctrl + U)") {item[i] <- "und"; CmdStyle[i] <- 1}
	if (event[i] == "Underline (Inefficient)") {item[i] <- "und"; CmdStyle[i] <- 0}}
# Ugh, isn't there a more efficient & elegant way do to this? Doesn't R at least have
# cond expressions?
item<-factor(item)
# And because event's last item evaluates to NA for item & CmdStyle
item[10557]<-NA
CmdStyle[10557]<-NA
newdata<-data.frame(cbind(Data, item, CmdStyle))
write.table(newdata, file="newdata.txt", sep="\t")
newdata <- read.delim("~/Documents/ESS/Manuscript/r3/newdata.txt")
attach(newdata)



# t-test on change scores. Do it separately with the participant means and item means.
# Make a new data frame with 1 subject per row, columns are variables iterating event efficiency, bin, and phase.

# Efficiency = # efficient events / (# efficient events + # inefficient events); excluding delete events; per bin per phase per subject




bldEfficiency<-numeric()
cpyEfficiency<-numeric()
cutEfficiency<-numeric()
dltEfficiency<-numeric()
fndEfficiency<-numeric()
itlEfficiency<-numeric()
pstEfficiency<-numeric()
undEfficiency<-numeric()
i<-1
for (s in unique(subject)) {
	bldEfficiency[i] <- (sum(CmdStyle[subject==s & item=="bld" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==3])) - (sum(CmdStyle[subject==s & item=="bld" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==1]))
	cpyEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cpy" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==3])) - (sum(CmdStyle[subject==s & item=="cpy" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==1]))
	cutEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cut" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==3])) - (sum(CmdStyle[subject==s & item=="cut" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==1]))
	dltEfficiency[i] <- (sum(CmdStyle[subject==s & item=="dlt" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==3])) - (sum(CmdStyle[subject==s & item=="dlt" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==1]))
	fndEfficiency[i] <- (sum(CmdStyle[subject==s & item=="fnd" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==3])) - (sum(CmdStyle[subject==s & item=="fnd" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==1]))
	itlEfficiency[i] <- (sum(CmdStyle[subject==s & item=="itl" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==3])) - (sum(CmdStyle[subject==s & item=="itl" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==1]))
	pstEfficiency[i] <- (sum(CmdStyle[subject==s & item=="pst" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==3])) - (sum(CmdStyle[subject==s & item=="pst" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==1]))
	undEfficiency[i] <- (sum(CmdStyle[subject==s & item=="und" & phase==3], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==3])) - (sum(CmdStyle[subject==s & item=="und" & phase==1], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==1]))
	i<-i+1
}

# bldEfficiency has NaNs for subjects 1024 & 5002. That shouldn't happen.
# Previous phase parsing had missed a phase transition from 1 to 2 for s1024 because there was no Start Phase event recorded when there should have been! Likewise for s5002.
# Compute efficiencies for all the other items and chack for NaNs to detect other subjects with something odd going on (still only 1024 & 5002, & they're consistent for all other items). Then fix phase manually for those subjects (going back through the original phase parsing for all subjects is likely to only make things worse).
# s1024 should have phase 2 begin with event 54 and phase 3 with event 103.
phase[subject==1024]<-c(rep(1, 53), rep(2, 49), rep(3, 62))
# s5002 should start phase 2 with event 14 and start phase 3 with event…14? Subjects 5001, 5003, and 5004 all had empty phase 2s. That's correct, subjects in that condition didn't issue any commands. Instead they watched the confederate perform a usability study on a prototype website.
phase[subject==5002]<-c(rep(1, 13), rep(3, 73))

# Save over all data in r3.

write.table(newdata, file="newdata.txt", sep="\t")


# Then recompute phase efficiency differences, t-tests on phase efficiency differences.
for (s in unique(subject)) {ifelse(s==1001, cond <- as.character(unique(condition[subject==s])), cond<-c(cond, as.character(unique(condition[subject==s]))))}


# Now that I've got change scores by subject and by item, getting the subject and item means is a simple matter of averaging horizontally or vertially. Then do a single-tailed t-test (for means > 0) on each.
subj.means<-numeric()
for (i in 1:length(unique(subject))){
subj.means[i] <- mean(c(bldEfficiency[i], cpyEfficiency[i], cutEfficiency[i], dltEfficiency[i], fndEfficiency[i], itlEfficiency[i], pstEfficiency[i], undEfficiency[i]), na.rm=T)}
t.test(subj.means[cond=="eff"], alternative="g")
t.test(subj.means[cond=="ineff"], alternative="g")

eff.item.means<-numeric()
eff.item.means <- c(mean(bldEfficiency[condition=="eff"], na.rm=T), mean(cpyEfficiency[condition=="eff"], na.rm=T), mean(cutEfficiency[condition=="eff"], na.rm=T), mean(dltEfficiency[condition=="eff"], na.rm=T), mean(fndEfficiency[condition=="eff"], na.rm=T), mean(itlEfficiency[condition=="eff"], na.rm=T), mean(pstEfficiency[condition=="eff"], na.rm=T), mean(undEfficiency[condition=="eff"], na.rm=T))
t.test(eff.item.means, alternative="g")

ineff.item.means<-numeric()
ineff.item.means <- c(mean(bldEfficiency[condition=="ineff"], na.rm=T), mean(cpyEfficiency[condition=="ineff"], na.rm=T), mean(cutEfficiency[condition=="ineff"], na.rm=T), mean(dltEfficiency[condition=="ineff"], na.rm=T), mean(fndEfficiency[condition=="ineff"], na.rm=T), mean(itlEfficiency[condition=="ineff"], na.rm=T), mean(pstEfficiency[condition=="ineff"], na.rm=T), mean(undEfficiency[condition=="ineff"], na.rm=T))
t.test(ineff.item.means, alternative="g")

 
# Graph phase 1 (mean of subject means) vs phase 3 (mean of subject means)
phase1<-numeric()
phase3<-numeric()
i<-1
for (s in unique(subject)) {
	phase1[i] <- mean(CmdStyle[subject==s & phase==1], na.rm=T)
	phase3[i] <- mean(CmdStyle[subject==s & phase==3], na.rm=T)
	i<-i+1}

Phases<-c(mean(phase1, na.rm=T), ciw(phase1, na.rm=T), mean(phase3, na.rm=T), ciw(phase3, na.rm=T))

require(gplots)


SmeanCond.EffChange <- list("Efficient" = phase3[cond=="eff"] - phase1[cond=="eff"], "Inefficient" = phase3[cond=="ineff"] - phase1[cond=="ineff"], "Comparison" = phase3[cond=="control"] - phase1[cond=="control"])
EffChange.means <- list("Efficient" = mean(phase3[cond=="eff"] - phase1[cond=="eff"], na.rm=T), "Inefficient" = mean(phase3[cond=="ineff"] - phase1[cond=="ineff"], na.rm=T), "Comparison" = mean(phase3[cond=="control"] - phase1[cond=="control"], na.rm=T))


# Boxplot of subject mean proportion efficiency change by condition
quartz(title="Figure 1", width=3.55, height=5, pointsize=10)
par(mai=c(.5, .75, .1, .1), family="sans", xpd=TRUE)
boxplot(SmeanCond.EffChange, ylab="Efficiency Change", yaxt="n", frame.plot=F)
axis(2, at=seq(-.4, .6, .1), labels=F)
text(par("usr")[1]-0.3, seq(-.4, .6, .1), srt = 0, labels = seq(-40, 60, 10), xpd = TRUE)
points(x=c(1:3), y=EffChange.means, pch=4, lwd=3)



# Line graph of subject mean percent efficiency by condition across P1B1, P1B2, P3B1, P3B2
eff.P1B1 <- c(mean(CmdStyle[condition=="eff" & phase==1 & bin==1], na.rm=T), ciw(CmdStyle[condition=="eff" & phase==1 & bin==1], na.rm=T))

for (c in unique(condition)) {
	for (p in c(1, 3)) {	
		for (b in 1:2) {
			assign(paste(c, ".", "P", p, "B", b, sep=""), c(mean(CmdStyle[condition==c & phase==p & bin==b], na.rm=T), ciw(CmdStyle[condition==c & phase==p & bin==b], na.rm=T)))}}}
# Is it possible to vectorize this operation?

EffOverTime.means <- data.frame()
EffOverTime.ciw <- data.frame()
for (c in unique(condition)) {
	for (p in c(1, 3)) {	
		for (b in 1:2) {
			assign(paste(c, ".", "P", p, "B", b, sep=""), c(mean(CmdStyle[condition==c & phase==p & bin==b], na.rm=T), ciw(CmdStyle[condition==c & phase==p & bin==b], na.rm=T)))}}}

# Ugh, this gets ugly because I slurped all those quantities into a bunch of 2-item vectors instead of one or two dataframes. Surely there must be a better way…
quartz(title="Figure 2", width=3.55, height=5, pointsize=10)
par(xpd=T, srt=0, mai=c(.5, .75, .1, .1), family="sans")
plotCI(c(eff.P1B1[1], eff.P1B2[1], eff.P3B1[1], eff.P3B2[1]), uiw=c(eff.P1B1[2], eff.P1B2[2], eff.P3B1[2], eff.P3B2[2]), pch=0, xaxt="n", xlab="", yaxt="n", ylab="Efficiency", ylim=c(.15, .6), frame.plot=F)
plotCI(x=1:4, y=c(ineff.P1B1[1], ineff.P1B2[1], ineff.P3B1[1], ineff.P3B2[1]), uiw=c(ineff.P1B1[2], ineff.P1B2[2], ineff.P3B1[2], ineff.P3B2[2]), pch=1, add=T)
plotCI(x=1:4, y=c(control.P1B1[1], control.P1B2[1], control.P3B1[1], control.P3B2[1]), uiw=c(control.P1B1[2], control.P1B2[2], control.P3B1[2], control.P3B2[2]), pch=2, add=T)
axis(1, at=c(1:4), labels=c("P1B1", "P1B2", "P3B1", "P3B2"))
axis(2, labels=F)
text(par("usr")[1]-.3, seq(.2, .6, .1), srt=0, labels=seq(20, 60, 10), xpd=T)
lines(c(eff.P1B1[1], eff.P1B2[1], eff.P3B1[1], eff.P3B2[1]), lty=1)
lines(x=1:4, y=c(ineff.P1B1[1], ineff.P1B2[1], ineff.P3B1[1], ineff.P3B2[1]), lty=2)
lines(x=1:4, y=c(control.P1B1[1], control.P1B2[1], control.P3B1[1], control.P3B2[1]), lty=3)
legend(1, .6, legend=c("Efficient", "Inefficient", "Comparison"), pch=c(0, 1, 2), lty=c(1, 2, 3), title="Condition", bty="n")


EffChange <- c(0, (eff.P3B1[1] - eff.P1B2[1]) / eff.P1B2[1], (eff.P3B2[1] - eff.P1B2[1]) / eff.P1B2[1])
IneffChange <- c(0, (ineff.P3B1[1] - ineff.P1B2[1]) / ineff.P1B2[1], (ineff.P3B2[1] - ineff.P1B2[1]) / ineff.P1B2[1])
CompChange <- c(0, (control.P3B1[1] - control.P1B2[1]) / control.P1B2[1], (control.P3B2[1] - control.P1B2[1]) / control.P1B2[1])

EffChange <- c(0, eff.P3B1[1] - eff.P1B2[1], eff.P3B2[1] - eff.P1B2[1])
IneffChange <- c(0, ineff.P3B1[1] - ineff.P1B2[1], ineff.P3B2[1] - ineff.P1B2[1])
CompChange <- c(0, control.P3B1[1] - control.P1B2[1], control.P3B2[1] - control.P1B2[1])


quartz(title="Figure 3", width=3.55, height=4, pointsize=10)
par(xpd=T, srt=0, mai=c(.5, .75, .5, .1), family="sans")
yscale <- .25
plotCI(x=1:3, y=EffChange, pch=0, xaxt="n", xlab="", yaxt="n", ylab="Efficiency Change", ylim=c(.0, yscale), frame.plot=F)
plotCI(x=1:3, y=IneffChange, pch=1, add=T)
plotCI(x=1:3, y=CompChange, pch=2, add=T)
axis(1, at=c(1:3), labels=c("P1B2", "P3B1", "P3B2"))
axis(2, at=seq(0, yscale, .05), labels=F)
text(par("usr")[1]-.2, seq(0, yscale, .05), srt=0, labels=seq(0, yscale * 100, 5), xpd=T)
lines(x=1:3, y=EffChange, lty=1)
lines(x=1:3, y=IneffChange, lty=2)
lines(x=1:3, y=CompChange, lty=3)
legend(1.75, yscale + .05, legend=c("Efficient", "Inefficient", "Comparison"), pch=c(0, 1, 2), lty=c(1, 2, 3), bty="n")
mtext(text="Condition:", side=3, line=1, at=1.4)




# Another version of Figure 3, but this time of subject mean base line differences so I can then show the mean and variances of those.

for (c in unique(condition)) {
	for (p in c(1, 3)) {	
		for (b in 1:2) {
			assign(paste(c, ".", "P", p, "B", b, sep=""), 
				for (s in unique(subject)) {
					mean(CmdStyle[subject==s, condition==c & phase==p & bin==b], na.rm=T)
				}
			
				c(mean(CmdStyle[subject==s, condition==c & phase==p & bin==b], na.rm=T), ciw(CmdStyle[subject==s, condition==c & phase==p & bin==b], na.rm=T)))}}}


PB <- data.frame("Condition"=NULL, "P1B2"=numeric(), "P3B1"=numeric(), "P3B2"=numeric())
for (s in unique(subject)) {
	PB[s,1] <- unique(condition[subject==s])
	PB[s,2] <- mean(CmdStyle[subject==s & phase==1 & bin==2], na.rm=T)
	PB[s,3] <- mean(CmdStyle[subject==s & phase==3 & bin==1], na.rm=T)
	PB[s,4] <- mean(CmdStyle[subject==s & phase==3 & bin==2], na.rm=T)}




# Since phase was wrong for at least two subjects, reparse for bin(2) & compare early to late for phase 3.
# compute bins (# trials in the phase ÷ 2) …and then what do I do about uneven divisions? Assign the remainders to the first bin.
bin<-numeric() # "bin" will be the variable encoding bins of trials within a phase
for (s in unique(subject)) { # for each subject
	for (p in 1:3) { # for each phase, each subject has 3 phases
	l<-length(phase[phase==p & subject==s]) # get the length of the phase
	q<-l%/%2 # get its quotient when divided by 2
	r<-l%%2 # get its remainder when divided by 2
	for (i in 1:2) { # for each bin, i
		ifelse(i<=r, # if i is less than or equal to the remainder
		bin<-c(bin, rep(i, times=q+1)), # …then the length of the current bin is the quotient + 1, so set that many values of bin to the current bin value
		bin<-c(bin, rep(i, times=q)))} # …else the length of the current bin is the quotient, so set that many values of bin to the current bin value
}}
newdata$bin <- bin
write.table(newdata, file="newdata.txt", sep="\t")




# Did it matter more for some items than others? Graph P1 next to P3 for each item.
phase1.mean<-numeric()
phase3.mean<-numeric()
phase1.ciw<-numeric()
phase3.ciw<-numeric()
j<-1
for (i in unique(item)[2:9]) {
	phase1.mean[j] <- mean(CmdStyle[item==i & phase==1], na.rm=T)
	phase3.mean[j] <- mean(CmdStyle[item==i & phase==3], na.rm=T)
	phase1.ciw[j] <- ciw(CmdStyle[item==i & phase==1], na.rm=T)
	phase3.ciw[j] <- ciw(CmdStyle[item==i & phase==3], na.rm=T)
	j<-j+1}

# I'm going to want to plot one item, across phases, together, so interleave those means across phases, grouping by item
interleave <- function(v1,v2)
{
ord1 <- 2*(1:length(v1))-1
ord2 <- 2*(1:length(v2))
c(v1,v2)[order(c(ord1,ord2))]
}

means <- interleave(phase1.mean, phase3.mean)
ciws <- interleave(phase1.ciw, phase3.ciw)

require(gplots)
quartz(title="Item Means", width=3.35, height=5, pointsize=10)
par(mai=c(.6, .75, .1, .25), family="serif", xpd=TRUE)
yscale<-1
barplot2(means, names.arg=interleave(as.character(unique(item)[2:9]), rep("", 8)), xlab="", plot.ci=T, ci.u=c(means+ciws), ci.l=c(means-ciws), ylim=c(0,yscale), yaxt="n", ylab="Efficiency", col=c("white", "gray40"), space=c(0, interleave(rep(0, 8), rep(.5, 7))))
legend(0, .8, legend=c("Pre", "Post"), fill=c("white", "gray40"), bty="n")
axis(2, at=seq(0, yscale, by=.1), labels=F)
ylabels = format(seq(0, yscale, by=.1), digits = 1)
text(par("usr")[1]-1, seq(0, yscale, by = .1), srt = 0, labels = ylabels, xpd = TRUE)



# Mean & 95% ciw efficiency change (within phase 3, bin 3 - bin 1) by subject and by item for efficient condition.
bldEfficiency<-numeric()
cpyEfficiency<-numeric()
cutEfficiency<-numeric()
dltEfficiency<-numeric()
fndEfficiency<-numeric()
itlEfficiency<-numeric()
pstEfficiency<-numeric()
undEfficiency<-numeric()
i<-1
for (s in unique(subject[condition=="eff"])) {
	bldEfficiency[i] <- (sum(CmdStyle[subject==s & item=="bld" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="bld" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==3 & bin==1]))
	cpyEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==1]))
	cutEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cut" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="cut" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==3 & bin==1]))
	dltEfficiency[i] <- (sum(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==1]))
	fndEfficiency[i] <- (sum(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==1]))
	itlEfficiency[i] <- (sum(CmdStyle[subject==s & item=="itl" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="itl" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==3 & bin==1]))
	pstEfficiency[i] <- (sum(CmdStyle[subject==s & item=="pst" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="pst" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==3 & bin==1]))
	undEfficiency[i] <- (sum(CmdStyle[subject==s & item=="und" & phase==3 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==3 & bin==3])) - (sum(CmdStyle[subject==s & item=="und" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==3 & bin==1]))
	i<-i+1}
	
subj.means<-numeric()
for (i in 1:length(unique(subject[condition=="eff"]))){
subj.means[i] <- mean(c(bldEfficiency[i], cpyEfficiency[i], cutEfficiency[i], dltEfficiency[i], fndEfficiency[i], itlEfficiency[i], pstEfficiency[i], undEfficiency[i]), na.rm=T)}

Smean<-mean(subj.means, na.rm=T); Smean
Sciw<-ciw(subj.means, na.rm=T); Sciw
item.means <- c(mean(bldEfficiency, na.rm=T), mean(cpyEfficiency, na.rm=T), mean(cutEfficiency, na.rm=T), mean(dltEfficiency, na.rm=T), mean(fndEfficiency, na.rm=T), mean(itlEfficiency, na.rm=T), mean(pstEfficiency, na.rm=T), mean(undEfficiency, na.rm=T))
Imean<-mean(item.means); Imean
Iciw<-ciw(item.means); Iciw


# Phase 1 bin 3 compared with Phase 3 bin 1
# AKA Phase 3 - Phase 1 difference
i<-1
for (s in unique(subject[condition=="eff"])) {
	bldEfficiency[i] <- (sum(CmdStyle[subject==s & item=="bld" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="bld" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="bld" & phase==1 & bin==3]))
	cpyEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="cpy" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="cpy" & phase==1 & bin==3]))
	cutEfficiency[i] <- (sum(CmdStyle[subject==s & item=="cut" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="cut" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="cut" & phase==1 & bin==3]))
	dltEfficiency[i] <- (sum(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="dlt" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="dlt" & phase==1 & bin==3]))
	fndEfficiency[i] <- (sum(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="fnd" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="fnd" & phase==1 & bin==3]))
	itlEfficiency[i] <- (sum(CmdStyle[subject==s & item=="itl" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="itl" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="itl" & phase==1 & bin==3]))
	pstEfficiency[i] <- (sum(CmdStyle[subject==s & item=="pst" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="pst" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="pst" & phase==1 & bin==3]))
	undEfficiency[i] <- (sum(CmdStyle[subject==s & item=="und" & phase==3 & bin==1], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==3 & bin==1])) - (sum(CmdStyle[subject==s & item=="und" & phase==1 & bin==3], na.rm=T)/length(CmdStyle[subject==s & item=="und" & phase==1 & bin==3]))
	i<-i+1}
	
	
	
	
	
# How many trials should be in the intervention phase? Camille's methods say participants averaged 54. I want the median number of phase 2 trials for subjects in the efficient condition.
dim(newdata[condition=="eff" & phase==2,])[1]
length(unique(subject[condition=="eff"]))

EffSubjs <- unique(subject[condition=="eff"])
trls <- numeric()

for (i in 1:length(EffSubjs)) {
	trls[i] <- dim(newdata[subject==EffSubjs[i] & phase==3,])[1]
}

median(trls)


# How many trials (median) per each of the three phases?
# pre-: 49
# int: 55
# post-: 60
	
	
# Model
detach(ModelData)
ModelData <- read.delim("~/Documents/ESS/Manuscript/r3/Goal Competition Model/model-data.txt")
attach(ModelData)

MRuns <- unique(run)
MPhase1 <- numeric()
MPhase3 <- numeric()

for (i in MRuns) {
	MPhase1[i+1] <- dim(ModelData[run==i & kind==0 & got==":KIC",])[1] / dim(ModelData[run==i & kind==0,])[1]
	MPhase3[i+1] <- dim(ModelData[run==i & kind==2 & got==":KIC",])[1] / dim(ModelData[run==i & kind==2,])[1]}
	
MPreEff <- mean(MPhase1); MPreEff
MPostEff <- mean(MPhase3); MPostEff


require(gplots)
quartz(title="Figure 4", width=3.55, height=5, pointsize=10)
par(mai=c(.6, .75, .25, .25), family="sans", xpd=TRUE)
yscale<-.6
barplot2(c(Phases[c(1, 3)]), names.arg=c("Pre-Intervention", "Post-Intervention"), xlab="", plot.ci=T, ci.u=c(Phases[c(1,3)]+Phases[c(2,4)]), ci.l=c(Phases[c(1,3)]-Phases[c(2,4)]), ylim=c(0, yscale), yaxt="n", ylab="Efficiency")
axis(2, at=seq(0, yscale, by=.1), labels=F)
ylabels = format(seq(0, yscale * 100, by=10))
text(par("usr")[1]-0.22, seq(0, yscale, by = .1), srt = 0, labels = ylabels, xpd = TRUE)
plotCI(x=c(.7, 1.9), y=c(MPreEff, MPostEff), add=T, cex=2, font=2)

fit1 = lm(MPhase1 ~ phase1)

MeanInterventionLength <- length(CmdStyle[condition=="eff" & phase==2]) / unique(subject[condition=="eff"]); MeanInterventionLength