#SEABASS 2026
#PASSIVE ACOUSTIC DENSITY ESTIMATION SESSION
#3rd July 2026

#Haggis Hunting exercise
#Written by Danielle Harris

#The aim of this exercise is to read in distance sampling data into R and analyse it using the Distance package (written by David Miller).
#The same analyses can be conducted in Program Distance.  

########################################################
#Step 1 - Load the Distance library

library(Distance)
#this may require the Distance library and associated packages to be installed, if they are not already.  
#Connection to the internet will usually be required to download and install the required packages.

#########################################################
#Step 2 - Load the data

#browse for the file (NB: file.choose saves the path name to the file)
data<-file.choose()
#read in the .csv file
datain<-read.csv(data, header=TRUE)
head(datain)

#plot measured distances vs estimated distances

plot(datain$distance[na.omit(datain$est.dist)], na.omit(datain$est.dist))
abline(0,1)
##########################################################
#Step 3 - Explore the data

#TASK: 	Explore the data structure and find and note the values for: the number of detected animals, n, and the minimum and maximum distances recorded

dim(datain)[1]
range(datain$distance)

#plot a histogram of the distances
hist(datain$distance,main="Distances of detections",xlab="Range (m)")

#TASK: What can you tell about the detectability of the haggis up to 50 m from this histogram?  
#Is there any evidence that animals were being missed at larger ranges?
############################################################################
#Step 4 - fit a detection function

#First fit a half normal detection function with default settings for adjustment terms and truncation to the data. 
#Note that convert.units is required in order to return the density as per hectare.

#TASK: what are the default settings for adjustment terms and truncation to the data?  Query ?ds to find out.

#fit a detection function
haggis.halfnorm<-ds(datain,key="hn",transect="point",convert_units=0.01)
#look at the summary results
summary(haggis.halfnorm)

#TASK: note the AIC score - this can also be retrieved using
summary(haggis.halfnorm)$ds$aic

###########################################################
#Step 5 - Plot the detection function and scaled histogram

plot(haggis.halfnorm,main="Half normal detection function")

#produce goodess of fit statistics and plot the QQ plot
fit.test<-ddf.gof(haggis.halfnorm$ddf)
fit.test

#TASK: use the notes from the Exercise document to interpret these goodness of fit results.

############################################################
#Step 6 - try a different detection function - the hazard rate key function

#TASK: Set up another analysis using this detection function.  
#You may want to constrain the adjustment terms (pay attention to the warning messages too).  
#Take a look at the results and compare them to the half normal detection function.  
#Which model does the goodness of fit tests and AIC score suggest is the preferred model?  
#Are the density results very different?

haggis.hazrate<-ds(datain,key="hr",transect="point",convert_units=0.01)
haggis.hazrate.noadj<-ds(datain,key="hr",transect="point",adjustment=NULL,convert_units=0.01)

plot(haggis.hazrate)


############################################################
#Step 7 - try truncation

#TASK: Set up a new analysis that uses the half normal detection function but truncate 5% of the observations.  
#Take a look at the results and compare them to the half normal detection function with no truncation.  
#Which model does the goodness of fit tests suggest is the preferred model?  
#Remember that AIC cannot be used to choose between a truncated and untruncated model.  
#Are the density results very different?

haggis.halfnorm.trunc<-ds(datain,key="hn",transect="point",convert_units=0.01,truncation="5%")

#You can also run an analysis with using the hazard rate detection function. 
#Use AIC to compare between the two truncated analyses (if the truncation distance is the same).  
#Which detection function is preferred?

############################################################
#Step 8 - produce some summary results from your preferred model

#e.g., (remember to use the name of your preferred model)

#e.g.,
haggis_summary<-haggis.halfnorm.trunc$dht$individuals$summary
haggis_density<-haggis.halfnorm.trunc$dht$individuals$D

#TASK: Using the results of your chosen model and the standard distance sampling formula (in the Exercise document), work out the density estimate by hand. 
#NB: no false positives were found in this analysis and you'll need to adjust the estimate to account for the females.



