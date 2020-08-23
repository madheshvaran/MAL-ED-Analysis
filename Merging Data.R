library(data.table)

par_country <- fread("cleaned data/Participant Country.csv", sep = ",", header = TRUE)
pardata <- fread("cleaned data/Cleaned Participant Data.csv", sep = ",", header = TRUE)
sampledata <- fread("cleaned data/Cleaned Sample Data.csv", sep = ",", header = TRUE)
agedata <- fread("cleaned data/Observation Age.csv", sep = ",", header = TRUE)
obsdata <- fread("cleaned data/Cleaned Observation Data.csv", sep = ",", header = TRUE)

par_country$V1 <- NULL
pardata$V1 <- NULL
sampledata$V1 <- NULL
agedata$V1 <- NULL
obsdata$V1 <- NULL

obsdata <- merge(agedata, obsdata)
agedata <- NULL #Deleting the df to clear RAM

## Checking whether Household Id and Household_Obs Id are same or not
count <- 0
for (i in 1:nrow(par_country)) {
  if (par_country$Household_Observation_Id[i] == par_country$Household_Id[i]) {
    count <-  count + 1
  }
}
print(count) ##As count = nrows(Bangladesh), both Household_ID and Household_Observation ID, the latter can be removed


##Spliting par_country according to country names
par_country$Country..OBI_0001627. <- as.factor(par_country$Country..OBI_0001627.)
str(par_country)
par_country$Household_Observation_Id <- NULL
templist <- split(par_country, par_country$Country..OBI_0001627.)
country_name <- levels(par_country$Country..OBI_0001627.)


##Merging Data for Bangladesh
Bangladesh <- templist[[1]]
setkey(Bangladesh, Household_Id)
setkey(pardata, Household_Id)
merge1  <- merge(Bangladesh, pardata)

setkey(merge1, Participant_Id, Household_Id)
setkey(sampledata, Participant_Id, Household_Id)
merge2 <- merge(merge1, sampledata)

setkey(merge2, Observation_Id, Participant_Id, Household_Id)
setkey(obsdata, Observation_Id, Participant_Id, Household_Id)
finalmerge <- merge(merge2, obsdata)

colname <- colnames(finalmerge)
id1 <- grep("Sample_Id", colname, ignore.case = TRUE)
vector <- c(id1, 1:(id1-1),(id1+1):length(colname))
finalmerge <- finalmerge[ , ..vector] #Moved Sample_Id to the first row

colname <- colnames(finalmerge)
id2 <- grep("Age..days...EUPATH_0000579.", colname, ignore.case = TRUE)
vector <- c(1:4, id2, 5:(id2-1), (id2+1):length(colname))
finalmerge <- finalmerge[ , ..vector] #Moved Sample_Id to the first row

write.table(finalmerge, "tidy data/Bangladesh.txt", sep = "\t")

##Similarly Continue for all other countries


## Vaccines Responses that are done through Blood Antibody Test
var_names <-  read.csv("MAL-ED Prelim work/FinalResult.csv")
#Extracting Antibody Titers and Log2 Values from var_names
id1 <- grep("titer", var_names$label, ignore.case = TRUE)
#Extracting Cut-off limits for a vaccine to be considered positive which is the first 8 columns
id2 <- 1:8

data_that_i_want <- var_names[c(id1, id2),]
write.csv(data_that_i_want ,"tidy data/Var_names for Antibody Titers.csv")


##Creating Cut-Off Data Set
label <- var_names[1:8,c(2,3,7)]
cutoff <- data.frame(Cutoff_In_IU_per_L = c(250, 38, 8 ,10 ,20, 100, 8, 8))
label <- cbind(label, cutoff)

write.csv(label, "tidy data/Vaccine Cutoff.csv")


##No. of Participants in Each country
No_of_par = c()
for(i in 1:length(country_name)) {
  temp <- templist[[i]]
  No_of_par <- c(No_of_par, nrow(temp))
}

file <- data.frame(Name = country_name, No_of_Participants = No_of_par)
write.csv(file, "tidy data/Frequency Table of Participants.csv")
