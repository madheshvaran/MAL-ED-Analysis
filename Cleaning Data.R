##Variables Needed to be searched
   whole_var_data <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_ontologyMetadata.txt", header = TRUE, sep = "\t")
   var_names <-  read.csv("MAL-ED Prelim work/FinalResult.csv")
   unclass(var_names$category)
   lvl <- levels(var_names$category) 
  
   var_in_sample <- var_names[var_names$category %in% lvl[4],]
   var_in_obs <- var_names[var_names$category %in% lvl[2],]
   var_in_par <- var_names[var_names$category %in% lvl[3],]
   
  
##Cleaning Sample Data (Working with samll dataset)
   tempdata1 <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_samples.txt", header = TRUE, nrows= 100, sep = "\t")
  
   headername <- colnames(tempdata1)
   iri <- var_in_sample[,2]
   col.no <- numeric()
   
   for (i in iri){
     temp_no <- grep(i, headername, ignore.case = TRUE)
     col.no <- c(col.no, temp_no)
   }
   
   col.no <- c(1:4 , col.no) #Adds the first four columns that contains IDs
   data_that_i_want <- tempdata1[, col.no] ##Not required to be stored, Checking whether the function works
   
  
##Cleaning Sample Data (Whole Dataset)
   sampledata <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_samples.txt", header = TRUE, sep = "\t")
   cleaned_sampledata <- sampledata[, col.no]
   write.csv( cleaned_sampledata,"cleaned data/Cleaned Sample Data.csv")
    
   
##Cleaning Observation Data (Working with small dataset)
   tempdata2 <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_observations.txt", header = TRUE, nrows = 100, sep = "\t")
   
   headername2 <- colnames(tempdata2)
   iri2 <- var_in_obs[,2]
   col.no2 <- numeric()
   
   for (j in iri2) {
      temp_no2 <- grep(j, headername2, ignore.case = TRUE)
      col.no2 <- c(col.no2, temp_no2)
   }
   
   col.no2 <- c(1:3 , col.no2) #Adds the first three columns that contains IDs
   data_that_i_want <- tempdata2[, col.no2] ##Not required to be stored, Checking whether the function works
  
##Cleaning Observation Data (Whole Dataset)  
   obsdata <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_observations.txt", header = TRUE, sep = "\t")
   cleaned_obsdata <- obsdata[, col.no2]
   write.csv(cleaned_obsdata, "cleaned data/Cleaned Observation Data.csv")
   
   
##Extracting Age (Days) when Observation was taken
   col.no3 <- grep("EUPATH_0000579", headername2) #EUPATH_0000579 is the iri for Age (Days) when an observation was taken
   col.no3 <- c(1:3, col.no3)  #Adds the first three columns that contains IDs
   
   obs_agedata <- obsdata[, col.no3]
   write.csv(obs_agedata, "cleaned data/Observation Age.csv")
   
   
##Cleaning Participant Data (Whole Dataset)  
   pardata <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_participant.txt", header = TRUE, sep = "\t")
   
   headername3 <- colnames(pardata)
   iri3 <- var_in_par[,2]
   col.no4 <- numeric()
   
   for (i in iri3){
      temp_no <- grep(i, headername3, ignore.case = TRUE)
      col.no4 <- c(col.no4, temp_no)
   }
   
   col.no4 <- c(1:2 , col.no4) #Adds the first two columns that contains IDs
   cleaned_pardata <- pardata[, col.no4]
   write.csv(cleaned_pardata,"cleaned data/Cleaned Participant Data.csv")
   

##Extracting Country of the Participant
   household_data <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_households.txt", header = TRUE, sep = "\t")
   str(household_data)
   cleaned_householddata <- household_data[,c(1,2,10)] #Selecting columns containing Household Observation ID, Household ID and Country
   
   #Removing NA columns (which are unnecessary)
   cleaned_householddata <- cleaned_householddata[!is.na(cleaned_householddata$Country..OBI_0001627.),]
   write.csv(cleaned_householddata, "cleaned data/Participant Country.csv")
   