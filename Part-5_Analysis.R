library(reshape2)
library(dplyr)
library(Hmisc)

substitute.to.dcast <- function(country, vaccine) {
  data <- country[ ,c("P_Id", "Month", vaccine)]
  
  dataM7 <- data[which(data$Month == "7m"), ]
  dataM7$Month <- NULL
  colnames(dataM7) <- c("P_Id", "M7")
  
  dataM15 <- data[which(data$Month == "15m"), ]
  dataM15$Month <- NULL
  colnames(dataM15) <- c("P_Id", "M15")
  
  data <- merge(dataM7, dataM15, by = "P_Id")
  # This merges data that has both titre values for month 7 and 15; removes any participant that has only one of the titres values
  # This helps removing non-complete cases as we need to find the attrition rate of vaccine responses from 7 to 15 months
  data$Vaccine <- vaccine
  data
}

filter.absolute.data <- function(country, col_names) {
  data <- country[, col_names]
  data <- data[complete.cases(data), ] # Removes NA values and retains rows that have antibody titre
  colnames(data) <- c("P_Id", "Month",
                      "Measles", "Pertussis",
                      "Polio IgG", "Polio S1",
                      "Polio S2", "Polio S3",
                      "Rotavirus IgG", "RotaVirus IgA",
                      "Tetanus")
  
  for(i in 1:length(data$P_Id)) {
    if (data$Month[i] < 350) {
      data$Month[i] <- "7m"
    } else {
      data$Month[i] <- "15m"
    }
    #Justification: The blood samples were usually taken at two points (around at 7 and 15 months)
    #So taking their midpoint (i.e 350 days) and segregating based on mid-point to get v1 and v2 works (7 and 15 months respectively)
  }
  data$Month <- factor(data$Month)
  data <- data[ ,c("P_Id", "Month", "Measles", "Pertussis","Polio IgG", "Tetanus")]
  data
}

# Problem with dcast! (Aggregates by length instead of values)
# Used this code to mimic the function of dcast

vaccine.data <- function(country, col_names, title_name) {
  dat <- filter.absolute.data(country, colname)
  pertussis <- substitute.to.dcast(dat, "Pertussis")
  tetanus <- substitute.to.dcast(dat, "Tetanus")
  polio <- substitute.to.dcast(dat, "Polio IgG")
  measles <- substitute.to.dcast(dat, "Measles")
  
  whole_data <- rbind(pertussis, tetanus, polio, measles)
  
  whole_data$FC <- (whole_data$M7+1) / (whole_data$M15+1)
  whole_data <- select(whole_data, P_Id, Vaccine, FC)
 
  pertussis <- whole_data[which(whole_data$Vaccine == "Pertussis"), ]
  pertussis$Vaccine = NULL
  colnames(pertussis) <- c("P_Id","Pertussis")
  
  tetanus <- whole_data[which(whole_data$Vaccine == "Tetanus"), ]
  tetanus$Vaccine = NULL
  colnames(tetanus) <- c("P_Id","Tetanus")
  
  polio <- whole_data[which(whole_data$Vaccine == "Polio IgG"), ]
  polio$Vaccine = NULL
  colnames(polio) <- c("P_Id","Polio IgG")
  
  measles <- whole_data[which(whole_data$Vaccine == "Measles"), ]
  measles$Vaccine = NULL
  colnames(measles) <- c("P_Id","Measles")
  
  temp1 <- merge(pertussis, tetanus, by = "P_Id")
  temp2 <- merge(polio, measles, by = "P_Id")
  whole_data <- merge(temp1, temp2, by = "P_Id")
  whole_data <- whole_data[!duplicated(whole_data$P_Id), ]
  whole_data <- na.omit(whole_data)
  
  FCm <- rcorr(as.matrix(whole_data[ , -1]), type = "spearman")
  
  mypath <- paste("results/Part - 5/",title_name,"/", sep = "")
  write.csv(FCm$r, paste(mypath,title_name," - Correlation Table.csv", sep=""), row.names = TRUE)
  write.csv(FCm$P, paste(mypath,title_name," - p-value Table.csv", sep=""), row.names = TRUE)
}

india <- read.table("tidy data/India.txt", sep = "\t", header = TRUE)
varname <- read.csv("tidy data/Var_names for Antibody Titers.csv", header = TRUE, sep = ",")
varname_iri <- varname[10:18,3]
colname_of_country <- colnames(india)
colname <- character()
for (i in 1:length(varname_iri)) {
  tempname <- colname_of_country[grepl(varname_iri[i], colname_of_country, ignore.case = TRUE)]
  colname <- c(colname, tempname)
}

colname <- colname[c(9, 6, 1, 7, 2, 8, 4, 5, 3)]
colname <- c("Participant_Id", "Age..days...EUPATH_0000579.", colname) # Column Names will be same for all datasets

bangladesh <- read.table("tidy data/Bangladesh.txt", sep = "\t", header = TRUE)
brazil <- read.table("tidy data/Brazil.txt", sep = "\t", header = TRUE)
nepal <- read.table("tidy data/Nepal.txt", sep = "\t", header = TRUE)
pakistan <- read.table("tidy data/Pakistan.txt", sep = "\t", header = TRUE)
peru <- read.table("tidy data/Peru.txt", sep = "\t", header = TRUE)
south_africa <- read.table("tidy data/South Africa.txt", sep = "\t", header = TRUE)
tanzania <- read.table("tidy data/Tanzania.txt", sep = "\t", header = TRUE)

vaccine.data(bangladesh, colname, "Bangladesh")
vaccine.data(brazil, colname, "Brazil")
vaccine.data(india, colname, "India")
vaccine.data(nepal, colname, "Nepal")
vaccine.data(pakistan, colname, "Pakistan")
vaccine.data(peru, colname, "Peru")
vaccine.data(south_africa, colname, "South Africa")
vaccine.data(tanzania, colname, "Tanzania")
