library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)

# PART - 4
# Attrition-Rate of Vaccines

# Since Brazil, South Africa and Tanzania aggregate by length when dcast function is applied
# This function does the equivalent to melting and dcasting data

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
  
  data
}

vaccine.attrition.plot <- function(country, vaccine){
  data <- substitute.to.dcast(country, vaccine)
  
  data <- data[order(data$M7), ]
  data$Rank <- 1:nrow(data)
  data$Quartile <- data$Rank
  data$Quartile <- cut(data$Rank, breaks = quantile(data$Rank),
                       labels = c("Q1","Q2","Q3","Q4"), type = 5) 
  data$Quartile[1] <- factor("Q1") # Cut function is half-open interval (1,25%] at first cut. So data$Rank 1 is excluded  
  data$Rank <- NULL
  
  data$FC <- (data$M15 + 1)/(data$M7 + 1)  
  data$Attrition <- data$FC
  data[which(data$FC < 0.25),"Attrition"] <- rep("Attrition")
  data[which(data$FC >= 0.25), "Attrition"] <- rep("No Attrition")
  data[which(data$FC >= 4), "Attrition"] <- rep("Increase")
  
  vaccine_df <- data.frame(table(data$Quartile, data$Attrition))
  vaccine_df <- rename(vaccine_df, value = Freq)
  vaccine_df <- dcast(vaccine_df, Var1 ~ Var2)
  vaccine_df$Total <- rowSums(vaccine_df[ ,-1])
  
  toplot <- data
  toplot$FC <- NULL
  toplot <- rename(toplot, Legend = Attrition)
  toplot <- toplot[!duplicated(toplot$P_Id) ,]
  toplot <- melt(toplot, id.vars = c("P_Id", "Quartile", "Legend"))
  
  p <- ggplot (na.omit(toplot), aes(variable,value, group = P_Id, colour = Legend)) +
       geom_point() + geom_line() + facet_wrap(~Quartile, nrow = 1) +
       xlab(" ") + ylab(" ") + ggtitle(vaccine) +
       theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"), panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), panel.background = element_blank(),
             axis.line = element_line(colour = "black")) +
       scale_y_log10()
  
  return(list(p, vaccine_df))
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

data.feeder <- function(country, col_names, title_name = NULL) {
  data <- filter.absolute.data(country, col_names)
  temp1 <- vaccine.attrition.plot(data, "Pertussis")
  temp2 <- vaccine.attrition.plot(data, "Tetanus")
  temp3 <- vaccine.attrition.plot(data, "Polio IgG")
  temp4 <- vaccine.attrition.plot(data, "Measles")
  
  pertussis <- temp1[[2]]
  pertussis$Vaccine <- "Pertussis"
  tetanus <- temp2[[2]]
  tetanus$Vaccine <- "Tetanus"
  if (ncol(tetanus) != 6) {
    tetanus$Increase <- 0
    tetanus <- tetanus[ , c(1,2,6,3:5)]
  } # Nepal dataset has no attrition rate above 4 for Tetanus
  
  polio <- temp3[[2]]
  polio$Vaccine <- "Polio"
  measles <- temp4[[2]]
  measles$Vaccine <- "Measles"
  if (ncol(measles) != 6) {
    measles$Attrition <- 0
    measles <- measles[, c(1,6,2:5)]
  } # Some datasets don't have attrition rate below 0.25 for measles as the vaccine is administed at 9th month
  
  mypath <- paste("results/Part - 4/",title_name,"/", sep = "")
  
  whole <- rbind(pertussis, tetanus, polio, measles)
  write.csv(whole, paste(mypath, title_name, " - Part 4 Table.csv", sep = ""), row.names = FALSE)
  
  pdf(paste(mypath,title_name," - Attrition Rate Plot.pdf", sep = ""), width = 9, height = 12)
  grid.arrange(temp1[[1]], temp2[[1]], temp3[[1]], temp4[[1]], nrow = 4, ncol = 1)
  dev.off()
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

data.feeder(bangladesh, colname, "Bangladesh")
data.feeder(brazil, colname, "Brazil")
data.feeder(india, colname, "India")
data.feeder(nepal, colname, "Nepal")
data.feeder(pakistan, colname, "Pakistan")
data.feeder(peru, colname, "Peru")
data.feeder(south_africa, colname, "South Africa")
data.feeder(tanzania, colname, "Tanzania")
