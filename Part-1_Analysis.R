library(dplyr)

# PART - 1 
# Distribution of Vaccine Titres 

filter.log2.data <- function(country, col_names) {
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
      data$Month[i] <- 7
    } else {
      data$Month[i] <- 15
    }
    #Justification: The blood samples were usually taken at two points (around at 7 and 15 months)
    #So taking their midpoint (i.e 350 days) and segregating based on mid-point to get v1 and v2 works (7 and 15 months respectively)
  }
  data
}

histogenerator <- function (temp, country_name, title = "Title", cutoff = NA) {
  
  #Make sure that folders containing all country's names exist before (if not create manually)
  mypath <- paste("results/Part - 1/", country_name, "/", title, ".pdf", sep = "")
  
  pdf(mypath)
  par(fig=c(0,1,0.3,1))
  {hist(temp,
        breaks = 20,
        main = title,
        xlab = "Antibody Titre (Log2 Transformed)",
        ylab = "No of Individuals",
        xlim = c(round(min(temp))-1, round(max(temp))+1),
        col = "red"
  )
   if (!is.na(cutoff)) {
      lines(x = c(cutoff, cutoff), y = c(0, 100), lwd = 2)
   }
  }
  par(fig=c(0,1,0,0.3), new=TRUE)
  boxplot(temp, horizontal = TRUE,
          outline = TRUE, width = 10,  
          frame = F, bg = "transparent",
          col = "skyblue")
  dev.off()
}

data.feeder.for.histogenerator <- function(country, col_names, country_name, cutoff) {
  temp <- filter.log2.data(country, col_names)
  col_names <- colnames(temp)
  col_names <- col_names[-c(1,2)] #Removing "P_Id" and "Month"
  
  temp7 <- temp[temp$Month == 7, ]
  temp15 <- temp[temp$Month == 15, ]
  
  for (i in 1:length(col_names)) {
    title <- paste(country_name," - ", col_names[i], " (7 months)", sep = "")
    cut <- cutoff$Log2_cutoff[i]
    histogenerator(temp7[ ,col_names[i]], country_name, title, cut)
  }
  
  for (i in 1:length(col_names)) {
    title <- paste(country_name," - ", col_names[i], " (15 months)", sep = "")
    cut <- cutoff$Log2_cutoff[i]
    histogenerator(temp15[ ,col_names[i]], country_name, title, cut)
  }
}

#Reading and rearranging Cutoff values to match the order of Filter.log2.data
#Note: Log2 of Polio Serotypes Titer is incremented by 0.5 from the cutoff value (Refer Discription of those var_names for more details)  
#Note: Cutoff for Rotavirus IgG is not available

  cutofftable <- read.csv("tidy data/Vaccine cutoff.csv")
  cutofftable <- cutofftable[, c(3,5)]
  Rota_IgG <- data.frame(label = as.factor("Rotavirus IgG"), Cutoff_In_IU_per_L = NA)
  cutofftable <- rbind(cutofftable, Rota_IgG)
  cutofftable <- cutofftable[c(1, 2, 4, 7, 3, 8, 9, 5, 6), ]
  
  log2cutoff <- log2(cutofftable$Cutoff_In_IU_per_L)
  log2cutoff[4:6] <- log2cutoff[4:6] + 0.5
  log2cutofftable <- data.frame(Log2_cutoff = log2cutoff)
  
  cutofftable <- cbind(cutofftable, log2cutofftable)


#Column That need to be selected
  india <- read.table("tidy data/India.txt", sep = "\t", header = TRUE)
  
  colname <- colnames(india)
  colname <- colname[grepl("^[Ll]og2", colname)]
  colname <- c("Participant_Id", "Age..days...EUPATH_0000579.", colname) #Adding Participant Id and AgeDays
  
  data.feeder.for.histogenerator(india, colname, "India", cutofftable)

# Doing for all countries
  bangladesh <- read.table("tidy data/Bangladesh.txt", sep = "\t", header = TRUE)
  brazil <- read.table("tidy data/Brazil.txt", sep = "\t", header = TRUE)
  nepal <- read.table("tidy data/Nepal.txt", sep = "\t", header = TRUE)
  pakistan <- read.table("tidy data/Pakistan.txt", sep = "\t", header = TRUE)
  peru <- read.table("tidy data/Peru.txt", sep = "\t", header = TRUE)
  south_africa <- read.table("tidy data/South Africa.txt", sep = "\t", header = TRUE)
  tanzania <- read.table("tidy data/Tanzania.txt", sep = "\t", header = TRUE)
  
  data.feeder.for.histogenerator(bangladesh, colname, "Bangladesh", cutofftable) #Column name is same for all datasets
  data.feeder.for.histogenerator(brazil, colname, "Brazil", cutofftable)
  data.feeder.for.histogenerator(nepal, colname, "Nepal", cutofftable)
  data.feeder.for.histogenerator(pakistan, colname, "Pakistan", cutofftable)
  data.feeder.for.histogenerator(peru, colname, "Peru", cutofftable)
  data.feeder.for.histogenerator(south_africa, colname, "South Africa", cutofftable)
  data.feeder.for.histogenerator(tanzania, colname, "Tanzania", cutofftable)

