library(dplyr)
library(reshape2)
library(onewaytests)
library(ggplot2)
library(gridExtra)

# PART - 2
# Inter-individual variance in vaccine titers

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
  data <- melt(data, id.vars = c("P_Id", "Month"))
  data$variable <- paste(data$variable, data$Month, sep = "_")
  data$Month <- NULL
  data
}

# Brown-Forsythe Test without the removal of zeros
bftest.feeder1 <- function(country, col_names, title_name) {
  data <- filter.absolute.data(country, col_names)
  data <- data[complete.cases(data), ]
  totest <- filter(data, variable %in% c( "Polio IgG_7m", "Measles_15m", 
                                            "Pertussis_7m", "Tetanus_7m", 
                                           "Rotavirus IgG_15m"))
  totest$variable <- factor(totest$variable)
  out <- bf.test(value ~ variable, data = totest)
  info <- data.frame(Country = title_name, Data_Used = "Included zero values",out[c(1,3:5)], num_df = out[[2]][1], denom_df = out[[2]][2])
  df <- data.frame(paircomp(out))
  return(list(info, df))
}  

# Brown-Forsythe Test excluding zeros
bftest.feeder2 <- function(country, col_names, title_name) {
  data <- filter.absolute.data(country, col_names)
  data <- data[complete.cases(data), ]
  totest <- filter(data, variable %in% c( "Polio IgG_7m", "Measles_15m", 
                                          "Pertussis_7m", "Tetanus_7m", 
                                          "Rotavirus IgG_15m"))
  totest$variable <- factor(totest$variable)
  totest[totest$value == 0, ] <- NA # removes zero values
  totest <- totest[complete.cases(totest), ]
  out <- bf.test(value ~ variable, data = totest)
  info <- data.frame(Country = title_name, Data_Used = "Excluded zero values",out[c(1,3:5)], num_df = out[[2]][1], denom_df = out[[2]][2])
  df <- data.frame(paircomp(out))
  return(list(info, df))
}  

# Graph for the variance of vaccine titres
graph.generator <- function(country, col_names, title_name = NULL) {
  data <- filter.absolute.data(country, col_names)
  data <- filter(data, variable %in% c("Polio IgG_7m", "Measles_15m", 
                                         "Pertussis_7m", "Tetanus_7m", 
                                         "Rotavirus IgG_15m"))
  # Without removing zeros
  data1 <- data
  data1 <- data1[complete.cases(data1), ]
  data1 <- dcast(data, P_Id~variable)
  data1[,-1] <- log(data1[,-1] + 1)
  M <- apply(data1[,-1], 2, median, na.rm = T)
  data1[,-1] <- data1[,-1]/M
  VAR = apply(data1[,-1], 2, sd, na.rm = T)
  df1 = data.frame(VAR)
  df1$Vaccine <- rownames(df1)
  rownames(df1) <- 1:nrow(df1)
  
  # Excluding zeros
  data2 <- data
  data2[data2$value == 0, ] <- NA # removes zero values
  data2 <- data2[complete.cases(data2), ]
  data2 <- dcast(data2, P_Id~variable)
  data2[,-1] <- log(data2[,-1]+1)
  M <- apply(data2[,-1], 2, median, na.rm = T)
  data2[,-1] <- data2[,-1]/M
  VAR <- apply(data2[,-1], 2, sd, na.rm = T)
  df2 <- data.frame(VAR)
  df2$Vaccine <- rownames(df2)
  rownames(df2) <- 1:nrow(df2)
  
  # Plotting Graph
  df1$Vaccine = gsub("_", " ", df1$Vaccine)
  df1 <- df1[order(df1$VAR),]
  df1$Vaccine = factor(df1$Vaccine, levels = df1$Vaccine)
  df2$Vaccine = gsub("_", " ", df2$Vaccine)
  df2 <- df2[order(df2$VAR),]
  df2$Vaccine = factor(df2$Vaccine, levels = df2$Vaccine)
  
  p1 <- ggplot(df1, aes(Vaccine, VAR))+
    geom_bar(stat = "identity", fill = "skyblue", colour = "black", width = 0.75)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(size=18))+
    labs(x = NULL, y = "Median-corrected \nVariance",
         title = paste(title_name ,"- Including all titre values", sep = " "))+
    scale_y_continuous(limits = c(0, 1))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  p2 <- ggplot(df2, aes(Vaccine, VAR))+
    geom_bar(stat = "identity", fill = "skyblue", colour = "black", width = 0.75)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(size=18))+
    labs(x = NULL, y = "Median-corrected \nVariance",
         title = paste(title_name, "- Excluding zero titre values", sep = " "))+
    scale_y_continuous(limits = c(0, 1))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  my_path <- paste("results/Part - 2/",title_name,"/", title_name," - Plot of Vaccine Variance.pdf", sep = "")
  pdf(my_path, width = 5.8, height = 8.3)
  grid.arrange(p1, p2)
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
colname <- c("Participant_Id", "Age..days...EUPATH_0000579.", colname) #Adding Participant Id and AgeDays

u <- bftest.feeder1(india, colname, "India")
v <- bftest.feeder2(india, colname, "India")
table1 <- rbind(u[[1]], v[[1]])
write.csv(table1, file = "results/Part - 2/India/Infomation of B-F Test.csv", row.names = FALSE)
write.csv(u[[2]], file = "results/Part - 2/India/Pairwise Comp Result - Including Zero Values.csv", row.names = FALSE)
write.csv(v[[2]], file = "results/Part - 2/India/Pairwise Comp Result - Excluding Zero Values.csv", row.names = FALSE)
graph.generator(india, colname, "India")

### Repeat for other countries except for Brazil, South Africa and Tanzania

### In case of those three countries the B-F Test can be performed using the same code
### But when you use graph.generator, there are missing data for some specific antibody titres 
### See "Distribution of Participants at Age 7 and 15 months" dataset in Results Folder. 
### For example, in Brazil - 7 months row, 4 titres have 157 paticipants whereas others have 158 
### So, when you try to dcast them (in the graph.generator function), they get aggreated by length not by value
### So you need to find median and variance seperately without dcasting it
### The code used to generate the plot for Brazil is given below 


brazil <- read.table("tidy data/Brazil.txt", sep = "\t", header = TRUE)
title_name <- "Brazil"
## Substitute the name "Brazil" with "South Arfica"/"Tanzania" in the previous two lines; The code will work


varname <- read.csv("tidy data/Var_names for Antibody Titers.csv", header = TRUE, sep = ",")
varname_iri <- varname[10:18,3]
colname_of_country <- colnames(brazil)
colname <- character()
for (i in 1:length(varname_iri)) {
  tempname <- colname_of_country[grepl(varname_iri[i], colname_of_country, ignore.case = TRUE)]
  colname <- c(colname, tempname)
}
colname <- colname[c(9, 6, 1, 7, 2, 8, 4, 5, 3)]
colname <- c("Participant_Id", "Age..days...EUPATH_0000579.", colname) #Adding Participant Id and AgeDays

#Filtering Data
data <- brazil[, colname]
data <- data[complete.cases(data), ]
colnames(data) <- c("P_Id", "Month", "Measles", "Pertussis", "Polio IgG", "Polio S1",
                    "Polio S2", "Polio S3", "Rotavirus IgG", "RotaVirus IgA", "Tetanus")
for(i in 1:length(data$P_Id)) {
  if (data$Month[i] < 350) {
    data$Month[i] <- "7m"
  } else {
    data$Month[i] <- "15m"
  }
}
data$Month <- factor(data$Month)
temp7 <- data[data$Month == "7m", ]
temp7 <- temp7[, c("P_Id", "Polio IgG", "Pertussis", "Tetanus")]
colnames(temp7) <- c("P_Id", "Polio IgG_7m", "Pertussis_7m", "Tetanus_7m")
temp15 <- data[data$Month == "15m", ]
temp15 <- temp15[, c("P_Id", "Measles", "Rotavirus IgG")]
colnames(temp15) <- c("P_Id", "Measles_15m", "Rotavirus IgG_15m")
data <- merge(temp7, temp15, by = "P_Id")

# Without removing zeros
data1 <- data
data1[,-1] <- log(data1[,-1] + 1)
M <- apply(data1[,-1], 2, median, na.rm = T)
data1[,-1] <- data1[,-1]/M
VAR = apply(data1[,-1], 2, sd, na.rm = T)
df1 = data.frame(VAR)
df1$Vaccine <- rownames(df1)
rownames(df1) <- 1:nrow(df1)

# Excluding zeros
data2 <- data
data2[data2$value == 0, ] <- NA # removes zero values
data2 <- data2[complete.cases(data2), ]
data2[,-1] <- log(data2[,-1]+1)
M <- apply(data2[,-1], 2, median, na.rm = T)
data2[,-1] <- data2[,-1]/M
VAR <- apply(data2[,-1], 2, sd, na.rm = T)
df2 <- data.frame(VAR)
df2$Vaccine <- rownames(df2)
rownames(df2) <- 1:nrow(df2)

# Plotting Graph
df1$Vaccine = gsub("_", " ", df1$Vaccine)
df1 <- df1[order(df1$VAR),]
df1$Vaccine = factor(df1$Vaccine, levels = df1$Vaccine)
df2$Vaccine = gsub("_", " ", df2$Vaccine)
df2 <- df2[order(df2$VAR),]
df2$Vaccine = factor(df2$Vaccine, levels = df2$Vaccine)

p1 <- ggplot(df1, aes(Vaccine, VAR))+
  geom_bar(stat = "identity", fill = "skyblue", colour = "black", width = 0.75)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(size=18))+
  labs(x = NULL, y = "Median-corrected \nVariance",
       title = paste(title_name ,"- Including all titre values", sep = " "))+
  scale_y_continuous(limits = c(0, 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

p2 <- ggplot(df2, aes(Vaccine, VAR))+
  geom_bar(stat = "identity", fill = "skyblue", colour = "black", width = 0.75)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(size=18))+
  labs(x = NULL, y = "Median-corrected \nVariance",
       title = paste(title_name, "- Excluding zero titre values", sep = " "))+
  scale_y_continuous(limits = c(0, 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

my_path <- paste("results/Part - 2/",title_name,"/", title_name," - Plot of Vaccine Variance.pdf", sep = "")
pdf(my_path, width = 5.8, height = 8.3)
grid.arrange(p1, p2)
dev.off()

## Repeat the same for South Africa and Tanzania