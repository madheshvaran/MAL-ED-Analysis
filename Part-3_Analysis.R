library(dplyr)

# PART - 3
# Scatterplot Matrix between vaccine titers

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
  temp7 <- data[data$Month == "7m", ]
  temp7 <- temp7[, c("P_Id", "Polio IgG", "Pertussis", "Tetanus")]
  colnames(temp7) <- c("P_Id", "Polio IgG (7m)", "Pertussis (7m)", "Tetanus (7m)")
  temp15 <- data[data$Month == "15m", ]
  temp15 <- temp15[, c("P_Id", "Measles")]
  colnames(temp15) <- c("P_Id", "Measles (15m)")
  data <- merge(temp7, temp15, by = "P_Id")
  data
}

scatterplot.matrix <- function(country, col_names, title_name) {
  dat <- filter.absolute.data(country, col_names)
  dat <- dat[complete.cases(dat), ]
  dat <- dat[, -1]
  dat <- log(dat+1)
  my_path <- paste("results/Part - 3/", title_name,".pdf", sep = "")
  pdf(my_path)
  pairs(dat, lower.panel = panel.smooth, pch = 20, lwd = 2,
        main = paste(title_name, " - Scatterplot Matrix", sep = ""))
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
# All country datasets have same column names

bangladesh <- read.table("tidy data/Bangladesh.txt", sep = "\t", header = TRUE)
brazil <- read.table("tidy data/Brazil.txt", sep = "\t", header = TRUE)
nepal <- read.table("tidy data/Nepal.txt", sep = "\t", header = TRUE)
pakistan <- read.table("tidy data/Pakistan.txt", sep = "\t", header = TRUE)
peru <- read.table("tidy data/Peru.txt", sep = "\t", header = TRUE)
south_africa <- read.table("tidy data/South Africa.txt", sep = "\t", header = TRUE)
tanzania <- read.table("tidy data/Tanzania.txt", sep = "\t", header = TRUE)

scatterplot.matrix(bangladesh, colname, "Bangladesh")
scatterplot.matrix(brazil, colname, "Brazil")
scatterplot.matrix(india, colname, "India")
scatterplot.matrix(nepal, colname, "Nepal")
scatterplot.matrix(pakistan, colname, "Pakistan")
scatterplot.matrix(peru, colname, "Peru")
scatterplot.matrix(south_africa, colname, "South Africa")
scatterplot.matrix(tanzania, colname, "Tanzania")

