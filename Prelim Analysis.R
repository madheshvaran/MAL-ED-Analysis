bangladesh <- read.table("tidy data/Bangladesh.txt", sep = "\t", header = TRUE)
brazil <- read.table("tidy data/Brazil.txt", sep = "\t", header = TRUE)
india <- read.table("tidy data/India.txt", sep = "\t", header = TRUE)
nepal <- read.table("tidy data/Nepal.txt", sep = "\t", header = TRUE)
pakistan <- read.table("tidy data/Pakistan.txt", sep = "\t", header = TRUE)
peru <- read.table("tidy data/Peru.txt", sep = "\t", header = TRUE)
south_africa <- read.table("tidy data/South Africa.txt", sep = "\t", header = TRUE)
tanzania <- read.table("tidy data/Tanzania.txt", sep = "\t", header = TRUE)

country_names <- c("Bangladesh", "Brazil", "India", "Nepal", "Pakistan", "Peru", "South Africa", "Tanzania")
varname <- read.csv("tidy data/Var_names for Antibody Titers.csv", header = TRUE, sep = ",")
varname_iri <- varname[,3]
colname <- colnames(bangladesh) ## All countries have same column names

colno <- c()
for (i in 1:26){
  x <- varname_iri[i]
  j <- grep(x, colname, ignore.case = TRUE)
  colno <- c(colno, j)
}

bangladesh1 <- bangladesh[, c(1:6, colno)]  #Subsetting Ids, Age, Country and Antibody Titer columns
brazil1 <- brazil[, c(1:6, colno)]
india1 <- india[, c(1:6, colno)]
nepal1 <- nepal[, c(1:6, colno)]
pakistan1 <- pakistan[, c(1:6, colno)]
peru1 <- peru[, c(1:6, colno)]
south_africa1 <- south_africa[, c(1:6, colno)]
tanzania1 <- tanzania[, c(1:6, colno)]




##PART-1: Distribution of No. of Individuals for which vaccine data is available

no_of_individuals <- function(country, col_id) {
  temp <- country[, c("Age..days...EUPATH_0000579.", col_id)]
  temp <- temp[!is.na(temp[,col_id]), ] #Setecting Non-empty Titer Rows
  v1 <- sum(temp$Age..days...EUPATH_0000579. < 350)
  v2 <- sum(temp$Age..days...EUPATH_0000579. > 350)
  data.frame(a = c(v1, v2))
  #Justification: The blood samples were usually taken at two points (around at 7 and 15 months)
  #So taking their midpoint (i.e 350 days) and segregating based on mid-point to get v1 and v2 works (7 and 15 months respectively)
}

Col1 <- data.frame(Name = rep(country_names, each = 2))  
Col2 <- data.frame(Age_in_months = rep(c(7,15), times = 8))
tempdf <- cbind(Col1, Col2)

colname <- colnames(bangladesh1) #All column names are same
colname <- colname[7:15] #Choosing the Titer Columns

for (i in 1:9) {
  v1 <- no_of_individuals(bangladesh1, colname[i])
  v2 <- no_of_individuals(brazil1, colname[i])
  v3 <- no_of_individuals(india1, colname[i])
  v4 <- no_of_individuals(nepal1, colname[i])
  v5 <- no_of_individuals(pakistan1, colname[i])
  v6 <- no_of_individuals(peru1, colname[i])
  v7 <- no_of_individuals(south_africa1, colname[i])
  v8 <- no_of_individuals(tanzania1, colname[i])
  df <- rbind(v1,v2,v3,v4,v5,v6,v7,v8)
  tempdf <- cbind(tempdf, df)
}

print(colname)
names(tempdf) <- c("Name", "Age_in_months", "No_of_Measles_Titer", "No_of_Pertussis_Titer", "No_of_Polio_IgG_Titer", 
                   "No_of_Polio_Serotype1_Titer", "No_of_Polio_Serotype2_Titer", "No_of_Polio_Serotype3_Titer",
                   "No_of_Rotavirus_IgG_Titer", "No_of_Rotavirus_IgA_Titer", "No_of_Tetanus_Titer")

write.csv(tempdf, "results/Distribution of Participants at Age 7 and 15 months.csv")




##PART-2: Histogram for each vaccine, each country 
cutofftable <- read.csv("tidy data/Vaccine cutoff.csv")

histogenerator <- function (country, col_id, cutoff = NA, label_x = "X-Axis", title = "Title") {
  temp <- country[, col_id]
  temp <- temp[!is.na(temp)]
  {hist(temp,
       breaks = 20,
       main = title,
       xlab = label_x,
       xlim = c(round(min(temp))-1, round(max(temp))+1)
  )
  if (!is.na(cutoff)) {
    lines(x = c(cutoff, cutoff), y = c(0, 100), lwd = 2)
  }
  }
}

colname <- colnames(bangladesh1)
colname <- colname[7:15]

#Referring cutofftable to get cutoff values
#Note: Log2 of Polio Serotypes Titer is incremented by 0.5 from the cutoff value (Refer Discription of those var_names for more details)  
#Note: Cutoff for Rotavirus IgG is not available

histogenerator(bangladesh1, colname[1], log2(cutofftable[1,5]), "Measles Titer", "Bangladesh-Measles Titer")
histogenerator(bangladesh1, colname[2], log2(cutofftable[2,5]), "Pertussis Titer", "Bangladesh-Pertussis Titer")
histogenerator(bangladesh1, colname[3], log2(cutofftable[4,5]), "Polio IgG Titer", "Bangladesh-Polio IgG Titer")
histogenerator(bangladesh1, colname[4], log2(cutofftable[7,5]) + 0.5, "Polio Serotype 1 Titer", "Bangladesh-Polio Serotype 1 Titer")
histogenerator(bangladesh1, colname[5], log2(cutofftable[3,5]) + 0.5, "Polio Serotype 2 Titer", "Bangladesh-Polio Serotype 2 Titer")
histogenerator(bangladesh1, colname[6], log2(cutofftable[8,5]) + 0.5, "Polio Serotype 3 Titer", "Bangladesh-Polio Serotype 3 Titer")
histogenerator(bangladesh1, colname[7], label_x = "Rotavirus IgG Titer", title = "Bangladesh-Rotavirus IgG Titer")
histogenerator(bangladesh1, colname[8], log2(cutofftable[5,5]), "Rotavirus IgA Titer", "Bangladesh-Rotavirus IgA Titer")
histogenerator(bangladesh1, colname[9], log2(cutofftable[6,5]), "Tetanus Titer", "Bangladesh-Tetanus Titer")

#Do the same for all other countries