data <- read.table("raw data/ISASimple_Gates_MAL-ED_phase3_RSRC_ontologyMetadata.txt", header = TRUE, sep = "\t")
##Search for keywords that can potentially contain vaccine data
row.no <- grep("illness", data$label, ignore.case = TRUE, perl = TRUE)

str(row.no)

x <- row.no[1]
result <- data[x,]

for(i in 2){
  x <- row.no[i]
  temp <- data[x,]
  result <- rbind(result, temp)
}

write.csv(result, file = "MAL-ED Prelim work/Results_for_Immunization.csv") #Or the keyowrds searched

##After filtering out the following keywords, they are combined

df1 <- read.csv("Results_for_Antibodies.csv")
df2 <- read.csv("Results_for_Antibody.csv")
df3 <- read.csv("Results_for_Immunization.csv")
df4 <- read.csv("Results_for_Vaccination.csv")
df5 <- read.csv("Results_for_Vaccine.csv")

result <- rbind(df1, df2, df3, df4, df5)

duplicated(result)
res <- result[!duplicated(result), ]

res$X <- NULL
row.names(res) <- NULL

 write.csv(res, "MAL-ED Prelim work/FinalResult.csv")
 