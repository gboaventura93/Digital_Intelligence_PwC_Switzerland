list.files()
#Extracting every DRUG and their REAC 
DRUG19Q1 <- read.table("DRUG19Q1.txt", fill = TRUE, header = TRUE, sep ="$")
DRUG19Q2 <- read.table("DRUG19Q2.txt", fill = TRUE, header = TRUE, sep ="$")
DRUG19Q3 <- read.table("DRUG19Q3.txt", fill = TRUE, header = TRUE, sep ="$")
DRUG19Q4 <- read.table("DRUG19Q4.txt", fill = TRUE, header = TRUE, sep ="$")

REAC19Q1 <- read.table("REAC19Q1.txt", fill = TRUE, header = TRUE, sep ="$")
REAC19Q2 <- read.table("REAC19Q2.txt", fill = TRUE, header = TRUE, sep ="$")
REAC19Q3 <- read.table("REAC19Q3.txt", fill = TRUE, header = TRUE, sep ="$")
REAC19Q4 <- read.table("REAC19Q4.txt", fill = TRUE, header = TRUE, sep ="$")

#Now we have to save just the importants columns for this task
head(DRUG19Q1, n=5)
#in DRUG, column 5 drugnames
head(REAC19Q1, n=5)
#in REAC, just column 3 pt, where is the drugs reactions

#Columns selection
library(dplyr)

dnames1 <- select(DRUG19Q1, primaryid, drugname)
head(dnames1, n=5)
dnames2 <- select(DRUG19Q2, primaryid, drugname)
dnames3 <- select(DRUG19Q3, primaryid, drugname)
dnames4 <- select(DRUG19Q4, primaryid, drugname)

rnames1 <- select(REAC19Q1, primaryid, pt)
rnames2 <- select(REAC19Q2, primaryid, pt)
rnames3 <- select(REAC19Q3, primaryid, pt)
rnames4 <- select(REAC19Q4, primaryid, pt)

#To merge every dnames in one table, same for rnames
dnames12 <- rbind(dnames1,dnames2)
head(dnames12, n=5) 
dnames34 <- rbind(dnames3,dnames4)
dnames <- rbind(dnames12, dnames34)
head(dnames, n=5)

rnames12 <- rbind(rnames1,rnames2)
head(rnames12, n=5) 
rnames34 <- rbind(rnames3,rnames4)
rnames <- rbind(rnames12, rnames34)

#Now we have to merge these two tables in on dataframe in the way that: drugname$pt(drugreaction)
#Important to say that dnames = 3.533.843 rows, while rnames = 2.791.959 rows
#it means we have to filter the missing values with all function to add NaN in a missing value:

df <- merge(dnames, rnames, by ="primaryid", all = TRUE)
head(df, n=5)
summary(df)
df$primaryid

#filtering tramal and lyrica medicines

tramal = filter(df, drugname == "Tramal")
lyrica = filter(df, drugname == "LYRICA")
head(lyrica, n=5)

#Frequencies analysis
freq_tramal <- sort((table(tramal$pt)), decreasing = TRUE)
freq_tramal_percent <- sort(100*prop.table(table(tramal$pt)), 
                    decreasing = TRUE)
freq_tramal[1:10]
freq_tramal_percent[1:10]

freq_lyrica <- sort((table(tramal$pt)), decreasing = TRUE)
freq_lyrica[1:10]

df_tramal = data.frame(tramal)
df_lyrica = data.frame(lyrica)
head(df_lyrica, n=5)

tra_lyr <- intersect(tramal$pt, lyrica$pt)
length(tra_lyr) #33-1 dados semelhantes (1 is NaN)

tra_lyr
write.csv(tra_lyr, file = "C:/Users/Startklar/Desktop/GetJob/Documents/PwC_Business/task5", row.names = FALSE)
