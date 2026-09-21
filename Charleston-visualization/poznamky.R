#LOADING DATA NEWBORN.TXT
dataNewborns <- read.delim("newborns.txt")
str(dataNewborns) #checking data
#factorizing categorical values
dataNewborns$edu.M <- factor(dataNewborns$edu.M, labels = c("ZS", "SS", "SSm", "VS"))
dataNewborns$sex.C <- as.factor(dataNewborns$sex.C)
str(dataNewborns)