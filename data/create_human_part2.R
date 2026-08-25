# Kuura Variskallio
# 26.11.2021
# Create `human` dataset part 2.

# Data description is found in 
# http://hdr.undp.org/en/content/human-development-index-hdi 
# and
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

# Data describes various aspect of human development and 
# gender inequalities stratified by countries.

# Official dataset solution available at 
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt


rm(list = ls())

# Set wd to the script location.
origin.wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))


# 1. Mutation of the data


# Here we continue from the last time (create_human.R).
# Previously, cleaning and GNI's cast to integer
#
# hd$Gross.National.Income..GNI..per.Capita <- 
#   as.numeric(gsub(",", "", hd$Gross.National.Income..GNI..per.Capita))
#
# or with tidyverse
#
# str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric
#
# was completed. Let's revert a bit and compare the
# columns to match the official solution.

# Read in data
human <- read.csv(file = "human.csv")

str(human)
dim(human)
table(sapply(human, typeof))

# There are 195 rows and 19 columns. Country variable is character
# valued, other variables are numeric and some integer valued.

# > colnames(human)
# [1] "HDI.Rank"           "Country"            "HDI"                "Life.exp"          
# [5] "Exp.education"      "Mean.education"     "GNI"                "GNI.minus.HDI.Rank"
# [9] "GII.Rank"           "GII"                "MMR"                "ABR"               
# [13] "Percent.PR"         "SE.f"               "SE.m"               "LFPR.f"            
# [17] "LFPR.m"             "edu.ratio"          "labour.ratio" 

# The column name descriptions are found behind the links above.
# edu.ratio is the ratio of second education proportion of females and males, SE.f / SE.m.
# labour.ratio is the ratio of labour force participation of females and males, LFPR.f / LFPR.m.


# 2. Column selection


# Read the official data for column names
url <-  "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt"
official <- read.csv(file = url)

# > colnames(official)
# [1] "Edu2.FM"   "Labo.FM"   "Edu.Exp"   "Life.Exp"  "GNI"       "Mat.Mor"   "Ado.Birth" "Parli.F" 

# Select the same columns from our data
library(tidyverse)
human <- 
  human %>% 
  select(Country, edu.ratio, labour.ratio, Exp.education, 
         Life.exp, GNI, MMR, ABR, Percent.PR)


# 3. Remove rows with missing data


human <- human %>% na.omit


# 4. Then remove rows with regions instead of countries, they are
# at the tail.


human <- human[1:(nrow(human) - 7), ]


# 5. Set row names to Country


rownames(human) <- human$Country
human <- human %>% select(-Country)

dim(human)
# [1] 155   8

# Check against the official solution
stopifnot(all.equal(human, official, check.attributes = FALSE))

print("Success!")

# Save to data folder with rownames
# Save to file
setwd("..")
filename <- "human2.csv"
write.csv(human, file = paste0("./data/", filename), row.names = TRUE)
