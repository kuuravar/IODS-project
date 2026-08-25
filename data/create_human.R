# Kuura Variskallio
# 22.11.2021
# Create `human` dataset.

# Data description is found in 
# http://hdr.undp.org/en/content/human-development-index-hdi 
# and
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

rm(list = ls())

# Set wd to the script location.
origin.wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# Read in the data

ROOT <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/"

hd <- read.csv(paste0(ROOT, "human_development.csv"), stringsAsFactors = F)
gii <- read.csv(paste0(ROOT, "gender_inequality.csv"), stringsAsFactors = F, na.strings = "..")


# Structure and dimensions

## hd
str(hd)
dim(hd)
table(sapply(hd, typeof))

# The human development data has 195 rows and 8 columns. "Country" and GNI 
# are strings; HDI rank and GNI rank are integers, and the rest are double
# typed. GNI has to be transformed into numeric variable (e.g. "64,992" -> 64992).

hd$Gross.National.Income..GNI..per.Capita <- 
  as.numeric(gsub(",", "", hd$Gross.National.Income..GNI..per.Capita))

## gii
str(gii)
dim(gii)
table(sapply(gii, typeof))

# The gender inequality data has 195 rows and 10 columns. "Country" is 
# a string column; GII rank and Maternal mortality ratio (deaths per 100000 
# live births) are integers, and the rest are double typed.


# Summaries 

summary(hd)

# HDI rank has 7 NA values corresponding to ranks 189-195.
# Human development index spans from 0.3480 to 0.9440, documentation is 
# available from the links above. Other variables are quite unsurprising.

summary(gii)

# HDI rank has 7 NA values corresponding to ranks 189-195.
# There are NA's in most variables.


# Rename with shorter informative names. Abbreviations
# are taken from the links above where available.

# hd

# > colnames(hd)
# [1] "HDI.Rank"                               "Country"                               
# [3] "Human.Development.Index..HDI."          "Life.Expectancy.at.Birth"              
# [5] "Expected.Years.of.Education"            "Mean.Years.of.Education"               
# [7] "Gross.National.Income..GNI..per.Capita" "GNI.per.Capita.Rank.Minus.HDI.Rank"  

colnames(hd) <- c("HDI.Rank", "Country", "HDI", "Life.exp", 
                  "Exp.education", "Mean.education",
                  "GNI", "GNI.minus.HDI.Rank")

# gii

# > colnames(gii)
# [1] "GII.Rank"                                     "Country"                                     
# [3] "Gender.Inequality.Index..GII."                "Maternal.Mortality.Ratio"                    
# [5] "Adolescent.Birth.Rate"                        "Percent.Representation.in.Parliament"        
# [7] "Population.with.Secondary.Education..Female." "Population.with.Secondary.Education..Male."  
# [9] "Labour.Force.Participation.Rate..Female."     "Labour.Force.Participation.Rate..Male." 

colnames(gii) <- c("GII.Rank", "Country", "GII", "MMR",
                   "ABR", "Percent.PR",
                   "SE.f", "SE.m", 
                   "LFPR.f", "LFPR.m")


# Gender inequality mutations

library(dplyr)

gii <- 
  gii %>%
    mutate(
      edu.ratio = SE.f / SE.m,
      labour.ratio = LFPR.f / LFPR.m
    )


# Join by country 
human <- inner_join(hd, gii, by = "Country")
str(human)

# The joined data has 195 rows and 19 columns.

# Let's now check against the official solution. Official result is at 
url <-  "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt"

official <- read.csv(file = url)

str(official)
# There are 155 rows and 8 columns.

any(sapply(official, function(x) { any(is.na(x)) }) == TRUE)
# There are no NA values, let's remove from them from `human` data.

human.comp <- human %>% na.omit

# Let's also remove unneeded columns
human.comp <- 
  human.comp %>% 
    select(edu.ratio, labour.ratio, Exp.education, 
           Life.exp, GNI, MMR, ABR, Percent.PR)

# Cast to integer
human.comp$GNI <- as.integer(human.comp$GNI)

# Check that data are now equal 
stopifnot(all.equal(human.comp, official, check.attributes = FALSE))

print("Success!")

# Save to data folder
# Save to file
setwd("..")
filename <- "human.csv"
write.csv(human, file = paste0("./data/", filename), row.names = FALSE)
