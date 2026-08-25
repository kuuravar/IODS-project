# Kuura Variskallio
# 02.12.2021
# Wrangle `BPRS` and `RATS` datasets.

rm(list = ls())

# Set wd to the script location.
origin.wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))


# 1. Load data in wide form


ROOT <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/"

BPRS <- read.csv(paste0(ROOT, "BPRS.txt"), stringsAsFactors = F, sep = " ")
# brief psychiatric rating scale
  
RATS <- read.csv(paste0(ROOT, "rats.txt"), stringsAsFactors = F, sep = "\t")
# rat weights

# Check variable names

colnames(BPRS)
# [1] "treatment" "subject"   "week0"     "week1"     "week2"    
# [6] "week3"     "week4"     "week5"     "week6"     "week7"    
# [11] "week8"

colnames(RATS)
# [1] "ID"    "Group" "WD1"   "WD8"   "WD15"  "WD22"  "WD29" 
# [8] "WD36"  "WD43"  "WD44"  "WD50"  "WD57"  "WD64" 

# Check content and structure

str(BPRS)
dim(BPRS)
table(sapply(BPRS, typeof))
# There are 40 rows, 11 columns, all of integer value. `treatment`
# and `subject` can be factorized, other columns are week 
# measurements.

str(RATS)
dim(RATS)
table(sapply(RATS, typeof))
# There are 16 rows, 13 columns, all of integer value. `ID`
# and `Group` can be factorized, other columns are week 
# measurements.

# Check summaries

summary(BPRS)
min(BPRS[, 3:ncol(BPRS)])
max(BPRS[, 3:ncol(BPRS)])
# There are two treatments, 20 subjects. Week measurement
# span values from 18 to 95.

summary(RATS)
min(RATS[, 3:ncol(RATS)])
max(RATS[, 3:ncol(RATS)])
# There are 16 IDs, 3 groups. Weight is measured by days, and
# spans values from 225 to 628.


# 2. Convert to factors


BPRS$treatment <- as.factor(BPRS$treatment)
BPRS$subject <- as.factor(BPRS$subject)

RATS$ID <- as.factor(RATS$ID)
RATS$Group <- as.factor(RATS$Group)


# 3. Convert to long form


library(tidyverse)

BPRSL <- BPRS %>% 
  gather(key = week, value = bprs, -treatment, -subject)

# Extract week info to int ("weekN" -> N)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(week, 5, 5)))


RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) 

# Extract day info to int ("WDN" -> N), remove WD
RATSL <- RATSL %>%
  mutate(Time = as.integer(substr(WD, 3, 4))) %>%
  select(-WD)


# 4. Compare against wide form

glimpse(BPRSL)
# Rows: 360
# Columns: 4
# $ treatment <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
# $ subject   <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,…
# $ week      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ bprs      <int> 42, 58, 54, 55, 72, 48, 71, 30, 41, 57, 30, 55…

glimpse(BPRS)
# Rows: 40
# Columns: 11
# $ treatment <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
# $ subject   <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,…
# $ week0     <int> 42, 58, 54, 55, 72, 48, 71, 30, 41, 57, 30, 55…
# $ week1     <int> 36, 68, 55, 77, 75, 43, 61, 36, 43, 51, 34, 52…
# $ week2     <int> 36, 61, 41, 49, 72, 41, 47, 38, 39, 51, 34, 49…
# $ week3     <int> 43, 55, 38, 54, 65, 38, 30, 38, 35, 55, 41, 54…
# $ week4     <int> 41, 43, 43, 56, 50, 36, 27, 31, 28, 53, 36, 48…
# $ week5     <int> 40, 34, 28, 50, 39, 29, 40, 26, 22, 43, 36, 43…
# $ week6     <int> 38, 28, 29, 47, 32, 33, 30, 26, 20, 43, 38, 37…
# $ week7     <int> 47, 28, 25, 42, 38, 27, 31, 25, 23, 39, 36, 36…
# $ week8     <int> 51, 28, 24, 46, 32, 25, 31, 24, 21, 32, 36, 31…

# Here we have melted the wide format to a long one. Where in the 
# wide format we had 40 rows of `treatment` and `subject` combinations 
# and each had data for 9 weeks, in the long format we add the `week`
# variable resulting in 40*9 = 360 rows. Values and types of the data of course
# remain the same, but they are now coded into `bprs` variable.
# All in all, apart from the variable names, the two data are equivalent.

stopifnot(min(BPRS[, 3:ncol(BPRS)]) == min(BPRSL$bprs))
stopifnot(max(BPRS[, 3:ncol(BPRS)]) == max(BPRSL$bprs))

summary(BPRSL)
#  treatment    subject         week        bprs      
#  1:180     1      : 18   Min.   :0   Min.   :18.00  
#  2:180     2      : 18   1st Qu.:2   1st Qu.:27.00  
#            3      : 18   Median :4   Median :35.00  
#            4      : 18   Mean   :4   Mean   :37.66  
#            5      : 18   3rd Qu.:6   3rd Qu.:43.00  
#            6      : 18   Max.   :8   Max.   :95.00  
#            (Other):252  

# Let's combine manually the week columns of wide data and
# generate summary.
summary(as.vector(as.matrix(BPRS[, 3:ncol(BPRS)])))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   18.00   27.00   35.00   37.66   43.00   95.00 

# And check the factors separately.

table(BPRS$treatment)
# 1  2 
# 20 20

table(BPRS$subject)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
# 2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 

# Multiplying by 9 (=n weeks), we conclude the data is - on the
# summary level - the same as in the long form. We can also
# check that the bprs distributions are the same

all(table(BPRSL$bprs) == table(as.vector(as.matrix(BPRS[, 3:ncol(BPRS)]))))
# [1] TRUE

# Checking for correct week to bprs pairing would require mutating
# the data into same format (long or wide).
# E.g. 
BPRSW <- BPRSL %>% 
  pivot_wider(names_from = week, values_from = bprs, names_prefix = "week")

# check equality
stopifnot(all.equal(BPRS, BPRSW, check.attributes = FALSE))

# Then do the checks for RATS


glimpse(RATSL)
# Rows: 176
# Columns: 4
# $ ID     <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1…
# $ Group  <fct> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,…
# $ Weight <int> 240, 225, 245, 260, 255, 260, 275, 245, 410, 405, 445, 555, 470, 535, 520, 510, 250…
# $ Time   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,…

glimpse(RATS)
# Rows: 16
# Columns: 13
# $ ID    <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
# $ Group <fct> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3
# $ WD1   <int> 240, 225, 245, 260, 255, 260, 275, 245, 410, 405, 445, 555, 470, 535, 520, 510
# $ WD8   <int> 250, 230, 250, 255, 260, 265, 275, 255, 415, 420, 445, 560, 465, 525, 525, 510
# $ WD15  <int> 255, 230, 250, 255, 255, 270, 260, 260, 425, 430, 450, 565, 475, 530, 530, 520
# $ WD22  <int> 260, 232, 255, 265, 270, 275, 270, 268, 428, 440, 452, 580, 485, 533, 540, 515
# $ WD29  <int> 262, 240, 262, 265, 270, 275, 273, 270, 438, 448, 455, 590, 487, 535, 543, 530
# $ WD36  <int> 258, 240, 265, 268, 273, 277, 274, 265, 443, 460, 455, 597, 493, 540, 546, 538
# $ WD43  <int> 266, 243, 267, 270, 274, 278, 276, 265, 442, 458, 451, 595, 493, 525, 538, 535
# $ WD44  <int> 266, 244, 267, 272, 273, 278, 271, 267, 446, 464, 450, 595, 504, 530, 544, 542
# $ WD50  <int> 265, 238, 264, 274, 276, 284, 282, 273, 456, 475, 462, 612, 507, 543, 553, 550
# $ WD57  <int> 272, 247, 268, 273, 278, 279, 281, 274, 468, 484, 466, 618, 518, 544, 555, 553
# $ WD64  <int> 278, 245, 269, 275, 280, 281, 284, 278, 478, 496, 472, 628, 525, 559, 548, 569


# Here we have melted the wide format to a long one. Where in the 
# wide format we had 16 rows of `ID` and `Group` combinations 
# and each had data for 11 measurements, in the long format we add the `Time`
# variable resulting in 16*11 = 176 rows. Values and types of the data
# remain the same, but they are now coded into `Weight` variable.

stopifnot(min(RATS[, 3:ncol(RATS)]) == min(RATSL$Weight))
stopifnot(max(RATS[, 3:ncol(RATS)]) == max(RATSL$Weight))

summary(RATSL)
#       ID      Group      Weight           Time      
# 1      : 11   1:88   Min.   :225.0   Min.   : 1.00  
# 2      : 11   2:44   1st Qu.:267.0   1st Qu.:15.00  
# 3      : 11   3:44   Median :344.5   Median :36.00  
# 4      : 11          Mean   :384.5   Mean   :33.55  
# 5      : 11          3rd Qu.:511.2   3rd Qu.:50.00  
# 6      : 11          Max.   :628.0   Max.   :64.00  
# (Other):110 

# Let's combine manually the WD columns of wide data and
# generate summary.
summary(as.vector(as.matrix(RATS[, 3:ncol(RATS)])))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 225.0   267.0   344.5   384.5   511.2   628.0 

# And check the factors separately.

table(RATS$Group)
# 1 2 3 
# 8 4 4

table(BPRS$ID)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
# 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 

# Multiplying by 11 (=n day measurements), we conclude the data is - on the
# summary level - the same as in the long form. We can also
# check that the weight distributions are the same

all(table(RATSL$Weight) == table(as.vector(as.matrix(RATS[, 3:ncol(RATS)]))))
# [1] TRUE

# Checking for correct day to weigth pairing would require mutating
# the data into same format (long or wide).
# E.g. 
RATSW <- RATSL %>% 
  pivot_wider(names_from = Time, values_from = Weight, names_prefix = "WD")

# check equality
stopifnot(all.equal(RATS, RATSW, check.attributes = FALSE))


# It appears both datasets have been melted to long form
# without loss of information.

# Save the data.
# Save to file
setwd("..")
write.csv(BPRSL, "./data/bprsl.csv", row.names = TRUE)
write.csv(RATSL, "./data/ratsl.csv", row.names = TRUE)
