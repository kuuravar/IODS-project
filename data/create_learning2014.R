# Kuura Variskallio
# 06.11.2021
# Create `learning2014` dataset.

library(dplyr)

# Set wd to the script location
origin.wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# Read in the data.
url <- "http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
learning2014 <- read.csv(file = url, header = TRUE, sep = "\t")

# Explore dimensions and structure
print(paste0("The raw data has ", dim(learning2014)[1], " rows and ",
             dim(learning2014)[2], " columns")) # or use nrow(), ncol()

print("The structure of the data is: ")
str(learning2014)

# Wait for input
# readline(prompt = "Press [Enter] to continue")

# There are 183 rows and 60 columns
# Seems like most of the columns are integer valued, except
# the `gender` column, which is of character type.
# Data encodes answers to various questions and some
# characteristics of the person answering (Age, gender).
# The full description can be found at
# https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-meta.txt

# Let's create analysis dataset with variables
# gender, age, attitude, deep, stra, surf and points
# following the instructions in 
# https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS2-meta.txt

analysis.data <- subset(learning2014, select = 
    c(gender, Age, Attitude, Points))

# Rename
colnames(analysis.data) <- c("gender", "age", "attitude", "points")

analysis.data$deep <- rowSums(
  select(learning2014, c(D03, D11, D19, D27, 
                         D07, D14, D22, D30, 
                         D06, D15, D23 ,D31)))

analysis.data$stra <- rowSums(
  select(learning2014, c(ST01, ST09, ST17, ST25, 
                         ST04, ST12, ST20, ST28)))

analysis.data$surf <- rowSums(
  select(learning2014, c(SU02, SU10, SU18, SU26, 
                         SU05, SU13, SU21, SU29, 
                         SU08, SU16, SU24, SU32)))

# Reorder
analysis.data <- subset(analysis.data, select = 
    c(gender, age, attitude, deep, stra, surf, points))

# Check the structure
str(analysis.data)

# Check that the attitude was formed as 
# Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj

temp <- rowSums(
  select(learning2014, c(Da, Db, Dc, Dd, 
                         De, Df, Dg, Dh, 
                         Di, Dj)))

stopifnot(all(analysis.data$attitude == temp))

rm(temp) # to not clutter namespace

# Now, scale combination variables by dividing 
# with the number of questions

analysis.data$deep <- analysis.data$deep / 12
analysis.data$stra <- analysis.data$stra / 8
analysis.data$surf <- analysis.data$surf / 12
analysis.data$attitude <- analysis.data$attitude / 10

# Exclude where exam points is zero

analysis.data <- subset(analysis.data, points > 0)

str(analysis.data)

# Set working directory
setwd("..")

# Save data
filename <- "learning2014.csv"
write.csv(analysis.data, file = paste0("./data/", filename), row.names = FALSE)

# Check that reading works
analysis.data.mem <- read.csv(paste0("./data/", filename))

# Check for near equality 
stopifnot(
  all.equal(analysis.data[, -1], analysis.data.mem[, -1], 
            check.attributes = FALSE))

stopifnot(all(analysis.data$gender == analysis.data.mem$gender))

dim(analysis.data.mem)

str(analysis.data.mem)

setwd(origin.wd)

# Seems OK!
