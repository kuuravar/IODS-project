# Kuura Variskallio
# 14.11.2021
# Create `alc` dataset.

# Loaded from https://archive.ics.uci.edu/ml/datasets/Student+Performance
# student-mat.csv (Math course)
# student-por.csv (Portugese language course)
# Column descriptions are found behind the url above.

rm(list=ls())

library(dplyr)

progressbar.avail <- FALSE
# Try loading progressbar for fun. 
# TODO (optional): define warning and finally parts
tryCatch(
  {
    library(progress)
    progressbar.avail <- TRUE
  }, 
  error=function(cond){
    message(paste0("Whoops:", cond))
  })

# Set wd to the script location. Dangerous?
origin.wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# Download the data, unzip
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"

# File and df names
file.names <- c("student-mat.csv", "student-por.csv")
table.names <- c("student.mat", "student.por")

stopifnot(length(file.names) == length(table.names))

temp <- tempfile()
download.file(url, destfile = temp)

pb <- NULL
if (progressbar.avail == TRUE) { pb <- progress_bar$new(total = length(file.names)) }

for (i in 1:length(file.names)) {
  if(!is.null(pb)) { pb$tick() }
  
  # create a variable named in iable.names[i]
  assign(table.names[i], 
         read.csv(unz(temp, file.names[i]), header = TRUE, sep = ";"))
}

unlink(temp)
rm(temp)

# readline(prompt = "Press [Enter] to continue")

## student_mat

print("The structure of the data is: ")
str(student.mat)

# There are 395 rows and 33 columns.
# dim(student.mat)

table(sapply(student.mat, typeof))
# 17 columns are of character type, 
# 16 columns are of type integer.

# student-por

print("The structure of the data is: ")
str(student.por)

# There are 649 rows and 33 columns.
# dim(student.por)

table(sapply(student.por, typeof))
# 17 columns are of character type, 
# 16 columns are of type integer.

# Join the dataset by all other variables, except
# "failures", "paid", "absences", "G1", "G2", "G3"

others <- c("failures", "paid", "absences", "G1", "G2", "G3")

join_by <- colnames(student.mat)[!(colnames(student.mat) %in% others)]

student.mat.por <- inner_join(
  student.mat,
  student.por,
  by = join_by,
  keep = FALSE,
  suffix = c(".m", ".p"))  # suffix to match IODS's alc.csv

str(student.mat.por)
# There are now 370 rows and 39 columns. Additional (6)
# columns are the ones not joined upon ("failures" etc.)
# with the .m and .p suffixes
table(sapply(student.mat.por, typeof))
# 18 columns are of character type, 
# 21 columns are of type integer.

# Next we reconcile variables in `others`.
student.mat.por <- cbind(student.mat.por, 
  sapply(others, function(x) {
    matching.cols <- student.mat.por[, grepl(x, names(student.mat.por))]
    # should be enough to check only one, but let's be careful
    both.numeric <- all(sapply(matching.cols, is.numeric))
  
    if(both.numeric) {
      # find matching columns, e.g. "G1" -> c("G1.m", "G1.p")
      rowMeans(matching.cols)
    } else {
      # First check both are the same
      # stopifnot(all(matching.cols[, 1] == matching.cols[, 2]))
      
      # >>> At least "failure" columns are not the same! To match the
      # official solution, select the second one.
      
      # Non-numeric, take the second match
      res <- matching.cols[, 2, drop = FALSE]
      colnames(res) <- x # "paid.m" -> "paid" for consistency
      res
    }
  })
)

# Now all mergings should be done, copy to other table for convenience.
alc <- student.mat.por
rm(student.mat.por)

# Create the `alc_use` variable.
alc$alc_use <- rowMeans(alc[, c("Walc", "Dalc")])
alc$high_use <- alc$alc_use > 2

# OK, now check against the official data
url <- "https://raw.githubusercontent.com/rsund/IODS-project/master/data/alc.csv"
temp <- tempfile()
download.file(url, destfile = temp)
official <- read.csv(temp, header = TRUE, sep = ",")

# First check if there are columns not present in our solution
colnames(official) [! (colnames(official) %in% colnames(alc))]
# [1] "n"    "id.p" "id.m" "cid" 
# "id" and "cid" vars are technical, "n" is 2 used for checking the merge
all(official$n == 2)

# The other way around is empty
colnames(alc) [! (colnames(alc) %in% colnames(official))]

# Remove the technical vars
official <- official[, (colnames(official) %in% colnames(alc))]

# Now reorder columns in both
official <- official[, order(colnames(official))]
alc <- alc[, colnames(official)]
stopifnot(all(colnames(alc) == colnames(official)))

# Check for near equality in numerical columns
numer.cols <- sapply(alc, is.numeric)

all.equal(alc[, numer.cols], official[, numer.cols],
          check.attributes = FALSE)

# Whoops, "absences" are differing at least. Summaries
# for `official$absences` and `alc$absences` are the same, 
# the row orders are therefore probably different. Let's sort by
# all columns.

official <- official[do.call(order, official), ]
alc <- alc[do.call(order, alc), ]

all.equal(alc[, numer.cols], official[, numer.cols],
          check.attributes = FALSE)

# Still doesn't work, but there is now something funny
# with integer values, e.g. `official$failures` is integer
# and `alc$failures` is double.

# Check sorting on integer values only
int.cols <- colnames(alc[, numer.cols])[
  apply(alc[, numer.cols], 2, function(x){ all(x %% 1 == 0)})]

official <- official[do.call(order, official[, int.cols]), ]
alc <- alc[do.call(order, alc[, int.cols]), ]

# Now we have found the problem:
all.equal(alc[, int.cols], official[, int.cols],
          check.attributes = FALSE)
# ... the join should round the values. 

# Round the numeric `others` -columns
alc[, others[-2]] <- apply(alc[, others[-2]], 2, round)

# Try again to order on all
official <- official[do.call(order, official),]
alc <- alc[do.call(order, alc),]

all.equal(alc[, numer.cols], official[, numer.cols],
          check.attributes = FALSE)

# It is the same. Also check the character columns.
all.equal(alc[, -numer.cols], official[, -numer.cols],
          check.attributes = FALSE)

# All works.

# Save to file
setwd("..")
filename <- "alc.csv"
write.csv(alc, file = paste0("./data/", filename), row.names = FALSE)
