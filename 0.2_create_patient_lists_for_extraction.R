# create patid extraction files

library(tidyverse)
library(parallel)
library(data.table)

# all eligible results according to the medcodeid extract:

# Analysis
# Inc 1

dt1 <- fread("data/raw/")
str(dt1) # matches the log
# we only need the first column
dt1 <- dt1[ ,1]

# seems like the below is just a list of the matched observations according to the codelist

#
# head(define_results)

# now, load the denominator file of acceptable patients

dt2 <- fread("data/202503_CPRDAurum_AcceptablePats.txt")
head(dt2)

# only keep the required columns (patid, regenddate, cprd_ddate)

dt2 <- dt2[ , c(1, 9, 11)]

str(dt2)

# Step 2: Set key on the ID column of the larger dataset
# This creates a binary search index for fast joining
setkey(dt2, patid)  # Assume 'id' is the common column

# Step 3: Perform the join

# This matches all rows from dt1 that have a match in dt2
# The result includes only rows from dt1 that match (an inner join)
matched_data <- dt1[dt2, on = "patid", nomatch = 0]

# Note: The syntax is dt1[dt2], but the join is performed on the key of dt2
# This keeps only the rows from dt1 that find a match in dt2

head(matched_data)

matched_data$regenddate <- as.Date(matched_data$regenddate, format = "%d/%m/%Y")
matched_data$cprd_ddate <- as.Date(matched_data$cprd_ddate, format = "%d/%m/%Y")
str(matched_data)

matched_data <- matched_data[regenddate > "2017-12-31" | is.na(regenddate)]
nrow(matched_data)

matched_data <- matched_data[cprd_ddate > "2017-12-31" | is.na(cprd_ddate)]
nrow(matched_data)

# it's not possible to link with eligibility files atm

matched_data <- matched_data[ , 1]

# it will eventually be useful to also link based on linkage eligibility, if this data is available

# head(check)

# Assume your data.table is called `dt` and contains only a column named 'patid'
# Example:

# Step 1: Determine chunk sizes
n_chunks <- 24
n_total <- nrow(matched_data)
chunk_size <- ceiling(n_total / n_chunks)

# Step 2: Create output directory (optional)
output_dir <- "output/chunks/"

# Step 3: Loop to split and save chunks
for (i in 1:n_chunks) {
  # Compute row indices for this chunk
  start_row <- (i - 1) * chunk_size + 1
  end_row <- min(i * chunk_size, n_total)

  # Subset the chunk
  chunk <- matched_data[start_row:end_row, .(patid)]  # Keep only 'patid' column

  # Define output file name
  file_name <- sprintf("%s/patid_chunk_%02d.txt", output_dir, i)

  # Write to text file (tab-separated, one column)
  fwrite(chunk, file = file_name, sep = "\t", col.names = FALSE, row.names = FALSE)
}
