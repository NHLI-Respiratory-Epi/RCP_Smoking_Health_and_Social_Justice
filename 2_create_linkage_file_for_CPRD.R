
library(tidyverse)
library(data.table)

dat <- readRDS("data/cleaned_denominator_step1.RDS")
head(dat)

linkage <- fread("data/Aurum_enhanced_eligibility_November_2024.txt")

head(linkage)
table(linkage$lsoa_e)

dat <- dat[ , 1]  # only need the patid column
dat <- unique(dat) # only need one patid per patient (should already be unique)
setkey(dat, patid)

linkage <- linkage[ , c(1, 4)] # only need patid and lsoa_e

# Step 2: Set key on the ID column of the larger dataset
# This creates a binary search index for fast joining
setkey(linkage, patid)  # Assume 'id' is the common column

# Step 3: Perform the join. This is equivalent to left_join(dat, linkage, by = "patid")
matched_data <- linkage[dat, on = "patid"]

table(matched_data$lsoa_e, useNA = "ifany") # this seems pretty insane - a third of the patients aren't even in the eligibility file
# however, CPRD say this is fine...

# save just the patients that are a '1' and send to CPRD

matched_data <- matched_data[lsoa_e == 1]

data.table::fwrite(matched_data, file = "output/linkage_request.txt",
                   sep = "\t")

n_chunks <- 10
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
  chunk <- matched_data[start_row:end_row]  # Keep only 'patid' column

  print(head(chunk))

  # Define output file name
  file_name <- sprintf("%s/linkage_request_part_%02d.txt", output_dir, i)

  # Write to text file (tab-separated, one column)
  fwrite(chunk, file = file_name, sep = "\t", col.names = TRUE, row.names = FALSE)
}


