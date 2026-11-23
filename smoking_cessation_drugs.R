#----------------------------------------------------------------------#
#                                                                      #
# Drug code list creation, with help of dm+d                           #

#                                                                      #
#----------------------------------------------------------------------#

# Read in libraries
library(tidyverse)
library(openxlsx2)

#----------------------------------------------------------------------#
#                                                                      #
# Read in files and clean them                                         #
#                                                                      #
#----------------------------------------------------------------------#

# This is where the dmd files are. Check you have the most recent version.
setwd("data/")

# Read in the dmd datasets
vtm <- read_xlsx("f_vtm.xlsx", sheet = "VTM")
vmp <- read_xlsx("f_vmp.xlsx", sheet = "VMP")
vmp_DrugForm <- read_xlsx("f_vmp.xlsx", sheet = "DrugForm")
vmp_DrugRoute <- read_xlsx("f_vmp.xlsx", sheet = "DrugRoute")
vmp_vpi <- read_xlsx("f_vmp.xlsx", sheet = "VPI")
amp <- read_xlsx("f_amp.xlsx", sheet = "AmpType")
# bnf <- read_xlsx("f_bnf.xlsx", sheet = "BNF_Vmp")
bnf2 <- read_xlsx("data/BNF Snomed Mapping data 20240516.xlsx")

# look ups
ROUTECD <- read_xlsx("f_lookup.xlsx", sheet = "Route")
FORMCD <- read_xlsx("f_lookup.xlsx", sheet = "Form")
UOMCD <- read_xlsx("f_lookup.xlsx", sheet = "UoM")

##### vtm cleaning

# Change the column name to something more suitable, remove those classed as 'invalid',
# And keep only relevant columns.

vtm <- vtm %>% rename(CHEMICAL_NAME = NM) %>%
  filter(is.na(INVALID)) %>% select(VTMID, CHEMICAL_NAME)

##### drug route cleaning

# Add in look-ups and keep relevant columns
vmp_DrugRoute <- left_join(vmp_DrugRoute, rename(ROUTECD, ROUTECD = CD, ROUTE_DESC = DESC), by = "ROUTECD")
vmp_DrugRoute <- vmp_DrugRoute %>% select(VPID, ROUTECD, ROUTE_DESC)

# NOTE: VMP is not unique for drug routes as one VMP can have multiple routes

##### drug form cleaning

# Add in look ups
vmp_DrugForm <- left_join(vmp_DrugForm, rename(FORMCD, FORMCD = CD, FORM_DESC = DESC), by = "FORMCD")

# Drug form is unique for each VMP

##### amp cleaning

# remove invalid codes
amp <- amp %>% filter(is.na(INVALID))

##### vmp cleaning

# remove invalid vmps
vmp <- vmp %>% filter(is.na(INVALID))

# only keep what is needed for the vmp

vmp <- vmp %>% select(VPID, VTMID, GENERIC_NAME = NM)
nrow(vmp)

# Add in VTM
vmp <- left_join(vmp, vtm, by = "VTMID")
nrow(vmp)

# Add in drug form
vmp <- left_join(vmp, vmp_DrugForm, by = "VPID")
nrow(vmp)

# Add in drug route
vmp <- left_join(vmp, vmp_DrugRoute, by = "VPID")
nrow(vmp) # more rows created as one VMP can have multiple routes

# Add in BNF data
# vmp <- left_join(vmp, bnf, by = "VPID") # use this if BNF is present in data

# Otherwise use this:
vmp <- left_join(vmp, select(bnf2, BNF = `BNF Code`, VPID = `SNOMED Code`), by = "VPID")

##### CPRD code browser and dm+d

cprd <- read.delim("data/CPRDAurumProduct.txt",
                 colClasses = "character", quote = "") # Need to use 'quote = ""'

nrow(cprd)
cprd %>% filter(dmdid != "") %>% pull(dmdid) %>% unique() %>% length()
cprd %>% filter(dmdid %in% c(vmp$VPID, amp$APID)) %>% nrow() # only 8 dmdids don't seem to match - maybe historical codes?

cprd_small <- cprd %>% filter(dmdid == "")
cprd_small %>% arrange(desc(as.numeric(DrugIssues))) %>% head(50) # Flucloxacillin  Oral suspension  125 mg/5 ml
# Important drugs are missed, with hundreds of thousands of prescriptions
# the prodcodeid is not a dmd code
cprd_small %>% filter(ProdCodeId %in% c(vmp$VPID, amp$APID)) %>% nrow()

#----------------------------------------------------------------------#
#                                                                      #
# Example of drug creation: inhaled corticosteroids (ICS)              #
#                                                                      #
#----------------------------------------------------------------------#

#change the working directory

setwd("XXXX - CHANGE to APPROPRIATE DIRECTORY")

# STAGE 1: find all appropriate drugs within the BNF chapter

# STAGE 3: Then, use this to identify all drugs included branded and generic.

# # # # # # # stage 1

# - Filter to the appropriate route
# - identify all the different drugs within that section

# ICS
vmp %>% filter(str_starts(BNF, "030200")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(CHEMICAL_NAME) %>%
  unique() # %>%
 # write.csv("ICS_stage1.csv", row.names = FALSE)

# # # # # # stage 2

#   we just double check that there are not any more combinations of drugs out there that
#   we have missed somehow, by filtering by chemical name instead of BNF chapter.

vmp %>% filter(CHEMICAL_NAME %in% c("Budesonide", "Beclometasone", "Ciclesonide",
                                    "Fluticasone", "Mometasone")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) # %>%
#  write.csv("ICS_stage2.csv", row.names = FALSE)

# # # # # Stage 3

#   of the generic VMP.
# - The VMP and AMP codes can be combined to give all required dm+d codes.

vmp_amp <- vmp %>% filter(CHEMICAL_NAME %in% c("Budesonide", "Beclometasone", "Ciclesonide",
                                    "Fluticasone", "Mometasone")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>%
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) # %>%
 #  write.csv("ICS_stage3.csv", row.names = FALSE) # can write if you want to check it

# # # # # Stage 4

# - If working with CPRD data, these dmdid codes can be used to cut down the number of product terms that
#   are left to check through

# CPRD product browser is made up of both VMP (generic) and AMP (branded) codes
dmdids <- unique(c(vmp_amp$VPID, vmp_amp$APID))

in_cprd1 <- cprd %>% filter(dmdid %in% dmdids) # extract as is

# then, we can look through the rest of the codes after removing all dmdids
cprd_no_dmdid <- cprd %>% filter(dmdid == "")

# and remember to write the tag file etc.

source("data/searchProdBro.R")

in_cprd2 <- searchProdBro(cprd_no_dmdid, searchlist = c("Budesonide", "Beclometasone", "Ciclesonide",
                                            "Fluticasone", "Mometasone")) %>%
  filter(allterms == 1)

# CPRD using the dmdid to give the finished codelist.
# write.csv(in_cprd2, "ICS_stage4.csv", row.names = FALSE)

# # # # #  # And finally, our CPRD codelist combines those that have dmdid codes and those that don't:

in_cprd <- in_cprd2 %>% select(matches(colnames(in_cprd1))) %>% bind_rows(in_cprd1)

# write.csv(in_cprd2, row.names = FALSE)

# (comparison with just doing it the normal way:

cprd_lots <- searchProdBro(cprd, searchlist = c("Budesonide", "Beclometasone", "Ciclesonide",
                                                        "Fluticasone", "Mometasone")) %>%
  filter(allterms == 1)

table(cprd_lots$allterms)

in_cprd %>% filter(ProdCodeId %in% cprd_lots$ProdCodeId) %>% nrow()
cprd_lots %>% filter(!(ProdCodeId %in% in_cprd$ProdCodeId))
