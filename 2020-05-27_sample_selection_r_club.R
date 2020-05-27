# R club sample selection stuff 
# load libraries
library(tidyverse) 

# load HAKE sample metadata
# We can't share coordinates so lat and long have been removed
load(file = "/Users/katherinerovinski/GIT/R-Club-Sing-A-Long/Hake_DFO_FEAT.Rdata")

DFO_FEAT

DFO_FEAT %>%
  group_by(box) %>%
  count() 

# ideally, I would like 35 samples from each "box"
# use sample_n to  randomly select row, in this case grouped based on box
# set.set makes this random selection reproducible
set.seed(9)
DFO_FEAT_extract <- DFO_FEAT %>% 
  group_by(box) %>% 
  sample_n(35, replace = TRUE) %>%
  ungroup()

# show me the distinct rows
DFO_FEAT_extract %>%
  distinct()

DFO_FEAT_extract %>%
  group_by(finclip_id) %>%
  count() %>%
  filter(n > 1)

# dump duplicates with unique
DFO_FEAT_extract_1 <-  unique(DFO_FEAT_extract)

# does sample_n replace after each sampling?
DFO_FEAT_extract_1 %>%
  group_by(box) %>%
  count()

# dang, it sure does!

# use sample_n and if else statement to randomly select within each "box"
# set the number of samples to extract from each box
n_samples <- 35
set.seed(9)
DFO_FEAT_extract_2 <- DFO_FEAT %>% 
  group_by(box) %>% 
  sample_n(if(n() < n_samples) n() else n_samples) %>%
  ungroup()

# note: dplyr::if_else is too strict and wont work here

# looking good!
DFO_FEAT_extract_2 %>%
  group_by(box) %>%
  count() 

# sweet this works!
DFO_FEAT_extract_2 %>%
  distinct()

##### nesting stuff #####
# what if we want to sample different numbers of individuals from different groups?
# for example different numbers from different "regions"
# we can use `nest` to create data.frames within a data.frames
# and sample rows from those data.frames n times based on value of 
# another column using map2*
# I don't fully understand how map2 works...
# https://adv-r.hadley.nz/functionals.html#map-variants

DFO_FEAT %>%
  group_by(region) %>%
  count()

# first lets make a dataframe with region as one column
# and number extractions as the other
region <- c(1:8)
region_extr_numbers <- data.frame(region)

# create lists to use in `mutate` in dplyr 
# to create and fill `n_extract` column
large_sample <- c(3, 4,6)
medium_sample <- c(1,2,5)
small_sample <- c(7,8)

region_extr_numbers <- region_extr_numbers %>%
  mutate(n_extract = case_when(region %in% large_sample ~ 200,
                               region %in% medium_sample ~ 150,
                               region %in% small_sample ~ 40))

# can fill in `n_extract` column in other ways too
# n_extract <- c(150, 150, 200, 200, 150, 200,  40,  40)
# region_extr_numbers <- data.frame(region, n_extract)

set.seed(9)
DFO_FEAT_extract_3 <- DFO_FEAT %>%
  nest(-region) %>%
  left_join(region_extr_numbers, by = "region") %>%
  mutate(Sample = map2(data, n_extract, sample_n)) %>%
  unnest(Sample) %>%
  select(-data,-n_extract)


##### a quick peaksie at nested data frame ####
peaksie <- DFO_FEAT %>%
  nest(-region)

peaksie[[1]]
peaksie[[2]]
#########

DFO_FEAT_extract_3 %>%
  group_by(region) %>%
  count()

# cool beans!!
unique(DFO_FEAT_extract_3)


