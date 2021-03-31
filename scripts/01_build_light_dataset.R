library(tidyverse)

## clean search_results

search_results <- read_rds(here::here("proc_data/search_results.rds"))

search_results_light <- search_results

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# unify entry_id and id_entry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# entry_id and id_entry are only missing when the other is missing and never missing at the same time
# we conclude they should be together
missing_entry_id_lgl <- is.na(search_results_light$entry_id)
search_results_light$entry_id[missing_entry_id_lgl] <-
  search_results_light$id_entry[missing_entry_id_lgl]
rm(missing_entry_id_lgl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# drop unused columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <- select(
  search_results_light,
  -id_entry, -id_keyword, # id_entry is not needed anymore
  -id_keyword, # same as keyword_id but character and sometimes missing
  -meta,       # the biggest column, and messy to explore
  -meta_2, -meta_3, # mostly missing
  -breadcrumb, # irrelevant
  -rank_absolute, # essentially same as rank_group, except in ~15% of cases it doesn't start with 1
  -keyword_id
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove source object to save memory for next operation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(search_results)
gc(reset = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# keep only relevant SE, cleanup problematic kws, keep only organic 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <- 
  search_results_light %>%
  filter(
    ## restrict to relevant SE
    search_engine %in% c("Google", "Bing", "DuckDuck", "Yahoo"),
    ## these keywords are formatted wrongly because they have the delim in them
    !keyword %in% c("the rolling stones ", "4th of july ","let's dance "),
    ## remove rows with missing keywords (they were very few)
    !is.na(keyword),
    ## keep only organic results
    type == "organic") %>% 
  select(-type) # remove obsolete column (always "organic")

print(dim(search_results_light)) # [1] 7154169       8

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# join to keywords to get valid keyword_id, drop some more keywords and
# add some info
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

keywords <- read_rds(here::here("proc_data/keywords.rds"))

gc(reset = TRUE)

search_results_light <-inner_join(
  search_results_light, 
  keywords[c("keyword", "keyword_id", "monthly_search_volume_level", "keyword_info_categories")], 
  by = "keyword")

print(dim(search_results_light)) # [1] 7011038      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert columns to numeric or factor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <- 
  search_results_light %>%
  ## convert numeric variables 
  mutate_at(vars(entry_id, rank_group, page_num), as.numeric) %>%
  # sort levels
  mutate(monthly_search_volume_level = fct_relevel(monthly_search_volume_level, "500-1000", "1000-10000", "10000-100000"))

print(dim(search_results_light)) # [1] 7011038      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove keywords/keyword_ids that are not found for all 4 engines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <-
  search_results_light %>%
  group_by(keyword_id) %>%
  mutate(distinct_se_by_kwid = length(unique(search_engine))) %>%
  ungroup() %>%
  filter(distinct_se_by_kwid == 4) %>%
  select(- distinct_se_by_kwid)

print(dim(search_results_light)) # [1] 7010575      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove keywords for which we find a missing URL (they are very few)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <-
  search_results_light %>%
  group_by(keyword_id) %>%
  mutate(wrong_url = anyNA(url)) %>%
  ungroup() %>%
  filter(!wrong_url) %>%
  select(- wrong_url)

print(dim(search_results_light)) # [1] 6976526      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove keywords for which we find a duplicate set of keyword_id, search_engine, rank_group
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# we expect that for a rank_group, a keyword and a search engine we find only one observation,
# it is not the case, we remove all observations featuring a keyword found in such anomaly

search_results_light <-
  search_results_light %>%
  add_count(keyword_id, search_engine, rank_group) %>%
  group_by(keyword_id) %>%
  mutate(ambiguous_set = any(n!=1)) %>%
  ungroup() %>%
  filter(!ambiguous_set) %>%
  select(-n, -ambiguous_set)

print(dim(search_results_light)) # [1] 6520886      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove keywords for which we don't find rank_group from 1 to 30 for
# every search engine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <-
  search_results_light %>%
  group_by(keyword_id, search_engine) %>%
  # number of provided ranks in the top 30 for each keyword SE pair
  mutate(n = sum(1:30 %in% rank_group)) %>%
  ungroup()  %>%
  group_by(keyword_id) %>%
  # an incomplete keyword is one that doesn't provide 30 top 30 result for any SE
  mutate(incomplete = any(n!=30)) %>%
  ungroup() %>%
  # keep only complete
  filter(!incomplete) %>%
  select(-n, -incomplete) %>% 
  # remove unused results
  filter(rank_group <= 30)

print(dim(search_results_light)) # [1] 694680     11 
# previously # [1] 6324619      11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove keywords for which we don't find all 4 search engines
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

search_results_light <-
  search_results_light %>%
  group_by(keyword_id) %>%
  mutate(n = length(unique(search_engine))) %>%
  ungroup()

# table(search_results_light$n)
#>    1       2       3       4 
#> 2357   10289  146732 6354282 

search_results_light <-
  search_results_light %>%
  filter(n == 4) %>%
  select(-n)

print(dim(search_results_light)) # [1] 694680     11 
# previously # [1] 6324619      11

write_rds(search_results_light, here::here("proc_data/search_results_light.rds"))