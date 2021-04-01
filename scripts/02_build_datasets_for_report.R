# search_results_light <- readRDS(here::here("proc_data/search_results_light.rds"))
# keywords <- readRDS(here::here("proc_data/keywords.rds"))

product_services <- read_rds(here::here("proc_data/dim_category.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# domain specificity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the domain specificity of a keyword is the number top 10 Google results 
# that feature dominant domain of this top 10

# we need to compute it early because it will be used in the next section

domain_specificity_by_kw <- 
  search_results_light %>%
  filter(search_engine == "Google",
         rank_group <= 10) %>%
  group_by(keyword_id) %>%
  summarize(domain_specificity = max(tabulate(factor(domain))), .groups = "drop") %>%
  mutate(domain_specificity_grouped = case_when(
    domain_specificity <= 4 ~ as.character(domain_specificity),
    TRUE ~ "5+"
  ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# monthly search volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_search_volume <-
  keywords %>% 
  filter(keyword %in% unique(search_results_light$keyword)) %>%
  mutate(monthly_search_volume_level = fct_relevel(
    monthly_search_volume_level,
    "500-1000",
    "1000-10000",
    "10000-100000")) %>% 
  count(monthly_search_volume_level) %>% 
  rename(Count = n) %>% 
  mutate(Count = scales::comma(Count))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# top domains
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

top_domains_0 <-
  search_results_light %>%
  filter(rank_group == 1) %>%
  count(search_engine, domain,sort = TRUE) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

top_google_domains <-
  top_domains_0 %>%
  filter(search_engine == "Google") %>%
  arrange(-n) %>%
  head(10) %>%
  pull(domain)

top_domains <-
  top_domains_0 %>%
  filter(domain %in% top_google_domains) %>%
  mutate(domain = factor(domain, top_google_domains),
         search_engine = factor(search_engine, c("Google", "DuckDuck","Bing", "Yahoo")))

top_domains_10_0 <-
  search_results_light %>%
  filter(rank_group <= 10) %>%
  count(search_engine, domain,sort = TRUE) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

top_google_domains_10 <-
  top_domains_10_0 %>%
  filter(search_engine == "Google") %>%
  arrange(-n) %>%
  head(10) %>%
  pull(domain)

top_domains_10 <-
  top_domains_10_0 %>%
  filter(domain %in% top_google_domains_10) %>%
  mutate(domain = factor(domain, top_google_domains_10),
         search_engine = factor(search_engine, c("Google", "DuckDuck","Bing", "Yahoo")))

# 3

top_domains_3_0 <-
  search_results_light %>%
  filter(rank_group <= 3) %>%
  count(search_engine, domain,sort = TRUE) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

top_google_domains_3 <-
  top_domains_3_0 %>%
  filter(search_engine == "Google") %>%
  arrange(-n) %>%
  head(10) %>%
  pull(domain)

top_domains_3 <-
  top_domains_3_0 %>%
  filter(domain %in% top_google_domains_3) %>%
  mutate(domain = factor(domain, top_google_domains_3),
         search_engine = factor(search_engine, c("Google", "DuckDuck","Bing", "Yahoo")))

# 20

top_domains_20_0 <-
  search_results_light %>%
  filter(rank_group <= 20) %>%
  count(search_engine, domain,sort = TRUE) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

top_google_domains_20 <-
  top_domains_20_0 %>%
  filter(search_engine == "Google") %>%
  arrange(-n) %>%
  head(10) %>%
  pull(domain)

top_domains_20 <-
  top_domains_20_0 %>%
  filter(domain %in% top_google_domains_20) %>%
  mutate(domain = factor(domain, top_google_domains_20),
         search_engine = factor(search_engine, c("Google", "DuckDuck","Bing", "Yahoo")))

# 30

top_domains_30_0 <-
  search_results_light %>%
  filter(rank_group <= 30) %>%
  count(search_engine, domain,sort = TRUE) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

top_google_domains_30 <-
  top_domains_30_0 %>%
  filter(search_engine == "Google") %>%
  arrange(-n) %>%
  head(10) %>%
  pull(domain)

top_domains_30 <-
  top_domains_30_0 %>%
  filter(domain %in% top_google_domains_30) %>%
  mutate(domain = factor(domain, top_google_domains_30),
         search_engine = factor(search_engine, c("Google", "DuckDuck","Bing", "Yahoo")))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# reusable first step, at keyword x search_engine level
accuracy_10_0 <- 
  search_results_light %>%
  filter(rank_group <= 10) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1],
         google_domain_1 = domain[search_engine == "Google" & rank_group == 1]
         ) %>%
  ungroup() %>%
  filter(search_engine != "Google") %>%
  left_join(domain_specificity_by_kw, by = "keyword_id") %>%
  group_by(keyword_id, 
           search_engine, 
           monthly_search_volume_level,  # unique by keyword_id
           keyword_info_categories,      # unique by keyword_id
           keyword_length = case_when(   # unique by keyword_id
             str_count(str_trim(keyword, "both"), " ") +1 >= 5 ~ "5+",
             TRUE ~ as.character(str_count(str_trim(keyword, "both"), " ") + 1)
           ),
           domain_specificity_grouped    # unique by keyword_id
           ) %>%
  summarize(found = any(url == google_url_1),
            domain = google_domain_1[1],
            .groups = "drop")

accuracy_20_0 <- 
  search_results_light %>%
  filter(rank_group <= 20) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
  ungroup() %>%
  filter(search_engine != "Google") %>%
  group_by(keyword_id, search_engine, monthly_search_volume_level) %>%
  summarize(found = any(url == google_url_1),
            .groups = "drop")

accuracy_30_0 <- 
  search_results_light %>%
  filter(rank_group <= 30) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
  ungroup() %>%
  filter(search_engine != "Google") %>%
  group_by(keyword_id, search_engine, monthly_search_volume_level) %>%
  summarize(found = any(url == google_url_1),
            .groups = "drop")

# accuracy at search engine level

accuracy_by_se_10 <-
  accuracy_10_0 %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_10 = mean(found), 
    n_keywords = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_10))

accuracy_by_se_20 <- 
  accuracy_20_0 %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_20 = mean(found), 
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_20))

accuracy_by_se_30 <- 
  accuracy_30_0 %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_30 = mean(found), 
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_30))

# combine all 3 in single table

accuracy_by_se <-
  accuracy_by_se_10 %>%
  inner_join(accuracy_by_se_20, by = "search_engine") %>%
  inner_join(accuracy_by_se_30, by = "search_engine") %>%
  select(search_engine, accuracy_top_10, accuracy_top_20, accuracy_top_30, n_keywords)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine and volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_and_vol_10 <-
  accuracy_10_0 %>% 
  group_by(search_engine, monthly_search_volume_level) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(grp = "top 10")

accuracy_by_se_and_vol_20 <-
  accuracy_20_0 %>% 
  group_by(search_engine, monthly_search_volume_level) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")   %>% 
  mutate(grp = "top 20")

accuracy_by_se_and_vol_30 <-
  accuracy_30_0 %>% 
  group_by(search_engine, monthly_search_volume_level) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(grp = "top 30")

accuracy_by_se_and_vol <-
  bind_rows(
    accuracy_by_se_and_vol_10, 
    accuracy_by_se_and_vol_20, 
    accuracy_by_se_and_vol_30)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# category counts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

keywords_for_category_counts <-
  keywords %>%
  filter(keyword %in% unique(search_results_light$keyword)) %>%
  mutate(keyword_info_categories = str_remove_all(keyword_info_categories,
                                                  "\\[") %>% 
           str_remove_all("\\]")) %>% 
  separate_rows(keyword_info_categories, sep = ",") %>% 
  mutate(keyword_info_categories = str_trim(keyword_info_categories, "both"))

category1_counts <-
  keywords_for_category_counts %>% 
  inner_join(
    product_services %>% 
      filter(is.na(category_level_2)) %>% 
      select(criterion_id, keyword_category = category_level_1),
    by = c("keyword_info_categories" = "criterion_id")
  ) %>% 
  count(keyword_category) %>%
  mutate(keyword_category = fct_reorder(keyword_category, n))

category2_counts <-
  keywords_for_category_counts %>% 
  inner_join(
    product_services %>% 
      filter(is.na(category_level_3), !is.na(category_level_2)) %>% 
      select(criterion_id, keyword_category = category_level_2),
    by = c("keyword_info_categories" = "criterion_id")
  ) %>% 
  count(keyword_category) %>%
  top_n(20, n) %>%
  mutate(keyword_category = fct_reorder(keyword_category, n))

category3_counts <-
  keywords_for_category_counts %>% 
  inner_join(
    product_services %>% 
      filter(is.na(category_level_4), !is.na(category_level_3)) %>% 
      select(criterion_id, keyword_category = category_level_3),
    by = c("keyword_info_categories" = "criterion_id")
  ) %>% 
  count(keyword_category) %>%
  top_n(20, n) %>%
  mutate(keyword_category = fct_reorder(keyword_category, n))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search and category 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

product_services_for_join_1 <- 
  product_services %>% 
  filter(is.na(category_level_2)) %>%
  select(criterion_id, keyword_category = category_level_1)

accuracy_by_se_and_category_0 <-
  accuracy_10_0 %>%
  mutate(
    keyword_info_categories = str_remove_all(keyword_info_categories, "\\[|\\]"),
  ) %>%
  separate_rows(keyword_info_categories, sep = ",") %>%
  distinct() %>%
  mutate(keyword_info_categories = str_trim(keyword_info_categories, "both")) %>%
  inner_join(
    product_services_for_join_1, 
    by = c("keyword_info_categories" = "criterion_id")
  )

set.seed(1)
real_estate_examples_small_volume <-
  search_results_light %>%
  # keep only rank 1
  filter(rank_group == 1, monthly_search_volume_level == "500-1000") %>%
  group_by(keyword_id) %>%
  mutate(keep = length(unique(url)) == 1) %>%
  ungroup() %>%
  filter(keep)  %>%
  separate_rows(keyword_info_categories, sep = ",") %>%
  inner_join(
    product_services_for_join_1, 
    by = c("keyword_info_categories" = "criterion_id")
  ) %>%
  filter(keyword_category == "Real Estate") %>%
  distinct(keyword, domain, url) %>%
  slice_sample(n = 5)

real_estate_examples_medium_volume <-
  search_results_light %>%
  # keep only rank 1
  filter(rank_group == 1, monthly_search_volume_level == "1000-10000") %>%
  group_by(keyword_id) %>%
  mutate(keep = length(unique(url)) == 1) %>%
  ungroup() %>%
  filter(keep)  %>%
  separate_rows(keyword_info_categories, sep = ",") %>%
  inner_join(
    product_services_for_join_1, 
    by = c("keyword_info_categories" = "criterion_id")
  ) %>%
  filter(keyword_category == "Real Estate") %>%
  distinct(keyword, domain, url) %>%
  slice_sample(n = 5)
  
accuracy_by_se_and_category <-
  accuracy_by_se_and_category_0  %>% 
  group_by(search_engine, keyword_category) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, accuracy)) %>%
  arrange(desc(n))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search and category 2 and 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

product_services_for_join_2 <- 
  product_services %>% 
  filter(is.na(category_level_3), !is.na(category_level_2)) %>%
  select(criterion_id, keyword_category = category_level_2)

product_services_for_join_3 <- 
  product_services %>% 
  filter(is.na(category_level_4), !is.na(category_level_3)) %>%
  select(criterion_id, keyword_category = category_level_3)

accuracy_by_se_and_category_2 <-
  accuracy_10_0 %>%
  mutate(
    keyword_info_categories = str_remove_all(keyword_info_categories, "\\[|\\]"),
  ) %>%
  separate_rows(keyword_info_categories, sep = ",") %>%
  distinct() %>%
  mutate(keyword_info_categories = str_trim(keyword_info_categories, "both")) %>%
  inner_join(
    product_services_for_join_2, 
    by = c("keyword_info_categories" = "criterion_id")
  )  %>% 
  group_by(search_engine, keyword_category) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, accuracy)) %>%
  arrange(desc(n)) %>%
  head(30)

accuracy_by_se_and_category_3 <-
  accuracy_10_0 %>%
  mutate(
    keyword_info_categories = str_remove_all(keyword_info_categories, "\\[|\\]"),
  ) %>%
  separate_rows(keyword_info_categories, sep = ",") %>%
  distinct() %>%
  mutate(keyword_info_categories = str_trim(keyword_info_categories, "both")) %>%
  inner_join(
    product_services_for_join_3, 
    by = c("keyword_info_categories" = "criterion_id")
  )  %>% 
  group_by(search_engine, keyword_category) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, accuracy)) %>%
  arrange(desc(n)) %>%
  head(30)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine, category 1 and volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_category_and_vol <-
  accuracy_by_se_and_category_0  %>% 
  group_by(search_engine, keyword_category, monthly_search_volume_level) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, accuracy)) %>%
  arrange(desc(accuracy))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine and keyword length
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
kw_length_counts <- 
  keywords %>% 
  filter(keyword %in% unique(search_results_light$keyword)) %>%
  mutate(keyword_length = str_count(keyword, " ") + 1) %>% 
  count(keyword_length)

accuracy_by_se_and_kw_length <-
  accuracy_10_0 %>% 
  group_by(search_engine, keyword_length) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy))  

accuracy_by_se_vol_and_kw_length <-
  accuracy_by_se_and_category_0  %>% 
  group_by(search_engine, monthly_search_volume_level, keyword_length) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
 # mutate(monthly_search_volume_level = monthly_search_volume_level)) %>%
  arrange(desc(accuracy))

set.seed(1)

kw_examples_0 <-
  search_results_light %>%
  # keep only rank 1
  filter(rank_group == 1) %>%
  group_by(keyword_id) %>%
  mutate(keep = length(unique(url)) == 1) %>%
  ungroup() %>%
  filter(keep)  %>%
  mutate(keyword_length = str_count(keyword, " ") + 1)

short_kw_examples <-
  kw_examples_0 %>%
  filter(keyword_length == 1) %>%
  distinct(keyword, domain, url) %>%
  slice_sample(n = 5)

long_kw_examples <-
  kw_examples_0 %>%
  filter(keyword_length >= 5) %>%
  distinct(keyword, domain, url) %>%
  slice_sample(n = 5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine and domain specificity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_and_spec <-
  accuracy_10_0 %>% 
  group_by(search_engine, domain_specificity_grouped) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy))
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute position of 1st google url
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

google1_positions_0 <- 
  search_results_light %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
  ungroup() %>%
  group_by(keyword_id, search_engine) %>%
  summarize(google1_position = rank_group[url == google_url_1][1], .groups = "drop")

google1_positions <-
  google1_positions_0 %>%
  filter(search_engine != "Google") %>%
  count(search_engine, google1_position) %>%
  group_by(search_engine) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

median_google1_positions <-
  google1_positions_0 %>%
  replace_na(list(google1_position = Inf)) %>%
  group_by(search_engine) %>%
  summarise(median = median(google1_position), .groups = "drop")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# domain extension
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_and_domain_ext <-
  accuracy_10_0 %>% 
  mutate(ext = gsub("^.*(\\.[^.]+)$", "\\1", domain)) %>%
  group_by(search_engine, ext) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop") %>%
  arrange(desc(n)) %>%
  head(30) %>%
  mutate(ext = fct_reorder(ext, -accuracy)) 
