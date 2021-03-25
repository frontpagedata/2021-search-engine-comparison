
product_services <- read_rds(here::here("proc_data/dim_category.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# monthly search volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_search_volume <-
  keywords %>% 
  mutate(monthly_search_volume_level = fct_relevel(
    monthly_search_volume_level,
    "500-1000",
    "1000-10000",
    "10000-100000")) %>% 
  count(monthly_search_volume_level) %>% 
  rename(Count = n) %>% 
  mutate(Count = scales::comma(Count))

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
# compute accuracy by search engine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# reusable first step, at keyword x search_engine level
accuracy_0 <- 
  search_results_light %>%
  filter(rank_group <= 10) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
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
            .groups = "drop")

accuracy_by_se_10 <-
  accuracy_0 %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_10 = mean(found), 
    n_keywords = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_10))

accuracy_by_se_20 <- 
  search_results_light %>%
  filter(rank_group <= 20) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
  ungroup() %>%
  filter(search_engine != "Google") %>%
  group_by(keyword_id, search_engine) %>%
  summarize(found = any(url == google_url_1),
            .groups = "drop") %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_20 = mean(found), 
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_20))

accuracy_by_se_30 <- 
  search_results_light %>%
  filter(rank_group <= 30) %>%
  group_by(keyword_id) %>%
  mutate(google_url_1 = url[search_engine == "Google" & rank_group == 1]) %>%
  ungroup() %>%
  filter(search_engine != "Google") %>%
  group_by(keyword_id, search_engine) %>%
  summarize(found = any(url == google_url_1),
            .groups = "drop") %>% 
  group_by(search_engine) %>% 
  summarize(
    accuracy_top_30 = mean(found), 
    .groups = "drop")  %>% 
  arrange(desc(accuracy_top_30))

accuracy_by_se <-
  accuracy_by_se_10 %>%
  inner_join(accuracy_by_se_20, by = "search_engine") %>%
  inner_join(accuracy_by_se_30, by = "search_engine") %>%
  select(search_engine, accuracy_top_10, accuracy_top_20, accuracy_top_30, n_keywords)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy_top_30 by search engine and volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_and_vol <-
  accuracy_0 %>% 
  group_by(search_engine, monthly_search_volume_level) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# category counts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

keywords_for_category_counts <-
  keywords %>%
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
# compute accuracy by search category 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

product_services_for_join_1 <- 
  product_services %>% 
  filter(is.na(category_level_2)) %>%
  select(criterion_id, keyword_category = category_level_1)

accuracy_by_se_and_category_0 <-
  accuracy_0 %>%
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
# compute accuracy by search category 2 and 3
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
  accuracy_0 %>%
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
  accuracy_0 %>%
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
  mutate(keyword_length = nchar(keyword),
         keyword_length = str_count(keyword, " ") + 1) %>% 
  # filter(keyword_length == 1)
  count(keyword_length)

accuracy_by_se_and_kw_length <-
  accuracy_0 %>% 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute accuracy by search engine and domain specificity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

accuracy_by_se_and_spec <-
  accuracy_0 %>% 
  group_by(search_engine, domain_specificity_grouped) %>% 
  summarize(
    accuracy = mean(found), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(accuracy))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute ranking similarity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# we check how much of the top 10 for each search engine is in google's top 10

similarity_to_google <-
    search_results_light %>%
      filter(rank_group <= 10) %>%
      group_by(keyword) %>%
      mutate(google_urls = list(url[search_engine == "Google"])) %>%
      ungroup() %>%
      group_by(keyword, search_engine) %>%
      summarize(similarity = mean(url %in% google_urls[[1]]), .groups = "drop") %>%
      group_by(search_engine) %>%
      summarize(similarity = mean(similarity), .groups = "drop") %>%
      mutate(search_engine2  = "Google")

similarity_to_duckduck <-
  search_results_light %>%
  filter(rank_group <= 10) %>%
  group_by(keyword) %>%
  mutate(google_urls = list(url[search_engine == "DuckDuck"])) %>%
  ungroup() %>%
  group_by(keyword, search_engine) %>%
  summarize(similarity = mean(url %in% google_urls[[1]]), .groups = "drop") %>%
  group_by(search_engine) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>%
  mutate(search_engine2  = "DuckDuck")

similarity_to_bing <-
  search_results_light %>%
  filter(rank_group <= 10) %>%
  group_by(keyword) %>%
  mutate(google_urls = list(url[search_engine == "Bing"])) %>%
  ungroup() %>%
  group_by(keyword, search_engine) %>%
  summarize(similarity = mean(url %in% google_urls[[1]]), .groups = "drop") %>%
  group_by(search_engine) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>%
  mutate(search_engine2  = "Bing")

similarity_to_yahoo <-
  search_results_light %>%
  filter(rank_group <= 10) %>%
  group_by(keyword) %>%
  mutate(google_urls = list(url[search_engine == "Yahoo"])) %>%
  ungroup() %>%
  group_by(keyword, search_engine) %>%
  summarize(similarity = mean(url %in% google_urls[[1]]), .groups = "drop") %>%
  group_by(search_engine) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>%
  mutate(search_engine2  = "Yahoo")

similarity <- bind_rows(
  similarity_to_google,
  similarity_to_duckduck,
  similarity_to_bing,
  similarity_to_yahoo
) %>%
  spread(search_engine2, similarity)

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
# saving all workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(
  file = here::here("proc_data/datasets_for_report.Rdata"),
  monthly_search_volume,
  accuracy_by_se,
  accuracy_by_se_and_vol,
  category1_counts,
  category2_counts,
  category3_counts,
  accuracy_by_se_and_category,
  accuracy_by_se_and_category_2,
  accuracy_by_se_and_category_3,
  accuracy_by_se_category_and_vol,
  kw_length_counts,
  accuracy_by_se_and_kw_length,
  accuracy_by_se_vol_and_kw_length ,
  accuracy_by_se_and_spec,
  similarity,
  domain_specificity_by_kw,
  google1_positions,
  median_google1_positions
)
