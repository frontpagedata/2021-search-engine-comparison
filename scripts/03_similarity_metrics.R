#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute ranking similarity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# we check how much of the top 10 for each search engine is in google's top 10

compute_similarity_by_kw_and_se <- 
  function(data, ref = "Google", max_rank) {
    data %>%
      filter(rank_group <= max_rank) %>%
      group_by(keyword) %>%
      mutate(ref_urls = list(url[search_engine == ref])) %>%
      ungroup() %>%
      filter(search_engine != ref) %>%
      left_join(domain_specificity_by_kw, by = "keyword_id") %>%
      group_by(keyword, 
               search_engine, 
               # below all are unique by keyword_id
               monthly_search_volume_level,  
               keyword_info_categories,
               keyword_length = case_when(
                 str_count(str_trim(keyword, "both"), " ") +1 >= 5 ~ "5+",
                 TRUE ~ as.character(str_count(str_trim(keyword, "both"), " ") + 1)
               ),
               domain_specificity_grouped    # unique by keyword_id
      ) %>%
      summarize(
        similarity = mean(url %in% ref_urls[[1]]),.groups = "drop")
  }

compute_similarity_by_se <- function(data, ref = "Google") {
  data %>%
    group_by(search_engine) %>%
    summarize(similarity = mean(similarity), .groups = "drop") %>%
    mutate(search_engine2  = ref)
}

# similarity to google

similarity_to_google_by_kw_and_se_3 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Google", max_rank = 3)

similarity_to_google_by_se_3 <- 
  compute_similarity_by_se(similarity_to_google_by_kw_and_se_3, "Google")

similarity_to_google_by_kw_and_se_10 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Google", max_rank = 10)

similarity_to_google_by_se_10 <- 
  compute_similarity_by_se(similarity_to_google_by_kw_and_se_10, "Google")

similarity_to_google_by_kw_and_se_20 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Google", max_rank = 20)

similarity_to_google_by_se_20 <- 
  compute_similarity_by_se(similarity_to_google_by_kw_and_se_20, "Google")

similarity_to_google_by_kw_and_se_30 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Google", max_rank = 30)

similarity_to_google_by_se_30 <- 
  compute_similarity_by_se(similarity_to_google_by_kw_and_se_30, "Google")

# similarity to duckduck

similarity_to_duckduck_by_kw_and_se_3 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "DuckDuck", max_rank = 3)

similarity_to_duckduck_by_se_3 <- 
  compute_similarity_by_se(similarity_to_duckduck_by_kw_and_se_3, "DuckDuck")

similarity_to_duckduck_by_kw_and_se_10 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "DuckDuck", max_rank = 10)

similarity_to_duckduck_by_se_10 <- 
  compute_similarity_by_se(similarity_to_duckduck_by_kw_and_se_10, "DuckDuck")

similarity_to_duckduck_by_kw_and_se_20 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "DuckDuck", max_rank = 20)

similarity_to_duckduck_by_se_20 <- 
  compute_similarity_by_se(similarity_to_duckduck_by_kw_and_se_20, "DuckDuck")

similarity_to_duckduck_by_kw_and_se_30 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "DuckDuck", max_rank = 30)

similarity_to_duckduck_by_se_30 <- 
  compute_similarity_by_se(similarity_to_duckduck_by_kw_and_se_30, "DuckDuck")

# similarity to bing

similarity_to_bing_by_kw_and_se_3 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Bing", max_rank = 3)

similarity_to_bing_by_se_3 <- 
  compute_similarity_by_se(similarity_to_bing_by_kw_and_se_3, "Bing")

similarity_to_bing_by_kw_and_se_10 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Bing", max_rank = 10)

similarity_to_bing_by_se_10 <- 
  compute_similarity_by_se(similarity_to_bing_by_kw_and_se_10, "Bing")

similarity_to_bing_by_kw_and_se_20 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Bing", max_rank = 20)

similarity_to_bing_by_se_20 <- 
  compute_similarity_by_se(similarity_to_bing_by_kw_and_se_20, "Bing")

similarity_to_bing_by_kw_and_se_30 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Bing", max_rank = 30)

similarity_to_bing_by_se_30 <- 
  compute_similarity_by_se(similarity_to_bing_by_kw_and_se_30, "Bing")

# similarity to yahoo

similarity_to_yahoo_by_kw_and_se_3 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Yahoo", max_rank = 3)

similarity_to_yahoo_by_se_3 <- 
  compute_similarity_by_se(similarity_to_yahoo_by_kw_and_se_3, "Yahoo")

similarity_to_yahoo_by_kw_and_se_10 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Yahoo", max_rank = 10)

similarity_to_yahoo_by_se_10 <- 
  compute_similarity_by_se(similarity_to_yahoo_by_kw_and_se_10, "Yahoo")

similarity_to_yahoo_by_kw_and_se_20 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Yahoo", max_rank = 20)

similarity_to_yahoo_by_se_20 <- 
  compute_similarity_by_se(similarity_to_yahoo_by_kw_and_se_20, "Yahoo")

similarity_to_yahoo_by_kw_and_se_30 <- 
  compute_similarity_by_kw_and_se(search_results_light, ref = "Yahoo", max_rank = 30)

similarity_to_yahoo_by_se_30 <- 
  compute_similarity_by_se(similarity_to_yahoo_by_kw_and_se_30, "Yahoo")

# bind for summary cross SE chart

similarity_by_se_10 <- bind_rows(
  similarity_to_google_by_se_10,
  similarity_to_duckduck_by_se_10  %>% filter(!search_engine %in% "Google"),
  similarity_to_bing_by_se_10      %>% filter(!search_engine %in% c("Google", "DuckDuck"))
) %>%
  mutate(descr = paste(search_engine, "and", search_engine2),
         descr = fct_reorder(descr, search_engine2 != "Google"))


similarity_by_se_20 <- bind_rows(
  similarity_to_google_by_se_20,
  similarity_to_duckduck_by_se_20 %>% filter(!search_engine %in% "Google"),
  similarity_to_bing_by_se_20     %>% filter(!search_engine %in% c("Google", "DuckDuck"))
) %>%
  mutate(descr = paste(search_engine, "and", search_engine2),
         descr = fct_reorder(descr, search_engine2 != "Google"))

similarity_by_se_30 <- bind_rows(
  similarity_to_google_by_se_30,
  similarity_to_duckduck_by_se_30 %>% filter(!search_engine %in% "Google"),
  similarity_to_bing_by_se_30     %>% filter(!search_engine %in% c("Google", "DuckDuck"))
) %>%
  mutate(descr = paste(search_engine, "and", search_engine2),
         descr = fct_reorder(descr, search_engine2 != "Google"))


similarity_by_se_3 <- bind_rows(
  similarity_to_google_by_se_3,
  similarity_to_duckduck_by_se_3 %>% filter(!search_engine %in% "Google"),
  similarity_to_bing_by_se_3     %>% filter(!search_engine %in% c("Google", "DuckDuck"))
) %>%
  mutate(descr = paste(search_engine, "and", search_engine2),
         descr = fct_reorder(descr, search_engine2 != "Google"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# similarity by search engine and volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

similarity_to_google_by_se_and_vol_10 <-
  similarity_to_google_by_kw_and_se_10 %>%
  group_by(search_engine, monthly_search_volume_level) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>% 
  mutate(grp = "top 10")

similarity_to_google_by_se_and_vol_20 <-
  similarity_to_google_by_kw_and_se_20 %>%
  group_by(search_engine, monthly_search_volume_level) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>% 
  mutate(grp = "top 20")

similarity_to_google_by_se_and_vol_30 <-
  similarity_to_google_by_kw_and_se_30 %>%
  group_by(search_engine, monthly_search_volume_level) %>%
  summarize(similarity = mean(similarity), .groups = "drop") %>% 
  mutate(grp = "top 30")

similarity_by_se_and_vol <-
  bind_rows(
    similarity_to_google_by_se_and_vol_10, 
    similarity_to_google_by_se_and_vol_20, 
    similarity_to_google_by_se_and_vol_30)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute similarity by search and category 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

similarity_by_se_and_category_0 <-
  similarity_to_google_by_kw_and_se_10 %>%
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

similarity_by_se_and_category <-
  similarity_by_se_and_category_0  %>% 
  group_by(search_engine, keyword_category) %>% 
  summarize(
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, similarity)) %>%
  arrange(desc(n))


similarity_by_se_and_category_2 <-
  similarity_to_google_by_kw_and_se_10 %>%
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
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, similarity)) %>%
  arrange(desc(n)) %>%
  head(30)

similarity_by_se_and_category_3 <-
  similarity_to_google_by_kw_and_se_10 %>%
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
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, similarity)) %>%
  arrange(desc(n)) %>%
  head(30)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute similarity by search engine, category 1 and volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

similarity_by_se_category_and_vol <-
  similarity_by_se_and_category_0  %>% 
  group_by(search_engine, keyword_category, monthly_search_volume_level) %>% 
  summarize(
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  mutate(keyword_category = fct_reorder(keyword_category, similarity)) %>%
  arrange(desc(similarity))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute similarity by search engine and keyword length
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

similarity_by_se_and_kw_length <-
  similarity_to_google_by_kw_and_se_10 %>% 
  group_by(search_engine, keyword_length) %>% 
  summarize(
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(similarity))  

similarity_by_se_vol_and_kw_length <-
  similarity_by_se_and_category_0  %>% 
  group_by(search_engine, monthly_search_volume_level, keyword_length) %>% 
  summarize(
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  # mutate(monthly_search_volume_level = monthly_search_volume_level)) %>%
  arrange(desc(similarity))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compute similarity by search engine and domain specificity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

similarity_by_se_and_spec <-
  similarity_to_google_by_kw_and_se_10 %>% 
  group_by(search_engine, domain_specificity_grouped) %>% 
  summarize(
    similarity = mean(similarity), 
    n = n(),
    .groups = "drop")  %>% 
  arrange(desc(similarity))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saving all workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(
  file = here::here("proc_data/datasets_for_report.Rdata"),
  monthly_search_volume,
  top_domains,
  top_domains_10,
  accuracy_by_se,
  accuracy_by_se_and_vol,
  category1_counts,
  category2_counts,
  category3_counts,
  accuracy_by_se_and_category,
  real_estate_examples_small_volume,
  real_estate_examples_medium_volume,
  accuracy_by_se_and_category_2,
  accuracy_by_se_and_category_3,
  accuracy_by_se_category_and_vol,
  kw_length_counts,
  accuracy_by_se_and_kw_length,
  accuracy_by_se_vol_and_kw_length ,
  short_kw_examples,
  long_kw_examples,
  accuracy_by_se_and_spec,
  similarity,
  similarity_3,
  domain_specificity_by_kw,
  google1_positions,
  median_google1_positions,
  accuracy_by_se_and_domain_ext,
  similarity_by_se_3,
  similarity_by_se_10,
  similarity_by_se_20,
  similarity_by_se_30,
  similarity_by_se_and_vol,
  similarity_by_se_and_category,
  similarity_by_se_and_category_2,
  similarity_by_se_and_category_3,
  similarity_by_se_category_and_vol,
  similarity_by_se_and_kw_length,
  similarity_by_se_vol_and_kw_length,
  similarity_by_se_and_spec
)
