## Fisher Kim, Kai Ying, Lucas Schroyer, Peter YH Kim
# Recode pre and post beliefs and excitement levels

interest <- tibble(
  #interest
  aliens_interest = 
    c("Very uninterested", "Somewhat uninterested", "Somewhat interested", "Very interested"),
  recycling_interest = 
    c("Very uninterested", "Somewhat uninterested", "Somewhat interested", "Very interested"),
  social_interest = 
    c("Very uninterested", "Somewhat uninterested", "Somewhat interested", "Very interested")) %>% 
  gather(key = "interest_question", value = "interest_answer") %>% 
  
  bind_cols(tibble(
    aliens_interest_ohe =   c(0, 1, 2, 3),
    recycling_interest_ohe = c(0, 1, 2, 3),
    social_interest_ohe = c(0, 1, 2, 3)) %>%
      gather(key = "interest_question_ohe", value = "interest_answer_ohe")
  ) %>% 
  
  mutate(question_topic = case_when(
    interest_question_ohe %like% "alien" ~ "alien",
    interest_question_ohe %like% "recyc" ~ "recycle",
    interest_question_ohe %like% "social" ~ "social",
    TRUE ~ "other")
  )

# Final Join
dat_interest <- dat_knowledge_interest %>% 
  gather(key = "interest_question", value = "interest_answer", 
         c(`alien_interest`, `recycling_interest`, `social_media_interest`)
  ) %>% 
  mutate(interest_question = str_remove(interest_question, "_interest"),
         interest_question = ifelse(interest_question %like% "recyc", "recycle", interest_question),
         interest_question = ifelse(interest_question %like% "social_media", "social", interest_question)) %>% 
  filter(question_topic == interest_question) %>% 
  
  # mutate(belief_question_post = paste0())
  select(-interest_question) %>% 
  left_join(interest, by = c("interest_answer", "question_topic"))

# QC
dat_interest %>%
  filter(response_id == "R_0JoNfdq3IWhf0LT") %>% glimpse()

# QC to make sure there are only 3 topics
stopifnot(nrow(dat_interest %>% count(response_id) %>% filter(n != 3)) == 0)
# each participant has 3 responses

#QC no participants were dropped
stopifnot(length(unique(dat_interest$response_id)) == length(unique(d_final_pre_regressions$response_id)))
# None were dropped




