## Fisher Kim, Kai Ying, Lucas Schroyer, Peter YH Kim
# Recode pre and post beliefs and excitement levels

knowledge <- tibble(

  aliens_knowledge = 
    c("Very unknowledgeable", "Somewhat unknowledgeable", "Somewhat knowledgeable", "Very knowledgeable"),
  recycling_knowledge = 
    c("Very unknowledgeable", "Somewhat unknowledgeable", "Somewhat knowledgeable", "Very knowledgeable"),
  social_knowledg = 
    c("Very unknowledgeable", "Somewhat unknowledgeable", "Somewhat knowledgeable", "Very knowledgeable")) %>% 
  
  gather(key = "knowledge_question", value = "knowledge_answer") %>% 
  
  bind_cols(tibble(
    aliens_knowledge_ohe =   c(0, 1, 2, 3),
    recycling_knowledge_ohe = c(0, 1, 2, 3),
    social_knowledg_ohe = c(0, 1, 2, 3)) %>%
      gather(key = "knowledge_question_ohe", value = "knowledge_answer_ohe")
  ) %>% 
  
  #select(knowledge_question, knowledge_answer, knowledge_answer_ohe) %>% 
  
  mutate(question_topic = case_when(
    knowledge_question_ohe %like% "alien" ~ "alien",
    knowledge_question_ohe %like% "recyc" ~ "recycle",
    knowledge_question_ohe %like% "social" ~ "social",
    TRUE ~ "other")
  )


dat_knowledge <- dat_knowledge_interest %>% 
  gather(key = "knowledge_question", value = "knowledge_answer", 
         c(`alien_knowledge`, `recycling_knowledge`, `social_media_knowledg`)
  ) %>% 
  mutate(knowledge_question = str_remove(knowledge_question, "_knowledge"),
         #knowledge_question = str_remove(knowledge_question, "_knowledg"),
         knowledge_question = ifelse(knowledge_question %like% "social_media", "social", knowledge_question),
         knowledge_question = ifelse(knowledge_question %like% "recyc", "recycle", knowledge_question)) %>% 
  filter(question_topic == knowledge_question) %>% 
  
  # mutate(belief_question_post = paste0())
  select(-knowledge_question) %>% 
  left_join(knowledge, by = c("knowledge_answer", "question_topic"))

# QC
dat_knowledge %>%
  filter(response_id == "R_0JoNfdq3IWhf0LT") %>% glimpse()

# QC to make sure there are only 3 topics
stopifnot(nrow(dat_knowledge %>% count(response_id) %>% filter(n != 3)) == 0)
# each participant has 3 responses

#QC no participants were dropped
stopifnot(length(unique(dat_knowledge$response_id)) == length(unique(d_final_pre_regressions$response_id)))
# None were dropped

