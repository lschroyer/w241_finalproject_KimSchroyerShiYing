## Fisher Kim, Kai Ying, Lucas Schroyer, Peter YH Kim
# Recode the time related variable to see it's impact on quiz score

dat_time_to_complete %>% 
  filter(response_id == "R_0JoNfdq3IWhf0LT") %>%
  glimpse()

dat_time_to_complete_2 <- dat_time_to_complete %>% 
  mutate(
    time_question_page_submit = case_when(
      question_topic == "alien" & treatment_bool == 1 ~ time_treat_alien_page_submit,
      question_topic == "alien" & treatment_bool == 0 ~ time_control_alien_page_submit,
      question_topic == "recycle" & treatment_bool == 1 ~ time_recycle_treat_page_submit,
      question_topic == "recycle" & treatment_bool == 0 ~ time_recycle_control_page_submit,
      question_topic == "social" & treatment_bool == 1 ~ time_treat_social_page_submit,
      question_topic == "social" & treatment_bool == 0 ~ time_control_social_page_submit,
      TRUE ~ "other"),
    time_fact_page_submit = case_when(
      question_topic == "alien" ~ alien_fact_time_page_submit,
      question_topic == "recycle" ~ time_recy_info_page_submit,
      question_topic == "social" ~ time_social_info_page_submit
    )) %>% 
  select(-time_treat_alien_page_submit, -time_control_alien_page_submit,
         -time_recycle_treat_page_submit, -time_recycle_control_page_submit,
         -time_treat_social_page_submit, -time_control_social_page_submit,
         -alien_fact_time_page_submit, -time_recy_info_page_submit, -time_social_info_page_submit) %>% 
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         duration_in_seconds = as.double(duration_in_seconds),
         time_question_page_submit = as.double(time_question_page_submit),
         time_fact_page_submit = as.double(time_fact_page_submit))

#calculate the decile score
dat_time_to_complete_2 <- dat_time_to_complete_2 %>% 
   group_by(question_topic, treatment_bool) %>% 
   mutate(
    quantile_rank_time_question_page_submit = ntile(time_question_page_submit,10),
    quantile_rank_time_fact_page_submit = ntile(time_fact_page_submit,10)) %>% 
   ungroup()

decile_times <- dat_time_to_complete_2 %>% 
  group_by(question_topic, treatment_bool) %>% 
    summarise(
      quantile_rank_time_question_page_submit = quantile(time_question_page_submit, seq(0, 1, 1/10)),
      quantile_rank_time_fact_page_submit = quantile(time_fact_page_submit, seq(0, 1, 1/10))
      ) %>% 
    ungroup()

ggplot(dat_time_to_complete_2, 
       aes(time_question_page_submit,
           color = factor(treatment_bool))) + 
  geom_histogram() + 
  facet_wrap("question_topic")
  

dat_time_to_complete_2 %>% 
  filter(response_id == "R_0JoNfdq3IWhf0LT") %>%
  glimpse()  

quantile(dat_time_to_complete_2$time_question_page_submit,
         probs = seq(0,1,1/nrow(dat_time_to_complete_2)))
