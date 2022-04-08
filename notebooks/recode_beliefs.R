## Fisher Kim, Kai Ying, Lucas Schroyer, Peter YH Kim
# Recode pre and post beliefs and excitement levels

prior_belief <- tibble(
  #Pre beliefs
  aliens_belief = 
    c("Definitely not", "Probably not", "Probably yes", "Definitely yes"),
  recycling_belief = 
    c("Very unimportant. E.g. it is a waste of time and taxpayer dollars as most recycled products likely end up in a landfill and shifts attention away from other environmental efforts that have a much greater impact",
      "Somewhat unimportant. E.g. Good in principle, but U.S. recycling programs are largely ineffective and it is unclear how much impact current programs have on the environment",
      "Somewhat important. E.g. it encourages environmental stewardship but could be improved",
      "Very important. E.g. it creates jobs and has significant environmental advocacy benefits"),
  social_belief = 
    c("Definitely not", 
      "Not really. Maybe one or two instances", 
      "Yes in some ways", 
      "Definitely yes")) %>% 
  gather(key = "belief_question", value = "belief_answer_pre") %>% 
  bind_cols(tibble(
    aliens_belief_pre =   c(0, 1, 2, 3),
    recycling_belief_pre = c(0, 1, 2, 3),
    social_belief_pre = c(0, 1, 2, 3)) %>%
      gather(key = "belief_question_pre", value = "belief_answer_ohe_pre")
    ) %>% 
  select(belief_question_pre, belief_answer_pre, belief_answer_ohe_pre) %>% 
  mutate(question_topic = case_when(
    belief_question_pre %like% "alien" ~ "alien",
    belief_question_pre %like% "recyc" ~ "recycle",
    belief_question_pre %like% "social" ~ "social",
    TRUE ~ "other")
    )

post_belief <- tibble(
  #Post beliefs
  aliens_belief_change = 
    c("Maintain my same relative belief level as before", 
      "Believe in them less", "Believe in them more"),
  recycle_belief_change = 
    c("Maintain my same relative belief level as before", 
      "Believe it is less beneficial than I did before", 
      "Believe in its benefits more"),
  social_belief_change = 
    c("Maintain my same relative belief level as before", 
      "Believe teenagers should have LESS restrictions compared to what I felt before",
      "Believe teenagers should have MORE restrictions compared to what I felt before" 
    )) %>% 
  gather(key = "belief_question", value = "belief_answer_post") %>% 
  bind_cols(tibble(
    aliens_belief_post = c(0, -1, 1),
    recycle_belief_post = c(0, -1, 1),
    social_belief_post = c(0, -1, 1)
  ) %>% 
    gather(key = "belief_question_post", value = "belief_answer_ohe_post")) %>% 
  select(belief_question_post, belief_answer_post, belief_answer_ohe_post)  %>% 
  mutate(question_topic = case_when(
    belief_question_post %like% "alien" ~ "alien",
    belief_question_post %like% "recyc" ~ "recycle",
    belief_question_post %like% "social" ~ "social",
    TRUE ~ "other")
  )

dat_change_in_belief_2 <- dat_change_in_belief %>% 
  gather(key = "belief_question_pre", value = "belief_answer_pre", 
         c(`alien_belief`, `recycling_belief`, `social_belief`)
         ) %>% 
  mutate(belief_question_pre = str_remove(belief_question_pre, "_belief"),
         belief_question_pre = ifelse(belief_question_pre %like% "recyc", "recycle", belief_question_pre)) %>% 
  filter(question_topic == belief_question_pre) %>% 
  gather(key = "belief_question_post", value = "belief_answer_post", 
         c(`aliens_belief_change`, `recycle_belief_change`, `social_belief_change`)
  ) %>% 
  mutate(belief_question_post = str_remove(belief_question_post, "_belief_change"),
         belief_question_post = ifelse(belief_question_post %like% "recyc", "recycle", belief_question_post),
         belief_question_post = ifelse(belief_question_post %like% "alien", "alien", belief_question_post)) %>% 
  # mutate(belief_question_post = paste0())
  filter(question_topic == belief_question_post) %>% 
  select(-belief_question_pre, -belief_question_post) %>% 
  left_join(prior_belief, by = c("belief_answer_pre", "question_topic")) %>%
  left_join(post_belief, by = c("belief_answer_post", "question_topic")) 


# dat_change_in_belief_2 %>% filter(question_topic %like% "social") %>% View()

# QC
dat_change_in_belief_2 %>%
filter(response_id == "R_0JoNfdq3IWhf0LT") %>% glimpse()
# dat_change_in_belief %>% 
#   filter(response_id == "R_0JoNfdq3IWhf0LT") %>% glimpse()

# QC to make sure there are only 3 topics
stopifnot(nrow(dat_change_in_belief_2 %>% count(response_id) %>% filter(n != 3)) == 0)
# each participant has 3 responses

#QC no participants were dropped
stopifnot(length(unique(dat_change_in_belief_2$response_id)) == length(unique(d_4$response_id)))
# None were dropped


# EDA
dat_change_in_belief_2 %>% count(belief_answer_ohe_pre)
# belief_answer_ohe_pre   n
# 1                     0  31
# 2                     1 160
# 3                     2 207
# 4                     3  64


dat_change_in_belief_2 %>% count(belief_answer_ohe_post)
# belief_answer_ohe_post   n
# 1                     -1  34
# 2                      0 351
# 3                      1  77

dat_change_in_belief_2 %>% 
  count(question_topic, belief_answer_ohe_pre) %>% 
  mutate(n_perc = round(n/nrow(dat_change_in_belief_2)*3,2))
# question_topic belief_answer_ohe_pre  n n_perc
# 1           alien                     0 23   0.15
# 2           alien                     1 80   0.52
# 3           alien                     2 47   0.31
# 4           alien                     3  4   0.03
# 5         recycle                     0  7   0.05
# 6         recycle                     1 43   0.28
# 7         recycle                     2 62   0.40
# 8         recycle                     3 42   0.27
# 9          social                     0  1   0.01
# 10         social                     1 37   0.24
# 11         social                     2 98   0.64
# 12         social                     3 18   0.12

dat_change_in_belief_2 %>% 
  count(question_topic, belief_answer_ohe_post) %>% 
  mutate(n_perc = round(n/nrow(dat_change_in_belief_2)*3,2))
# question_topic belief_answer_ohe_post   n n_perc
# 1          alien                     -1  18   0.12
# 2          alien                      0 127   0.82
# 3          alien                      1   9   0.06
# 4        recycle                     -1  14   0.09
# 5        recycle                      0 101   0.66
# 6        recycle                      1  39   0.25
# 7         social                     -1   2   0.01
# 8         social                      0 123   0.80
# 9         social                      1  29   0.19