complete.dataset.GG %>%
  filter(grepl('School',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("schools.csv")

complete.dataset.GG %>%
  filter(grepl('College',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("colleges.csv")

complete.dataset.GG %>%
  filter(grepl('Institute',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("institute.csv")

complete.dataset.GG %>%
  filter(grepl('Universities',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("university.csv")

complete.dataset.GG %>%
  filter(grepl('Tutor',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("tutors.csv")

complete.dataset.GG %>%
  filter(grepl('Study',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("study.csv")

complete.dataset.GG %>%
  filter(grepl('Teacher',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("teacher.csv")

complete.dataset.GG %>%
  filter(grepl('Cloth',all.professions)) %>% 
  select(name,km,all.B,all.professions) %>% write.csv("clothing.csv")