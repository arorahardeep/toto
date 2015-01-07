
th %>%
  select(Date:N7) %>%
  filter(N1 == 3 | N2 == 3) %>%
  arrange(desc(as.Date(Date,format="%Y-%m-%d"))) %>%
  print

gather(th, numID, numDrawn, N1:N7, na.rm=TRUE) %>% mutate(numID = extract_numeric(numID)) %>% arrange(as.Date(Date,format="%Y-%m-%d"),numID)

students2 %>%
  gather( sex_class, count, -grade  ) %>%
  separate( sex_class, c("sex", "class")) %>%
  print

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread( test, grade ) %>%
  print

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = extract_numeric(class)) %>%
  ### Call to mutate() goes here %>%
  print

rbind_list(passed,failed)

sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate( total = sum(count),
          prop = count/total
  ) %>% print



