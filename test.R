
th %>%
  select(Date:N7) %>%
  filter(N1 == 3 | N2 == 3) %>%
  arrange(desc(as.Date(Date,format="%Y-%m-%d"))) %>%
  print