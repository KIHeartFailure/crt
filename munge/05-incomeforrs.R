
# income

inc <- data3 %>%
  filter(data == "rs") %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop")

data3 <- left_join(
  data3,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-incmed)
