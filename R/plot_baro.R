# Plot barometer

# Plotting Question 171 
baro_perc_nat %>%
  filter(name == "q171") %>%
  ggplot(., aes(x = share, y = reorder(country, share))) +
  geom_col() +
  facet_wrap(~ value)

# Plotting Question 163
baro_perc_nat %>%
  filter(name == "q163") %>%
  filter(value %in% c("china","japan","india","united states")) %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ value, scales = "free_x") +
  labs(title="Which country has the most influence in Asia", subtitle ="Question 163",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

baro %>% names()

# Plotting Question 165
baro_perc_nat %>%
  filter(country != "china", value != "missing", value != "decline to answer", value != "do not understand the question", value != "can't choose") %>%
  filter(name == "q165") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ value, scales = "free_x") +
  labs(title="Does China do more good or harm to the region?", subtitle ="Question 165",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()
  

# Plotting Question 167 
baro_perc_nat %>%
  filter(name == "q167") %>% 
  filter(country != "china") %>%
  filter(value %in% c("china", "china and singapore", "japan", "singapore", "united states", "we should follow our country's own model")) %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ value, scales = "free_x") +
  labs(title="Which Country Should be a Model for our own Country's Future Development?", subtitle ="Question 167",
        x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 168
baro_perc_nat %>%
  filter(!value %in% c("decline to answer","can't choose", "missing","do not understand the question")) %>%
  filter(name == "q168") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ value, scales = "free_x")+
  labs(title="How much influence does China have on our country?", subtitle ="Question 168",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 169
baro_perc_nat %>%
  filter(country != "china") %>%
  filter(!value %in% c("decline to answer","can't choose","do not understand the question", "missing")) %>%
  filter(name == "q169") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ value, scales = "free_x") +
  labs(title="Generally speaking, the influence China has on our country is?", subtitle ="Question 169",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()
  


# Plotting Question 127
baro_perc_nat %>%
  filter(!value %in% c("do not understand the question", "can't choose", "decline to answer", "missing")) %>%
  filter(name == "q127") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ value, scales = "free_x") +
  labs(title="If you had to choose between democracy and economic development, what is more important to you?", subtitle ="Question 127",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()
  

# Plotting Question 125
baro_perc_nat %>%
  filter(!value %in% c("do not understand the question", "can't choose", "decline to answer", "missing")) %>%
  filter(name =="q125") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ value, scales = "free_x",labeller = label_wrap_gen(width = 50)) +
  labs(title="Which of the following statements comes closest to your own opinion?", subtitle ="Question 125",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()
  
# Plotting Question 125 Rural and Urban
baro_perc_nat_urban_rural %>%
  filter(!value %in% c("do not understand the question", "can't choose", "decline to answer", "missing")) %>%
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(name =="q125") %>%
  filter(level !="missing") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(level ~ value, scales = "free_x",labeller = label_wrap_gen(width = 50)) +
  labs(title="Which of the following statements comes closest to your own opinion?", subtitle ="Question 125",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 167 RURAL AND URBAN
baro_perc_nat_urban_rural %>%
  filter(name == "q167") %>% 
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(country != "china") %>%
  filter(value %in% c("china", "china and singapore", "japan", "singapore", "united states", "we should follow our country's own model")) %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(level ~ value, scales = "free_x") +
  labs(title="Which Country Should be a Model for our own Country's Future Development?", subtitle ="Question 167",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 163 RURAL URBAN
baro_perc_nat_urban_rural %>%
  filter(name == "q163") %>%
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(value %in% c("china","japan","india","united states")) %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(level ~ value, scales = "free_x") +
  labs(title="Which country has the most influence in Asia", subtitle ="Question 163",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 169 RURAL URBAN
baro_perc_nat_urban_rural %>%
  filter(country != "china") %>%
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(!value %in% c("decline to answer","can't choose","do not understand the question", "missing")) %>%
  filter(name == "q169") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(level ~ value, scales = "free_x") +
  labs(title="Generally speaking, the influence China has on our country is?", subtitle ="Question 169",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Plotting Question 165
baro_perc_nat_urban_rural %>%
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(country != "china", value != "missing", value != "decline to answer", value != "do not understand the question", value != "can't choose") %>%
  filter(name == "q165") %>%
  ggplot(., aes(x = share, y = reorder(country, share), fill = country)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(level ~ value, scales = "free_x") +
  labs(title="Does China do more good or harm to the region?", subtitle ="Question 165",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

# Percentage Point Difference Between Urban and Rural question 165
baro_perc_nat_urban_rural %>%
  filter(!country %in% c("china", "hong kong", "singapore")) %>%
  filter(country != "china", value != "missing", value != "decline to answer", value != "do not understand the question", value != "can't choose") %>%
  filter(name == "q165") %>%
  select(-n, -sum_country) %>%
  pivot_wider(names_from = level, values_from = share) %>%
  mutate(difference = urban - rural) %>%
  ggplot(., aes(x = difference, y = reorder(country, difference), fill = country)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = difference, label = round(difference, 1)), hjust = .2, colour = "black") +
  facet_wrap(~ value, scales = "free" ) +
  labs(title="Does China do more good or harm to the region? Difference in Urban vs Rural Districts", subtitle ="Question 165 answers: Percentage Point difference between Urban and Rural residents",
       x="share of respondents", y="country name", caption = "Source: asianbarometersurvey wave 4") +
  theme_fivethirtyeight()

  


# Scatterplot between 127 and 167
corr127and167 <- baro_perc %>%
  filter(value %in% c("economic development is definitely more important", "economic development is somewhat more important", "china") & name %in% c("q127", "q167")) %>%
  ungroup() %>%
  select(-n, -sum_country, -name) %>%  
  pivot_wider(names_from = value, values_from = share) %>%
  mutate(priority_economy = `economic development is definitely more important` + `economic development is somewhat more important`)

ggplot(corr127and167, aes(x = china, y= priority_economy)) + 
  geom_point(aes(color = country)) + 
  geom_smooth(method="lm", se=F)

# a per x for this linear function
lm(china ~ priority_economy, data = corr127and167)

# Rural urban divide
rural_or_urban %>%
  filter(value == "prefers economic" & name == "q127") %>%
  ggplot(., aes(y = share, x = level)) +
  geom_col() +
  labs(title = "Share of people who answered 'economic development is more important'",
       subtitle = "Share by rural versus urban population across all countries")


# Divide between countries q127
rural_or_urban_per_country %>%
  filter(name == "q127") %>%
  ggplot(., aes(y = prefers_democracy, x = prefers_economic)) +
  geom_point(size = 4, aes(colour = country)) +
  geom_label_repel(aes(label = country)) +
  facet_wrap(~level)

rural_or_urban_per_country %>%
  filter(name == "q127") %>%
  ggplot(., aes(y = prefers_democracy, x = prefers_economic)) +
  geom_point(size = 4, aes(colour = level)) +
  geom_label_repel(aes(label = level)) +
  facet_wrap(~country)

rural_or_urban_per_country %>%
  filter(name == "q127") %>%
  mutate(diff = prefers_democracy - prefers_economic) %>%
  ggplot(., aes(y = diff, x = country)) +
  geom_point(size = 4, aes(colour = level)) +
  geom_label_repel(aes(label = level))

# Divide between countries q169
rural_or_urban_per_country %>%
  filter(name == "q169") %>%
  ggplot(., aes(y = more_negative_than_positive, x = more_positive_than_negative)) +
  geom_point(size = 4, aes(colour = country)) +
  geom_label_repel(aes(label = country)) +
  facet_wrap(~level)

rural_or_urban_per_country %>%
  filter(name == "q169") %>%
  ggplot(., aes(y = prefers_democracy, x = prefers_economic)) +
  geom_point(size = 4, aes(colour = level)) +
  geom_label_repel(aes(label = level)) +
  facet_wrap(~country)

rural_or_urban_per_country %>%
  filter(name == "q169") %>%
  mutate(diff = prefers_democracy - prefers_economic) %>%
  ggplot(., aes(y = diff, x = country)) +
  geom_point(size = 4, aes(colour = level)) +
  geom_label_repel(aes(label = level))

# 169 and 127 -----
baro_169_127 <- baro_selec %>%
  select(country, year, region, level, q127, q169) %>%
  mutate(q127 = case_when(q127 %in% c("democracy is definitely more important",
                                      "democracy is somewhat more important") ~ "democracy important",
                          q127 %in% c("economic development is definitely more important",
                                      "economic development is somewhat more important") ~ "economic important",
                          TRUE ~ NA_character_),
         q169 = case_when(q169 %in% c("somewhat negative", "negative", "very negative") ~ "negative",
                          q169 %in% c("somewhat positive", "positive", "very positive") ~ "positive",
                          TRUE ~ NA_character_)
  )

head(baro_169_127)

# Chi Square for all countries
chisq.test(baro_169_127$q127, baro_169_127$q169) 

# Chi Square by country
# P-value of test for each country
baro_169_127_p_values <- baro_169_127 %>%
  filter(country != "china") %>% # remove China
  group_by(country) %>%
  summarise(p_value_169_127 = chisq.test(q127, q169)$p.value)

baro_169_127_p_values

# Plot p-value for each country
baro_169_127_p_values %>%
  mutate(country = str_to_title(country)) %>%
  ggplot(., aes(y = reorder(country, p_value_169_127), x = p_value_169_127)) +
  geom_col(aes(fill = country), show.legend = FALSE) +
  geom_vline(xintercept = 0.05) +
  labs(title = "P-value for each country of Chi-squared test between value in democracy and perception of China",
       subtitle = "The p-value for each country of a Chi-squared test between their citizen's perceptions of democracy and China",
       x = "P-value",
       y = NULL) +
  scale_x_continuous(limits = c(0, 1))


# 169 and 171 ------
baro_selec$q127 %>% unique
baro_selec$q169 %>% unique
baro_selec$q171 %>% unique
baro_selec$q164 %>% unique
baro_selec$q165 %>% unique

# Subset
baro_169_others <- baro_selec %>%
  mutate(q169 = case_when(q169 %in% c("somewhat negative", "negative", "very negative") ~ "negative",
                          q169 %in% c("somewhat positive", "positive", "very positive") ~ "positive",
                          TRUE ~ NA_character_),
         q171 = case_when(q171 %in% c("somewhat negative", "negative", "very negative") ~ "negative",
                          q171 %in% c("somewhat positive", "positive", "very positive") ~ "positive",
                          TRUE ~ NA_character_),
         q164 = case_when(q164 %in% c("somewhat more good than harm", "much more good than harm") ~ "more good",
                          q164 %in% c("somewhat more harm than good", "much more harm than good") ~ "more harm",
                          TRUE ~ NA_character_),
         q165 = case_when(q165 %in% c("somewhat more good than harm", "much more good than harm") ~ "more good",
                          q165 %in% c("somewhat more harm than good", "much more harm than good") ~ "more harm",
                          TRUE ~ NA_character_)
  )


baro_169_others_p_values <- baro_169_others %>%
  filter(country != "china") %>% # remove China
  group_by(country) %>%
  summarise(p_value_164_and_165 = chisq.test(q164, q165)$p.value,
            p_value_169_and_171 = chisq.test(q169, q171)$p.value)

baro_169_others_p_values

# Plot
# Between 164 and 165
baro_169_others_p_values %>%
  mutate(country = str_to_title(country)) %>%
  ggplot(., aes(y = reorder(country, p_value_164_and_165), x = p_value_164_and_165)) +
  geom_col(aes(fill = country), show.legend = FALSE) +
  geom_vline(xintercept = 0.05) +
  scale_x_continuous(limits = c(0, 1))

# Between 169 and 171
baro_169_others_p_values %>%
  mutate(country = str_to_title(country)) %>%
  ggplot(., aes(y = reorder(country, p_value_169_and_171), x = p_value_169_and_171)) +
  geom_col(aes(fill = country), show.legend = FALSE) +
  geom_vline(xintercept = 0.05) +
  scale_x_continuous(limits = c(0, 1))










