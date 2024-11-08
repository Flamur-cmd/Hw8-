Flamur Kukaj, Abdullah Al Aman, Marvin Harricharran,Alicia Persad

acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")

acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0",
                                       "Mexican" = "1",
                                       "Puerto Rican" = "2",
                                       "Cuban" = "3",
                                       "Other" = "4")


trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )

trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)


table(trad_data$he_more_than_5yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))

ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)

ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)
ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)


ols_out4 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + STATEFIP, data = trad_data)
summary(ols_out4)
ols_out5 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + REGION, data = trad_data)
summary(ols_out5)



ggplot(trad_data, aes(x = educ_numeric, y = he_more_than_5yrs_than_her)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probability of Being More than 5 Years Older by Education Level",
       x = "Education Level",
       y = "Probability") +
  theme_minimal()



library(car)


ols_out <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)


linearHypothesis(ols_out, "educ_numeric = 0")


linearHypothesis(ols_out, c("educ_numeric = 0", "h_educ_numeric = 0"))


ggplot(trad_data, aes(x = AGE, y = he_more_than_5yrs_than_her)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probability of Being More than 5 Years Older",
       x = "Age of Woman",
       y = "Probability (1 = He is Older by 5+ Years)") +
  theme_minimal()
