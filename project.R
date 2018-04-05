colnames(census_data) <-c("p_count", "count", "label", "age", "capital_gain", "capital_loss", "education", "edu_years", "fnlwgt", "hours_week", 
                          "marital_status", "native_country", "occupation", "race", "relationship", "sex", "work_label", "label_page", 
                          "label_capital_gain", "lavel_capital_loss", "label_edu", "labe_edu_years", "label_hours", "labe_marital_status",
                           "label_country", "label_occupation", "labe_race", "label_relationship", "labe_sex", "labe_work_label")

edu <- group_by(census_data, education)
ct <- summarise(edu, count=n(), avg_edu = mean(edu_years))
ct1 <- arrange(ct, desc(avg_edu))


ggplot(ct1, aes(x= reorder(education,avg_edu), y=avg_edu)) +
  geom_point(aes(size = count, color = factor(avg_edu)), alpha=1/2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  labs(x= "Education", "Average Years of Education", "Education Vs. Education Years")

ggplot(data = census_data) +
  (mapping = aes(x =edu_years, y = age)) +
  geom_point()+
  geom_smooth()+
   facet_wrap(~ marital_status, nrow = 2)

ggplot(data = census_data) + 
  geom_bar(mapping = aes(x = occupation, color = education))

ggplot(data = census_data) +
  geom_smooth(mapping = aes(x = capital_gain, y = age))

ggplot(data = census_data) +
  (mapping = aes(x = education, y = fnlwgt , color = sex)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~ race, nrow = 2)

ggplot(data = census_data) +
  (mapping = aes(x = fnlwgt, y = sex)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~ race, nrow = 2)

ggplot(data = census_data) +
  geom_bar(mapping = aes(x = edu_years, y= , fill = race))


ggplot(data = census_data) +
  geom_smooth(mapping = aes(x = fnlwgt, y= edu_years , fill = race))

ggplot(data = census_data) +
  geom_smooth(mapping = aes(x = fnlwgt, y= edu_years , fill = occupation))

ggplot(data = census_data) +
  geom_bar(mapping = aes(x = occupation, y= , fill = sex))
