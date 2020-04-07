#5 
#run regressions;


################Notes 
#found contraband (numerator of hit rate)
data$found.true <- NA
data$found.true <- ifelse(data$contraband_found == T, T, ifelse(data$found.pistol ==T, T, ifelse(data$found.rifle==T, T, ifelse(data$found.assault == T, T, ifelse(data$found.knife== T, T, ifelse(data$found.machinegun==T, T, ifelse(data$found.other== T, T, ifelse(data$found.gun == T, T, ifelse(data$found.weapon==T, T, F)))))))))
names(tx)


#Data frame for Predicted Probability Plot
glm_data <- data %>% filter(suspected.crime == "cpw") %>% mutate(year.f = factor(year, level = c(2012:2016)))

glm.found.t <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build + suspect.sex + year.f*suspect.race, data = glm_data, family = "binomial")

glm.found <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build + suspect.sex + year*suspect.race, data = glm_data, family = "binomial")

glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.t, type = "response"))

# Save the logistic regression result
result <- tidy(glm.found.t) %>% kable()

#time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")
#forcats::fct_inorder(Day44$Sample)