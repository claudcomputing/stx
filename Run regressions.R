#5 
#run regressions

#Data clean up
### mutate and make H_W and H_H outcomes, searched, citation, found weapon, found drugs, etc
## might want to randomly select from non searched since finding a weapon is so infrequent

#Regressions on MC
glm.found.w <- glm(H_W ~ year +rich_neighborhood +tacos+steak, data = glm_data, family = "binomial")
result <- tidy(glm.found.w) %>% kable()
glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.w, type = "response"))

#Regressions on search citation etc

#Regressions on hit rate
glm.found.w <- glm(found.weapon ~ mc+ year, data = glm_data, family = "binomial")
result <- tidy(glm.found.w) %>% kable()
glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.w, type = "response"))

glm.found.d <- glm(found.drugs ~ mc + year, data = glm_data, family = "binomial")
result <- tidy(glm.found.d) %>% kable()
glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.d, type = "response"))





