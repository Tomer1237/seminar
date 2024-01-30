

database123 <- read.csv("C:/seminar/database123.csv")

coefficients <- numeric()
p_values <- numeric()

num_questions <-10 

for (question in 1:num_questions) {
  pre_col <- paste0("q", question, "_pre")
  after_col <- paste0("q", question, "_after")
  
  database123[[paste0("TreatmentAfter_q", question)]] <- database123$group * database123[[after_col]]
  
  model <- lm(database123[[paste0("q", question, "_after")]] ~ database123$group + database123[[after_col]] + database123[[paste0("TreatmentAfter_q", question)]], data = database123)
  
  print(summary(model))
  
  
  coefficients[question] <- coef(model)[4]  
  p_values[question] <- summary(model)$coefficients[4, 4]  
}

for (question in 1:num_questions) {
  cat("Question", question, "Coefficient:", coefficients[question], "P-value:", p_values[question], "\n")
}

