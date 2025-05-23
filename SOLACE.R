# Install and load required packages
packages <- c("readr", "dplyr", "ggplot2", "tm", "wordcloud", "RColorBrewer", "forcats", "shiny", "nnet")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)

library(readr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(forcats)
library(shiny)
library(nnet) # for multinom

# --- STEP 1: Read Data ---
csv_path <- "/home/hp/Downloads/amchab.csv"
data <- read_csv(csv_path)

# --- STEP 2: Data Cleaning & Validation ---
required_cols <- c("Sleep_Hours", "Stress_Level", "Most_Difficult_Subject")
missing_cols <- setdiff(required_cols, names(data))
if(length(missing_cols) > 0) {
  stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))
}

data <- data %>% 
  filter(!is.na(Sleep_Hours), !is.na(Stress_Level), !is.na(Most_Difficult_Subject))

data$Sleep_Hours <- as.numeric(data$Sleep_Hours)
data$Stress_Level <- as.numeric(data$Stress_Level)
data$Study_Hours <- as.numeric(data$Study_Hours)

# --- STEP 3: Categorize Stress Level ---
data <- data %>%
  mutate(Stress_Category = case_when(
    Stress_Level <= 3 ~ "Low",
    Stress_Level <= 7 ~ "Moderate",
    TRUE ~ "High"
  ))

# --- STEP 4: Sleep Hours Histogram ---
p1 <- ggplot(data, aes(x = Sleep_Hours)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", boundary = 0) +
  labs(title = "Sleep Hours Distribution", x = "Sleep Hours", y = "Count") +
  theme_minimal()
print(p1)

# --- STEP 5: Top 10 Most Difficult Subjects ---
top_subjects <- data %>%
  count(Most_Difficult_Subject, sort = TRUE) %>%
  slice_max(n, n = 10)

p2 <- ggplot(top_subjects, aes(x = fct_reorder(Most_Difficult_Subject, n), y = n)) +
  geom_col(fill = "salmon") +
  coord_flip() +
  labs(title = "Top 10 Most Difficult Subjects", x = "Subject", y = "Count") +
  theme_minimal()
print(p2)

# --- STEP 6: Sleep vs Stress Scatter Plot ---
p3 <- ggplot(data, aes(x = Sleep_Hours, y = Stress_Level)) +
  geom_jitter(color = "darkgreen", size = 2.5, alpha = 0.7, width = 0.2, height = 0.2) +
  labs(title = "Sleep Hours vs Stress Level", x = "Sleep Hours", y = "Stress Level") +
  theme_minimal()
print(p3)

# --- STEP 7: Stress Category Pie Chart ---
stress_dist <- data %>% count(Stress_Category)
p4 <- ggplot(stress_dist, aes(x = "", y = n, fill = Stress_Category)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Stress Level Distribution") +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1")
print(p4)

# --- STEP 8: Study Hours vs Stress Level Boxplot ---
p5 <- ggplot(data, aes(x = Stress_Category, y = Study_Hours, fill = Stress_Category)) +
  geom_boxplot() +
  labs(title = "Study Hours by Stress Category", x = "Stress Category", y = "Study Hours") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")
print(p5)

# --- STEP 9: Gender-wise Stress Analysis (if Gender column exists) ---
if ("Gender" %in% names(data)) {
  gender_data <- data %>% filter(!is.na(Gender) & Gender != "")
  if (nrow(gender_data) > 0) {
    gender_data$Gender <- as.factor(gender_data$Gender)
    p6 <- ggplot(gender_data, aes(x = Gender, fill = Stress_Category)) +
      geom_bar(position = "dodge") +
      labs(title = "Stress Category Distribution by Gender", x = "Gender", y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    print(p6)
  } else {
    cat("No valid data in 'Gender' column to plot.\n")
  }
} else {
  cat("'Gender' column not found in the data.\n")
}

# --- STEP 10: Descriptive Statistics (Improved Output) ---
summary_stats <- data %>%
  group_by(Stress_Category) %>%
  summarise(
    Avg_Sleep = round(mean(Sleep_Hours, na.rm = TRUE), 2),
    Avg_Study = round(mean(Study_Hours, na.rm = TRUE), 2),
    Count = n()
  )

if (requireNamespace("knitr", quietly = TRUE)) {
  cat("\nSummary of Sleep and Study Hours by Stress Category:\n")
  print(knitr::kable(summary_stats, align = "c"))
} else {
  cat("\nSummary of Sleep and Study Hours by Stress Category:\n")
  print(summary_stats)
}

# --- STEP 11: Correlation between Sleep and Stress (Improved Output) ---
correlation <- cor(data$Sleep_Hours, data$Stress_Level, use = "complete.obs", method = "pearson")
cat(sprintf("\nCorrelation between Sleep and Stress Level: %.2f\n", correlation))

# Interpretation of Correlation
if (!is.na(correlation)) {
  interpretation <- if (correlation > 0.5) {
    "There is a strong positive correlation—higher sleep hours are associated with higher stress levels."
  } else if (correlation > 0.3) {
    "There is a moderate positive correlation—higher sleep hours are somewhat associated with higher stress levels."
  } else if (correlation > 0.1) {
    "There is a weak positive correlation."
  } else if (correlation < -0.5) {
    "There is a strong negative correlation—higher sleep hours are associated with lower stress levels."
  } else if (correlation < -0.3) {
    "There is a moderate negative correlation—higher sleep hours are somewhat associated with lower stress levels."
  } else if (correlation < -0.1) {
    "There is a weak negative correlation."
  } else {
    "There is little to no correlation between sleep and stress levels."
  }
  cat("Interpretation:", interpretation, "\n")
}

# --- STEP 12: Word Cloud for Describe_Your_Stress ---
stress_texts <- na.omit(data$Describe_Your_Stress)
if(length(stress_texts) > 0) {
  stress_corpus <- VCorpus(VectorSource(stress_texts))
  stress_corpus <- tm_map(stress_corpus, content_transformer(tolower))
  stress_corpus <- tm_map(stress_corpus, removePunctuation)
  stress_corpus <- tm_map(stress_corpus, removeWords, stopwords("english"))
  # Remove empty documents after cleaning
  stress_corpus <- stress_corpus[!sapply(stress_corpus, function(x) identical(x$content, ""))]
  suppressWarnings(wordcloud(stress_corpus, max.words = 50, random.order = FALSE, 
                             colors = brewer.pal(8, "Dark2"), scale = c(2, 0.5)))
}

# --- STEP 13: Suggestions For Improvement as Random Text Message ---
suggestions_col <- names(data)[grepl("suggestion", names(data), ignore.case = TRUE)][1]
if (!is.na(suggestions_col) && suggestions_col %in% names(data)) {
  suggestions_texts <- na.omit(data[[suggestions_col]])
  suggestions_texts <- suggestions_texts[suggestions_texts != "" & !grepl("^\\s*$", suggestions_texts)]
  if(length(suggestions_texts) > 0) {
    set.seed(as.numeric(Sys.time())) # Seed with current time for randomness each run
    random_msg <- sample(suggestions_texts, 1)
    cat("\nSuggestion for Improvement:\n")
    cat(random_msg, "\n")
  }
}

# --- STEP 14: Export Cleaned Data and Summary Statistics ---
write_csv(data, "amchab_data_cleaned.csv")
write_csv(summary_stats, "amchab_summary_stats.csv")

# --- STEP 15: Optional - Interactive Dashboard using shiny ---
run_dashboard <- FALSE # Set to TRUE to launch the dashboard
if(run_dashboard) {
  ui <- fluidPage(
    titlePanel("Student Stress Data Dashboard"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("sleep", "Sleep Hours:", min = min(data$Sleep_Hours, na.rm = TRUE), 
                    max = max(data$Sleep_Hours, na.rm = TRUE), 
                    value = c(min(data$Sleep_Hours, na.rm = TRUE), max(data$Sleep_Hours, na.rm = TRUE))),
        selectInput("stress_cat", "Stress Category:", choices = unique(data$Stress_Category), 
                    selected = unique(data$Stress_Category), multiple = TRUE)
      ),
      mainPanel(
        plotOutput("sleepHist"),
        plotOutput("stressPie")
      )
    )
  )
  
  server <- function(input, output) {
    filtered <- reactive({
      data %>%
        filter(Sleep_Hours >= input$sleep[1], Sleep_Hours <= input$sleep[2], Stress_Category %in% input$stress_cat)
    })
    output$sleepHist <- renderPlot({
      ggplot(filtered(), aes(x = Sleep_Hours)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        labs(title = "Sleep Hours (Filtered)", x = "Sleep Hours", y = "Count") +
        theme_minimal()
    })
    output$stressPie <- renderPlot({
      ggplot(filtered() %>% count(Stress_Category), aes(x = "", y = n, fill = Stress_Category)) +
        geom_col(width = 1) +
        coord_polar("y") +
        labs(title = "Stress Level Distribution (Filtered)") +
        theme_void() +
        scale_fill_brewer(palette = "Pastel1")
    })
  }
  shinyApp(ui, server)
}
# --- STEP 16: Improved Prediction Function with Manual Rules ---
predict_stress_category_manual <- function(sleep_hours, study_hours) {
  # Logic-based stress prediction that makes more intuitive sense
  
  # Rule 1: Very low sleep is always high stress
  if (sleep_hours < 4) {
    return("High")
  }
  
  # Rule 2: Good balance of sleep and study
  if (sleep_hours >= 7 && sleep_hours <= 9) {
    if (study_hours >= 2 && study_hours <= 6) {
      return("Low")  # Balanced lifestyle
    }
  }
  
  # Rule 3: Too much study + moderate or less sleep = high stress
  if (study_hours > 6 && sleep_hours <= 7) {
    return("High")
  }
  
  # Rule 4: Good sleep but high study
  if (sleep_hours > 7 && study_hours > 6) {
    return("Moderate")  # Good sleep balances high study
  }
  
  # Rule 5: Very low study + good sleep = low stress
  if (study_hours < 2 && sleep_hours >= 7) {
    return("Low")
  }
  
  # Rule 6: Moderate sleep (5-7) with moderate study (2-6)
  if (sleep_hours >= 5 && sleep_hours < 7 && study_hours >= 2 && study_hours <= 6) {
    return("Moderate")
  }
  
  # Rule 7: Too much sleep (>10) might indicate other issues
  if (sleep_hours > 10) {
    return("Moderate")  # Might indicate depression or other issues
  }
  
  # Default fallback for any cases not covered
  if (sleep_hours <= 7) {
    return("Moderate")
  } else {
    return("Low")
  }
}

# Function to get detailed explanation for a prediction
get_stress_explanation <- function(sleep_hours, study_hours, prediction) {
  explanation <- ""
  
  if (prediction == "High") {
    if (sleep_hours < 4) {
      explanation <- paste(explanation, "Very low sleep (less than 4 hours) severely impacts cognitive function and emotional regulation,",
                           "leading to high stress levels regardless of study time. Sleep deprivation increases cortisol levels",
                           "and decreases the brain's ability to manage stress.\n")
    }
    if (study_hours > 6 && sleep_hours <= 7) {
      explanation <- paste(explanation, "High study workload (more than 6 hours) combined with insufficient sleep (7 hours or less)",
                           "creates a harmful imbalance. The brain doesn't have enough recovery time while being subjected to",
                           "prolonged mental exertion, leading to high stress and potential burnout.\n")
    }
  } 
  else if (prediction == "Moderate") {
    if (sleep_hours > 7 && study_hours > 6) {
      explanation <- paste(explanation, "While the study workload is high (more than 6 hours), good sleep (more than 7 hours)",
                           "provides some buffer against stress. Adequate sleep supports memory consolidation and emotional regulation,",
                           "but the extended study hours still create significant mental demands, resulting in moderate stress.\n")
    }
    if (sleep_hours >= 5 && sleep_hours < 7 && study_hours >= 2 && study_hours <= 6) {
      explanation <- paste(explanation, "Moderate sleep (5-7 hours) with a reasonable study load (2-6 hours)",
                           "represents a common but not ideal balance. This sleep amount is slightly below optimal recommendations",
                           "for most students, leading to moderate stress levels despite the manageable study workload.\n")
    }
    if (sleep_hours > 10) {
      explanation <- paste(explanation, "Excessive sleep (more than 10 hours) can be associated with disrupted sleep cycles,",
                           "potential depression, or other health issues. While it may seem restful, oversleeping can lead to",
                           "lethargy, reduced motivation, and moderate stress levels due to physiological imbalances.\n")
    }
  } 
  else if (prediction == "Low") {
    if (sleep_hours >= 7 && sleep_hours <= 9 && study_hours >= 2 && study_hours <= 6) {
      explanation <- paste(explanation, "Optimal sleep duration (7-9 hours) combined with an effective study schedule (2-6 hours)",
                           "represents an ideal balance. This sleep pattern aligns with recommended guidelines for young adults,",
                           "allowing for proper cognitive function, emotional regulation, and stress management.\n")
    }
    if (study_hours < 2 && sleep_hours >= 7) {
      explanation <- paste(explanation, "Minimal study demands (less than 2 hours) paired with sufficient sleep (7+ hours)",
                           "creates a low-stress environment. The limited academic workload allows the body and mind to recover fully,",
                           "although this pattern may not be sustainable during exam periods or for long-term academic success.\n")
    }
  }
  
  # Additional scientific context based on prediction
  if (prediction == "High") {
    explanation <- paste(explanation, "\nScientific context: High stress states are characterized by elevated cortisol levels,",
                         "which affect memory formation and can lead to anxiety and reduced immune function. Studies show that",
                         "sleep deprivation compounds these effects dramatically, creating a harmful cycle.\n")
  } else if (prediction == "Moderate") {
    explanation <- paste(explanation, "\nScientific context: Moderate stress can actually be beneficial for alertness and focus in short bursts,",
                         "but becomes problematic when sustained over longer periods. The body's ability to manage this stress level depends",
                         "heavily on recovery periods (particularly sleep) and overall health habits.\n")
  } else {
    explanation <- paste(explanation, "\nScientific context: Low stress states allow for optimal learning and memory consolidation.",
                         "Research indicates that adequate sleep (7-9 hours) significantly improves cognitive performance and emotional regulation.",
                         "This balanced approach supports long-term academic success and mental well-being.\n")
  }
  
  # Recommendations based on prediction
  explanation <- paste(explanation, "\nRecommendations:")
  
  if (prediction == "High") {
    explanation <- paste(explanation, "\n1. Prioritize sleep immediately - aim for at least 7 hours nightly")
    explanation <- paste(explanation, "\n2. Break study sessions into 25-minute focused blocks with 5-minute breaks")
    explanation <- paste(explanation, "\n3. Consider seeking support from academic counseling services")
    explanation <- paste(explanation, "\n4. Practice brief mindfulness exercises (even 2-5 minutes) between study sessions")
  } else if (prediction == "Moderate") {
    explanation <- paste(explanation, "\n1. Optimize sleep schedule by maintaining consistent sleep/wake times")
    explanation <- paste(explanation, "\n2. Review study efficiency - focus on quality over quantity of study time")
    explanation <- paste(explanation, "\n3. Incorporate short relaxation techniques like deep breathing or brief walks")
    explanation <- paste(explanation, "\n4. Monitor caffeine intake, especially in afternoons and evenings")
  } else {
    explanation <- paste(explanation, "\n1. Maintain current sleep patterns while ensuring study time remains effective")
    explanation <- paste(explanation, "\n2. Consider adding spaced repetition techniques to study sessions for better retention")
    explanation <- paste(explanation, "\n3. Schedule regular physical activity to maintain overall well-being")
    explanation <- paste(explanation, "\n4. Use this balanced state to develop sustainable academic and wellness habits")
  }
  
  return(explanation)
}

# --- Interactive CLI Input with BOTH Prediction Methods ---
cat("\n--- Student Stress Category Predictor ---\n")
user_sleep <- readline(prompt="Enter Sleep Hours: ")
user_study <- readline(prompt="Enter Study Hours: ")

# Convert inputs to numeric, handle invalid input
if (suppressWarnings(is.na(as.numeric(user_sleep))) | suppressWarnings(is.na(as.numeric(user_study)))) {
  cat("Invalid input. Please enter numeric values for sleep and study hours.\n")
} else {
  user_sleep <- as.numeric(user_sleep)
  user_study <- as.numeric(user_study)
  
  # Get both data-based and rule-based predictions
  pred_data <- predict_stress_category(user_sleep, user_study)
  pred_manual <- predict_stress_category_manual(user_sleep, user_study)
  
  # Compare the results
  cat(sprintf("Data-based Prediction: %s\n", pred_data))
  cat(sprintf("Manual Rule Prediction: %s\n", pred_manual))
  
  # Recommendation based on manual rules
  cat(sprintf("\nRECOMMENDED PREDICTION: %s\n", pred_manual))
  
  # Detailed explanation
  cat("\n----- DETAILED ANALYSIS -----\n")
  cat(get_stress_explanation(user_sleep, user_study, pred_manual))
  
  # General advice for students
  cat("\n\nREMEMBER: Everyone's stress responses are unique. This prediction is based on general patterns,")
  cat("\nbut individual factors like nutrition, exercise, social support, and personal resilience all play")
  cat("\nimportant roles in how we experience and manage stress.\n")
}

# Print the random student's stress info
set.seed(Sys.time())
random_row <- data[sample(nrow(data), 1), ]
cat(sprintf(
  "\n\n----- RANDOM STUDENT PROFILE -----\nSleep Hours: %s, Study Hours: %s, Stress Level: %s, Stress Category: %s\n",
  random_row$Sleep_Hours,
  random_row$Study_Hours,
  random_row$Stress_Level,
  random_row$Stress_Category
))

# Apply manual prediction to the random student
random_manual_pred <- predict_stress_category_manual(random_row$Sleep_Hours, random_row$Study_Hours)
cat(sprintf("Manual prediction for this student: %s\n", random_manual_pred))

# Add explanation for random student
cat("\nAnalysis for random student:")
cat(get_stress_explanation(random_row$Sleep_Hours, random_row$Study_Hours, random_manual_pred))