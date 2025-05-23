# 🧠 SOLACE – Student Outlook on Lifestyle, Academics, and Challenges through Exploration

**SOLACE** is a data-driven R project designed to analyze lifestyle, academic habits, and stress levels among students. By leveraging data visualization, text mining, and statistical analysis, it aims to uncover patterns that can inform well-being interventions and foster academic success.

---

## 📌 Objectives

- Analyze the relationship between sleep hours, study hours, and stress levels.
- Identify the most challenging subjects reported by students.
- Visualize trends and distributions using graphs and plots.
- Use text mining to interpret emotional responses.
- Provide personalized stress-level predictions based on user inputs.
- Offer evidence-based well-being recommendations.

---

## 📂 Files

- `data.csv` – Raw input dataset (from Google Forms).
- `data_cleaned.csv` – Cleaned and processed dataset.
- `summary_stats.csv` – Aggregated summary statistics.
- `solace_analysis.R` – Main R script for data analysis, visualization, prediction, and reporting.

---

## 🔧 Technologies & Libraries Used

- **R** (base)
- `readr`, `dplyr` – Data handling and transformation
- `ggplot2` – Visualizations
- `tm`, `wordcloud` – Text mining and word cloud generation
- `RColorBrewer`, `forcats` – Aesthetic enhancements and factor handling
- `nnet` – Multinomial prediction (if used)
- `shiny` – Interactive dashboard (optional)

---

## 📊 Features

### ✅ Data Visualizations
- Histogram of sleep hours
- Bar chart of top 10 most difficult subjects
- Scatter plot of sleep vs. stress
- Pie chart of stress categories
- Box plot of study hours by stress level
- Gender-wise stress distribution

### 🧠 Predictive Logic
- Rule-based stress level predictor using user input (sleep & study hours)
- Detailed scientific and practical explanations for each prediction

### 💬 Text Analysis
- Word cloud generated from students' stress descriptions
- Random suggestion of well-being tips from student responses

### 🧪 Correlation
- Pearson correlation between sleep hours and stress level, with interpretation

---

## 🚀 How to Run

1. Install R and RStudio (if not already installed).
2. Ensure required libraries are installed by running:

   ```r
   packages <- c("readr", "dplyr", "ggplot2", "tm", "wordcloud", "RColorBrewer", "forcats", "shiny", "nnet")
   new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
   if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
