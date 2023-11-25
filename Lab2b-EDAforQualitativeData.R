.libPaths()

lapply(.libPaths(), list.files)

if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE) 
}
require("dplyr")

if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE)
}
require("ggplot2")

if (!is.element("ggrepel", installed.packages()[, 1])) {
  install.packages("ggrepel", dependencies = TRUE)
}
require("ggrepel")

if (!is.element("ggraph", installed.packages()[, 1])) {
  install.packages("ggraph", dependencies = TRUE)
}
require("ggraph")

if (!is.element("tidytext", installed.packages()[, 1])) {
  install.packages("tidytext", dependencies = TRUE)
}
require("tidytext")

if (!is.element("tidyr", installed.packages()[, 1])) {
  install.packages("tidyr", dependencies = TRUE)
}
require("tidyr")

if (!is.element("widyr", installed.packages()[, 1])) {
  install.packages("widyr", dependencies = TRUE)
}
require("widyr")

if (!is.element("gridExtra", installed.packages()[, 1])) {
  install.packages("gridExtra", dependencies = TRUE)
}
require("gridExtra")

if (!is.element("knitr", installed.packages()[, 1])) {
  install.packages("knitr", dependencies = TRUE)
}
require("knitr")

if (!is.element("kableExtra", installed.packages()[, 1])) {
  install.packages("kableExtra", dependencies = TRUE)
}
require("kableExtra")

if (!is.element("formattable", installed.packages()[, 1])) {
  install.packages("formattable", dependencies = TRUE)
}
require("formattable")

if (!is.element("circlize", installed.packages()[, 1])) {
  install.packages("circlize", dependencies = TRUE)
}
require("circlize")

if (!is.element("memery", installed.packages()[, 1])) {
  install.packages("memery", dependencies = TRUE)
}
require("memery")

if (!is.element("magick", installed.packages()[, 1])) {
  install.packages("magick", dependencies = TRUE)
}
require("magick")

if (!is.element("yarrr", installed.packages()[, 1])) {
  install.packages("yarrr", dependencies = TRUE)
}
require("yarrr")

if (!is.element("radarchart", installed.packages()[, 1])) {
  install.packages("radarchart", dependencies = TRUE)
}
require("radarchart")

if (!is.element("igraph", installed.packages()[, 1])) {
  install.packages("igraph", dependencies = TRUE)
}
require("igraph")

if (!is.element("wordcloud2", installed.packages()[, 1])) {
  install.packages("wordcloud2", dependencies = TRUE)
}
require("wordcloud2")

if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("readr")

blue_grey_colours_11 <- c("#27408E", "#304FAF", "#536CB5", "#6981c7", "#8da0db",
                          "#dde5ec", "#c8c9ca", "#B9BCC2", "#A7AAAF", "#888A8E",
                          "#636569")

blue_grey_colours_6 <- c("#27408E", "#304FAF", "#536CB5",
                         "#B9BCC2", "#A7AAAF", "#888A8E")

blue_grey_colours_4 <- c("#27408E", "#536CB5",
                         "#B9BCC2", "#888A8E")

blue_grey_colours_2 <- c("#27408E",
                         "#888A8E")

blue_grey_colours_1 <- c("#6981c7")

blue_grey_theme <- function() {
  theme(
    axis.ticks = element_line(
      linewidth = 1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    axis.text = element_text(
      face = "bold", color = "#3f3f41",
      size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", color = "#3f3f41",
                              size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", color = "#3f3f41",
                              size = 16, hjust = 0.5),
    panel.grid = element_line(
      linewidth = 0.1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    panel.background = element_rect(fill = "#f3eeee"),
    legend.title = element_text(face = "plain", color = "#3f3f41",
                                size = 12, hjust = 0),
    legend.position = "right"
  )
}

kable_theme <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

library(readr)
Argos_UK <- read_csv("Github/-Product-and-Service-Discount-Tracker-/data/Argos_UK.csv")

View(Argos_UK)

# Replace missing values with column means
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Dimensions
dim(Argos_UK)

# Data Types
sapply(Argos_UK, class)
glimpse(Argos_UK)

# Summary of each variable
summary(Argos_UK)


library(dplyr)

subset_result <- Argos_UK %>%
  filter(Product.Price > 50) %>%
  select(Column1, Category, Sub.Cateogry, Product.Type, Product.Price)

head(subset_result)


# Function to expand contractions without contractions package
expand_contractions <- function(text) {
  text <- tolower(text)
  text <- gsub("\\bcan't\\b", "cannot", text)
  text <- gsub("\\baren't\\b", "are not", text)
  # Add more rules as needed
  
  return(text)
}

# Apply the function to relevant columns
Argos_UK[qualitative_columns] <- lapply(Argos_UK[qualitative_columns], expand_contractions)

# Function to remove special characters and convert to lowercase
clean_text <- function(text) {
  # Remove special characters
  cleaned_text <- gsub("[^a-zA-Z0-9 ]", "", text)
  
  # Convert to lowercase
  cleaned_text <- tolower(cleaned_text)
  
  return(cleaned_text)
}

# Apply the function to relevant columns
Argos_UK[qualitative_columns] <- lapply(Argos_UK[qualitative_columns], clean_text)
