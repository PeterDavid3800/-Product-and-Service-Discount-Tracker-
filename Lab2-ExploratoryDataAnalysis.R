if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

.libPaths()

lapply(.libPaths(), list.files)


if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")


library(readr)
Argos_UK <- read_csv("Github/-Product-and-Service-Discount-Tracker-/data/Argos_UK.csv")
View(Argos_UK)

dim(Argos_UK)

sapply(Argos_UK, class)

Argos_UK_freq <- Argos_UK$`Product Price`
cbind(frequency = table(Argos_UK_freq),
      percentage = prop.table(table(Argos_UK_freq)) *100)

Argos_UK_freq <- Argos_UK$`Product Line Item`
cbind(frequency = table(Argos_UK_freq),
      percentage = prop.table(table(Argos_UK_freq)) *100)

Argos_UK_freq <- Argos_UK$`Product Type`
cbind(frequency = table(Argos_UK_freq),
      percentage = prop.table(table(Argos_UK_freq)) *100)

Argos_UK_ProductPrice_mode <- names(table(Argos_UK$`Product Price`))[
  which(table(Argos_UK$`Product Price`) == max(table(Argos_UK$`Product Price`)))]
print(Argos_UK_ProductPrice_mode)

Argos_UK_ProductLineItem_mode <- names(table(Argos_UK$`Product Line Item`))[
  which(table(Argos_UK$`Product Line Item`) == max(table(Argos_UK$`Product Line Item`)))]
print(Argos_UK_ProductLineItem_mode)

Argos_UK_ProductType_mode <- names(table(Argos_UK$`Product Type`))[
  which(table(Argos_UK$`Product Type`) == max(table(Argos_UK$`Product Type`)))]

print(Argos_UK_ProductType_mode)


summary(Argos_UK)

sapply(Argos_UK[, c(7)], sd)


sapply(Argos_UK[, c(7)], var)

sapply(Argos_UK [c(7)],kurtosis, type=2)


sapply(Argos_UK [c(7)],skewness, type=2)

Argos_UK_cov <- cov(Argos_UK [c(7)])
View(Argos_UK_cov)

Argos_UK_cor <- cor(Argos_UK [c(7)])
View(Argos_UK_cor)

Argos_UK_one_way_anova <- aov( `Product Price` ~ `Product Type`, data = Argos_UK)
summary(Argos_UK_one_way_anova)

Argos_UK_two_way_anova <- aov( `Product Price` ~ `Product Type`* `Product Line Item`, data = Argos_UK)
summary(Argos_UK_two_way_anova)

par(mfrow = c(7))
for (i in 1:3) {
  barplot(table(Argos_UK[, i]), main = names(Argos_UK)[i])
}

if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")

missmap(Argos_UK, col = c("red", "grey"), legend = TRUE)


{plot.new(); dev.off()}

if (!is.element("ggcorrplot", installed.packages()[, 1])) {
  install.packages("ggcorrplot", dependencies = TRUE)
}
require("ggcorrplot")
corrplot(cor(Argos_UK[, c(7)]), method = "circle")


featurePlot(x = Argos_UK[, c(7)], y = X20230412_20230719_BI1_BBIT4_1_StudentPerformanceDataset[, 100], plot = "box")