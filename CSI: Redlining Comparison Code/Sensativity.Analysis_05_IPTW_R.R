library(MatchIt)  
library(SuperLearner)
library(tmle)
library(dplyr)        
library(ggplot2)       
library(cobalt)         
library(ggthemes)
library(patchwork)      
library(e1071)
library(gtable) 
library("moments")
library("sf")

# 1.  Downloads and prepares the data set. Adds the additional "Unlined" category.
### A) Downloads the data set for analysis. 
main_folder <- "/Users/martasymkowick/Finals/"
generated.data.folder <- "CSI/Generated/"
final_sf_data <- readRDS(
  paste0(main_folder, generated.data.folder, "HOLC.CSI.1940Census_dta_us.rds")
)
data <- final_sf_data

### B) Select the list of covariates for matching and adjustment
covariates <- c("prop_non_white", "prop_black", "prop_foreign_born_white",
                "prop_employed", "prop_high_school", "prop_homes_major_repairs",
                "prop_homes_radios", "people_per_unit", "population_density_1940",
                "median_home_value")

### C) Selects the 5 HOLC grades
data <- data %>%
  mutate(HOLC_grade = factor(HOLC_grade, levels = c("A", "B", "C", "D","Unlined")))

### D) Removes units from Tot Pop, allows further analysis. 
data$population_density_1940 <- as.numeric(data$Tot_Pop)
### E) plots population density - to check that the unit conversion works.
pop_plot <- ggplot() +
  geom_sf(data = data, aes(fill = population_density_1940)) +
  labs(title = "Population Density in San Francisco in 1940", subtitle = "(Persons Per Square Meter)", fill = "Density") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  scale_fill_viridis_c(option = "magma", direction = -1)
pop_plot
rm(pop_plot)



# 2. Creates the Unlined grade category.
### A. Determines the "Unlined" threshold by calculating the 
####  population density distributions across all of the block groups, and the graded block groups.
summary(data$population_density_1940)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#  2.321e-05  0.003291  0.007089  0.009407  0.01245   0.0783
graded_data <- subset(data, !is.na(data$HOLC_grade))
summary(graded_data$population_density_1940)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0008564 0.0038683 0.0070876 0.0084580 0.0121065 0.0241915 
rm(graded_data)

### B. Creates the "Unlined" category based of the selected cut-off.
#### Currently, the median cut-off is selected.
for(i in 1:nrow(data)){
  if(is.na(data$HOLC_grade[i]) && !is.na(data$population_density_1940[i])&&
     data$population_density_1940[i] > 0.0070876){
    data$HOLC_grade[i] <- "Unlined"
  }
}
### C. Plots the "Unlined" selection as a check.
unlined_plot <- ggplot() +
  geom_sf(data = data, aes(fill = HOLC_grade)) +
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred", "purple"))
unlined_plot
rm(unlined_plot)
#### breakdown of the summary of the HOLC grades
summary(data$HOLC_grade)



# 3. Selects the data of the grades used in the binary analysis. 
### A. Selects grades. Manually change grades to conduct analysis on different comparisons.
grade1 <- "D"
grade2 <- "Unlined"

### B. Filters dataset to include only the two selected grades.
subset_data <- data %>%
  filter(HOLC_grade %in% c(grade1, grade2)) %>%
  mutate(treatment = ifelse(HOLC_grade == grade2, 1, 0)) # Set grade2 as "treated" group

### C. Remove geometry and units from all covariates
subset_data <- st_drop_geometry(subset_data)
library(units)
subset_data[] <- lapply(subset_data, function(x) {
  if (inherits(x, "units")) drop_units(x) else x
})



# 4. Calculates the propensity scores.
### A) Uses the glm function to conduct logistic regression. 
ps_model <- glm(
  treatment ~ ., 
  data = subset_data[, c("treatment", covariates)], 
  family = binomial
)
### B) Converts the logistic regression into the propensity score
subset_data$pscore <- predict(ps_model, type = "response")
### C) Demonstrates the count of treated and not treated block groups.
nrow(subset(subset_data, treatment == 0))
nrow(subset(subset_data, treatment == 1))




# 5. IPTW Analysis (no truncation)
### A) Sets up the IPTW analysis through calculating weights.
subset_data$IPTW <- ifelse(
  subset_data$treatment == 1,
  1 / subset_data$pscore,        # treated
  1 / (1 - subset_data$pscore)   # control
)
### B) Applies a linear model to the weighted data.
iptw_model <- lm(Com_Severance_Index ~ treatment, data = subset_data, weights = IPTW)
### C) Summarizes the results and confidence intervals.
summary(iptw_model)
print (confint(iptw_model))



# 6. IPTW Analysis with Truncation
### A) Sets truncation boundaries.
lower_bound <- quantile(subset_data$IPTW, 0.02)  # 2nd percentile
upper_bound <- quantile(subset_data$IPTW, 0.98)  # 98th percentile
### B) Apply truncation to the weights.
subset_data$IPTW_trunc <- pmin(pmax(subset_data$IPTW, lower_bound), upper_bound)
### C) Summarizes the truncated weights.
summary(subset_data$IPTW_trunc)
### D) Refits the model with truncated weights, summarize, and determine the confidence interval.
iptw_model_trunc <- lm(Com_Severance_Index ~ treatment, data = subset_data, weights = IPTW_trunc)
summary(iptw_model_trunc)
print (confint(iptw_model_trunc))

library(cobalt)

bal <- bal.tab(
  as.formula(
    paste("treatment ~", paste(covariates, collapse = " + "))
  ),
  data = subset_data,
  weights = subset_data$IPTW_trunc,
  method = "weighting",
  un = TRUE,
  thresholds = c(m = 0.25)
)

bal$Balance


# 7. Histogram of IPTW weights (truncated and not truncated)
hist(subset_data$IPTW, breaks = 30, col = "lightblue",
     main = "Histogram of IPTW", xlab = "IPTW")
hist(subset_data$IPTW_trunc, breaks = 30, col = "lightblue",
     main = "Histogram of IPTW (Adjusted)", xlab = "IPTW")



# 8. Further checks.
### A) Graphing the linear model for skews
##### Base R diagnostic plots
par(mfrow = c(2, 2))  # 2x2 layout
plot(iptw_model_trunc)
par(mfrow = c(1,1))   # reset
resids <- residuals(iptw_model_trunc)
##### Histogram
hist(resids, breaks = 15, col = "green",
     main = "Histogram of Residuals", xlab = "Residuals")
##### Density plot
plot(density(resids), main = "Density of Residuals",
     xlab = "Residuals", lwd = 2, col = "blue")
##### Skewness
resid_skew <- skewness(resids)
resid_skew  # ~0 means roughly symmetric
##### Shapiro-Wilk test for normality
shapiro.test(resids)


