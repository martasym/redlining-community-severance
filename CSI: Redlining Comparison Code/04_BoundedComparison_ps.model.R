
# The following packages are needed to run this code (only need to install once)
install.packages("MatchIt")  
install.packages("SuperLearner")   
install.packages("tmle")           
install.packages("cobalt")
install.packages("ggthemes")       
install.packages("e1071")
install.packages("gtable")
install.packages("moments")

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

data <- census1940_HOLC_CSI

# 1.  Downloads and prepares the data set. Adds the additional "not_lined" category.
#A) Downloads the data set for analysis. Stores the data as data as an additional safeguard (if also working with sensitivity analysis data).

main_folder <- "/Users/martasymkowick/Finals/"
generated.data.folder <- "CSI/Generated/"
final_sf_data <- readRDS(
  paste0(main_folder, generated.data.folder, "HOLC.CSI.1940Census_dta_us.rds")
)
### data used in paper can be found here: final_sf_data readRDS(paste0(main_folder, "Data Test/", "HOLC.CSI.1940Census_081826_eta011_rank2_us.rds"))
data <- final_sf_data

# B) Select the list of covariates for matching and adjustment
covariates <- c("prop_non_white", "prop_black", "prop_foreign_born_white",
                "prop_employed", "prop_high_school", "prop_homes_major_repairs",
                "prop_homes_radios", "people_per_unit", "population_density_1940",
                "median_home_value")

# C) Selects the 5 HOLC grades
data <- data %>%
  mutate(HOLC_grade = factor(HOLC_grade, levels = c("A", "B", "C", "D", "not_lined")))

# D) Removes units from Tot Pop, allows further analysis. Then plots the results of this to check.
data$population_density_1940 <- as.numeric(data$Tot_Pop)
##### plots population density - to check that the unit conversion works
pop_plot <- ggplot() +
  geom_sf(data = data, aes(fill = population_density_1940)) +
  scale_fill_viridis_c(option = "magma", direction = -1)
pop_plot
rm(pop_plot)

# 2. Creates the not_lined grade category.
### A. Determines the "not_lined" threshold by calculating the 
####  population density distributions across all of the block groups, and the graded block groups.
summary(data$population_density_1940)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#  2.321e-05  0.003291  0.007089  0.009407  0.01245   0.0783
graded_data <- subset(data, !is.na(data$HOLC_grade))
summary(graded_data$population_density_1940)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0008564 0.0038683 0.0070876 0.0084580 0.0121065 0.0241915 
rm(graded_data)

### B. Creates the "not_lined" category based of the selected cut-off.
#### Currently, the median cut-off is selected.
for(i in 1:nrow(data)){
  if(is.na(data$HOLC_grade[i]) && !is.na(data$population_density_1940[i])&&
     data$population_density_1940[i] > 0.0070876){
    data$HOLC_grade[i] <- "not_lined"
  }
}
### C. Plots the "not_lined" selection as a check.
not_lined_plot <- ggplot() +
  geom_sf(data = data, aes(fill = HOLC_grade)) +
  scale_fill_manual(values=c("darkgreen", "lightblue","lightyellow", "darkred", "purple"))
not_lined_plot
rm(not_lined_plot)
# breakdown of the summary of the HOLC grades
summary(data$HOLC_grade)


# 3. Selects the data of the grades used in the binary analysis. 
### A. Selects grades. Manually change grades to conduct analysis on different comparisons.
grade1 <- "A"
grade2 <- "B"

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

### D) Maps checking the overlap of the propensity score density of the control/ treated
####  and the community severance index distribution before matching.
ggplot(subset_data, aes(x = pscore, fill = factor(treatment))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("green", "blue"),
                    labels = c("Control, Grade A", "Treated, Grade B")) +
  labs(x = "Propensity Score", y = "Density", 
       fill = "Group", 
       title = "Propensity Score Distribution Before Matching") +
  theme_minimal()
ggplot(subset_data, aes(x = Com_Severance_Index, fill = factor(treatment))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("green", "blue"),
                    labels = c("Control, Grade A", "Treated, Grade B")) +
  labs(x = "Com_Severance_Index", y = "Density", 
       fill = "Group", 
       title = "Com_Severance_Index Distribution Before Matching") +
  theme_minimal()

# 5. Conducts matching the control/ treatment propensity scores. 
### A) The matchit function essentially calculates the same propensity scores as above
#### and then does nearest neighbor matching on the calculated socres. 
# Note: This is double checked below.
m.out <- matchit(
  as.formula(paste("treatment ~", paste(covariates, collapse = " + "))), 
  data = subset_data,
  method = "nearest",   # nearest-neighbor matching
  distance = "logit",   # match on logit of PS 
  caliper = 0.2,        # smaller caliper = stricter matches
  replace = FALSE       # no reuse of controls 
)
matched_data_cut <- match.data(m.out)

### B) Summarizes the number of block groups in the treated and control grades. 
nrow(subset(matched_data_cut, treatment == 0))
nrow(subset(matched_data_cut, treatment == 1))

### C) Maps checking the overlap of the propensity score density of the control/ treated
####  and the community severance index distribution after matching.
ggplot(matched_data_cut, aes(x = Com_Severance_Index, fill = factor(treatment))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("green", "blue"),
                    labels = c("Control, Grade A", "Treated, Grade B")) +
  labs(x = "CSI Value", y = "Density", 
       fill = "Group", 
       title = "CSI Value Distribution After Matching") +
  theme_minimal()
ggplot(matched_data_cut, aes(x = pscore, fill = factor(treatment))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("green", "blue"),
                    labels = c("Control, Grade A", "Treated, Grade B")) +
  labs(x = "Propensity Score", y = "Density", 
       fill = "Group", 
       title = "Propensity Score Distribution After Matching") +
  theme_minimal()

# 6. Creates the Love Plot for the analysis to check the standardized mean differences
### before and after matching.
library(cobalt)
## checks the balances to see if the absolute standardized mean differences are <0.25
bal <- bal.tab(
  m.out,
  un = TRUE,
  thresholds = c(m = 0.25)
)

bal$Balance

# 7. Applies the linear model and determines analysis results. 
### (Adjusts for any covariates with standardized mean differences >0.25 
### - un-comment to run for each iteration)
lm_cut <- lm(Com_Severance_Index ~ treatment # + prop_non_white + 
             # prop_black + 
             # prop_foreign_born_white + 
             # prop_employed + 
             #prop_high_school + 
             #prop_homes_major_repairs + 
             # prop_homes_radios + 
             #people_per_unit + 
             # population_density_1940 + 
             # median_home_value
             ,
             data = matched_data_cut)


### Summary with treatment and p-value.
summary(lm_cut)
### The confidence intervals for analysis.
confint(lm_cut, level = 0.95)

# 8. Examines the linearity.
### A) starting with a simple t-test
t.test(Com_Severance_Index ~ treatment, data = matched_data_cut)

### B) Now checking a linear model
lm_model <- lm(Com_Severance_Index ~ treatment, data = matched_data_cut)
summary(lm_model)

### C) Graphing the linear model for skews
##### Base R diagnostic plots
par(mfrow = c(2, 2))  # 2x2 layout
plot(lm_model)
par(mfrow = c(1,1))   # reset
resids <- residuals(lm_model)
##### Histogram
hist(resids, breaks = 15, col = "green",
     main = "Histogram of Residuals", xlab = "Residuals")
#### Density plot
plot(density(resids), main = "Density of Residuals",
     xlab = "Residuals", lwd = 2, col = "blue")
#### Skewness
resid_skew <- skewness(resids)
resid_skew  # ~0 means roughly symmetric
#### Shapiro-Wilk test for normality
shapiro.test(resids)




