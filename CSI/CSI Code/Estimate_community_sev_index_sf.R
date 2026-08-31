# script aim: put together inputs to run community severance index over SF
# First step to load packages etc.

# # load packages
library(PCPhelpers) # this one you need to download from github https://github.com/Columbia-PRIME/PCPhelpers
library(gridExtra)
library(pcpr) # this one you need to download from github https://github.com/Columbia-PRIME/pcpr
library(foreach)
library(tictoc) # for timing
library(plotly)    # for visualizing the gridsearches
library(progressr) # needed for progress bars with the new PCP gridsearch
library(tidyverse)
library(future)
library(furr)
library(future.apply)
library(parallelly)
## read data

# Sets variable names.
crs <- 2163
name_short <- "sf"

# Sets the pathways to the different folders.
# Main folder can be changed depending on where the finals folder is on computer.
main.folder <- "/Users/martasymkowick/CommunitySeverance/code_MS/Finals/"
generated.folder.CSI <- "CSI/Generated/"

sld_us <- sf::read_sf(paste0(main.folder, "pulledData/","SmartLocationDatabaseV3/", "SmartLocationDatabase.gdb"))

#Selection of urban spatial variables, details in table 1 of manuscript (Jaime, you can add more details or delete if it's incorrect)
var_name <- c("GEOID20", "STATEFP", "CBSA_Name", "COUNTYFP", "TRACTCE", "BLKGRPCE",
              "Ac_Total", "Ac_Unpr", 
              "TotPop", "CountHU", "HH", "P_WrkAge", 
              "AutoOwn0", "AutoOwn1", "AutoOwn2p", 
              "Workers", "R_LowWageWk", "R_MedWageWk", "R_HiWageWk",
              "D1A", "D1B", "D1C",
              "D2B_E8MIXA", "D2A_EPHHM", 
              "D3A", "D3AAO", "D3AMM", "D3APO",
              "D3B", "D3BAO", "D3BMM3", "D3BMM4", "D3BPO3", "D3BPO4",
              "D4A",
              "D5AR", "D5AE", "D5BR", "D5BE",
              "D2A_Ranked", "D2B_Ranked", "D3B_Ranked", "D4A_Ranked", "NatWalkInd")

desc <- c("Census block group 12-digit FIPS code (2018)", "State FIPS code", "CBSA name", "County FIPS code", "Census tract FIPS code in which CBG resides", "Census block group FIPS code in which CBG resides",
          "Total geometric area (acres) of the CBG", "Total land area (acres) that is not protected from development (i.e., not a park, natural area or conservation area)",
          "Population, 2018", "Housing units, 2018", "Households (occupied housing units), 2018", "Percent of population that is working aged 18 to 64 years, 2018",
          "Number of households in CBG that own zero automobiles, 2018", "Number of households in CBG that own one automobile, 2018", "Number of households in CBG that own two or more automobiles, 2018",
          "Count of workers in CBG (home location), 2017", "Count of workers earning $1250/month or less (home location), 2017", "Count of workers earning more than $1250/month but less than $3333/month (home location), 2017", "Count of workers earning $3333/month or more (home location), 2017",
          "Gross residential density (HU/acre) on unprotected land", "Gross population density (people/acre) on unprotected land", "Gross employment density (jobs/acre) on unprotected land", 
          "8-tier employment entropy (denominator set to the static 8 employment types in the CBG)", "Employment and household entropy", 
          "Total road network density", "Network density in terms of facility miles of auto-oriented links per square mile", "Network density in terms of facility miles of multi-modal links per square mile", "Network density in terms of facility miles of pedestrian-oriented links per square mile",
          "Street intersection density (weighted, auto-oriented intersections eliminated)", "Intersection density in terms of auto-oriented intersections per square mile", "Intersection density in terms of multi-modal intersections having three legs per square mile", "Intersection density in terms of multi-modal intersections having four or more legs per square mile", "Intersection density in terms of pedestrian-oriented intersections having three legs per square mile", "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile",
          "Distance from the population-weighted centroid to nearest transit stop (meters)",
          "Jobs within 45 minutes auto travel time, time- decay (network travel time) weighted)", "Working age population within 45 minutes auto travel time, time-decay (network travel time) weighted", "Jobs within 45-minute transit commute, distance decay (walk network travel time, GTFS schedules) weighted", "Working age population within 45-minute transit commute, time decay (walk network travel time, GTFS schedules) weighted",
          "Quantile ranked order (1-20) of [D2a_EpHHm] from lowest to highest", "Quantile ranked order (1-20) of [D2b_E8MixA] from lowest to highest", "Quantile ranked order (1-20) of [D3b] from lowest to highest", "Quantile ranked order (1,13-20) of [D4a] from lowest to highest", "Walkability Index")

source <- c("2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line",
            "sld", "sld",  # if source is not unique, write sld for smart location database, where source info can be found
            "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)",
            "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)",
            "2017 Census LEHD RAC", "2017 Census LEHD RAC", "2017 Census LEHD RAC", "2017 Census LEHD RAC",
            "sld", "sld", "sld", 
            "sld", "sld", 
            "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS",
            "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS",
            "2020 GTFS, 2020 CTOD",
            "2020 TravelTime API, 2017 Census LEHD", "2020 TravelTime API, 2018 Census ACS", "2020 TravelTime API, 2017 Census LEHD, 2020 GTFS", "2020 TravelTime API, 2018 Census ACS, 2020 GTFS",
            "sld", "sld", "sld", "sld", "sld")

# MS - This is just double checking to ensure that they match/ fit together
all(var_name %in% colnames(sld_us))
length(var_name) == length(desc) 
length(var_name)== length(source)

# MS - This is a data set of all of the var names, their description, and the source of the var names/ desciption)
data_desc <- data.frame(var_name = var_name, description = desc, source = source)

dta_cs_in <- readRDS(paste0(main.folder, generated.folder.CSI, "community_severance_sf_input_data.rds"))

# Used in Previous Index
  # dta_cs_in <- readRDS(paste0(main.folder, "Data Test/", "community_severance_sf_081826__input_data.rds"))

built_social_block_comm_sev_m <-  as.matrix(dta_cs_in[,-1])

### estimate community severance index
c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
  "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
  "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")
family_vars <- c("Road infrastructure", "Road infrastructure", "Road infrastructure", 
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure",
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road traffic activity", "Road traffic activity", 
                 "Road traffic activity", "Pedestrian infrastructure", "Pedestrian infrastructure", "Pedestrian infrastructure" )
cng <- data.frame(vars = colnames(built_social_block_comm_sev_m), family_vars = family_vars)
cng_comm_sev_vars <- cng


## run pcp grid search
# including also rows with some na
geoids <- dta_cs_in[,
                    "GEOID20"]
dat <- built_social_block_comm_sev_m
data <- list("M" = dat) %>% purrr::map(as.matrix)
# second vanilla search
# previous search was for 0.10 through 0.11 

### These are the results from the CSI grid search.

# Search eta: 0.01 - 0.07 with rank 6 ->  eta =  0.07, r = 2, rel_err= 0.184,  S_sparsity= 98.3  
  # Variance was 66% for that input (eta =  0.07, r = 2) 
# Search eta: 0.07 - 0.10 with rank 6 ->     eta =  0.1, r = 3, rel_err= 0.178,  S_sparsity= 99.3 
  # Variance was 49% for that input (eta =  0.1, r = 3)
# Search eta: 0.10 - 0.15 with rank 6 ->     eta =  0.15, r = 3, rel_err= 0.173,  S_sparsity= 99.9 
  # Greatest variance was 43% for that input (eta =  0.15, r = 3)
# Search eta: 0.10 - 0.15 with rank 2 ->     eta = 0.14, r = 2, rel_err= 0.180, S_sparsity= 100 
  # Variance was 64% for eta = 0.14, r = 2
  # Greatest variance was 66% for that input (eta =  0.11, r = 2, rel_err = 0.181, S_sparsity= 99.9)
# Search eta: 0.01 - 0.10 with rank 2 ->     eta =  0.1, r = 2, rel_err= 0.182,  S_sparsity= 99.7 
  # Greatest variance was 66% for that input eta =  (0.1, r = 2)

etas <- seq(0.10,0.11, length.out=11)
rank <- 2
rrmc_grid <- expand.grid(eta = etas, r = rank) # RRMC will search up to rank 2
runs = 22
LOD = rep(0, ncol(data$M))
perc_test = 0.15
cores = parallel::detectCores(logical = F) /2
# 3b. Run gridsearch:
with_progress(expr = {
  rrmc_results <- vanilla_search(
    cores = cores,
    mat = data$M, 
    pcp_func = rrmc, 
    grid = rrmc_grid,
    LOD = LOD,
    perc_test = perc_test,
    runs = runs,
    save_as = paste0(main.folder, generated.folder.CSI,"GridSearchResults/", "rrmc_vanilla_results_community_severance")
  )
})
# # read results
rrmc_results <- readRDS(paste0(main.folder, generated.folder.CSI,"GridSearchResults/", "rrmc_vanilla_results_community_severance", ".rds"))
# # \mk/
# # # 
# # # # 3c. The best parameter setting according to relative error...

rrmc_results$summary_stats %>% slice_min(rel_err)

rrmc_results$summary_stats %>%
  filter(r == 2) %>%
  arrange(rel_err) %>%
  slice(1)

rrmc_results$summary_stats %>%
  filter(abs(eta - 0.11) < 1e-8)

rrmc_results$summary_stats %>%
  dplyr::arrange(rel_err) %>%
  print(n = Inf)


# # # # 3d. Visualizing the whole gridsearch:
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")

output.folder <- generated.folder.CSI

# As used in the paper CSI index.
# run pcp for optimal result
# for 66% run with r = 2, eta = 0.11
#    eta     r rel_err L_rank S_sparsity iterations run_error_perc
#   <dbl> <int>   <dbl>  <dbl>      <dbl>      <dbl> <chr>         
#   0.16     2   0.180      2       100.        NaN 0%  

pcp_outs <- rrmc(data$M, r = 2, eta = 0.11, LOD = LOD) 
# % below 0 in sparsity matrix
sum(pcp_outs$L<0)/prod(dim(pcp_outs$L)) # 6 % below 0 in L matrix
sum(pcp_outs$L<(-1/2))/prod(dim(pcp_outs$L)) # 0% below -1/2
# save pcp result
saveRDS(pcp_outs, file = paste0(main.folder, generated.folder.CSI, "pcp_rrmc_081816", name_short, ".rds"))
pcp_outs <- readRDS(file = paste0(main.folder, generated.folder.CSI, "pcp_rrmc_081816", name_short, ".rds"))


# run factor analysis on low rank matrix
cn <- colnames(pcp_outs$S)
data_desc <- data_desc[which(data_desc$var_name %in% cn),]

#re-order columns in low-rank matrix
cng <- data_desc[,c("var_name", "source")]
cng <- cng[which(cng$var_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)
pcp_outs$L <- pcp_outs$L[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

#re-order columns in sparsity matrix
pcp_outs$S <- pcp_outs$S[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

# manuscript Figure 3b
# L matrix correlations:
graph_title <- paste0("pcp_rrmc", ": L Pearson correlation")
png(paste0(main.folder, output.folder, "pcp_rrmc", "_l_matrix_correlations", name_short, ".png"), 900, 460)
pcp_outs$L %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# raw data matrix correlations
data$M <- data$M[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                    "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                    "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

# manuscript Figure 3a
graph_title <- paste0("raw_data", ": Pearson correlation")
png(paste0(main.folder, output.folder, "raw_mat_corr_", name_short, ".png"), 900, 460)
data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                          label = T, label_size = 3, label_alpha = T,
                          hjust = 1, nbreaks = 10, limits = TRUE,
                          size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

#install.packages("ggfortify")
library(ggfortify)

output.folder <- generated.folder.CSI
# factor analysis
ranktol <- 1e-04
L.rank <- Matrix::rankMatrix(pcp_outs$L, tol = ranktol)
scale_flag <- FALSE
pcs <- paste0("PC", 1:L.rank)
factors <- 1:L.rank
n <- nrow(pcp_outs$L)
colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                          family = data_desc[match(colnames(pcp_outs$L), data_desc$var_name), "source"])
colgroups_l$family <- family_vars
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = data_desc[match(colnames(data$M), data_desc$var_name), "source"])
colgroups_m$family <- family_vars
L.eda <-PCPhelpers::eda(pcp_outs$L, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_l, rowgroups = NULL)

#install.packages("psych")
library(psych)

# run factor analysis
orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))
# explore results
orthos %>% walk(print, digits = 2, sort = T)
ortho_ebics <- orthos %>% map_dbl(~.$EBIC)
best_fit <- which.min(ortho_ebics)
# visualize in table
library(kableExtra)
data.frame("Factors" = factors, "EBIC" = ortho_ebics) %>% kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>%
  row_spec(best_fit, bold = T, color = "white", background = "#D7261E")

fa_model <- orthos[[best_fit]]

print(fa_model, digits = 2)

# organize loadings
loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"
loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)
loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5
# loadings table
# manuscript Table S1
loadings %>% kbl(caption = "Loadings") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px") 
# organize scores
scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)
scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]
# scores table
scores %>% kbl(caption = "Scores") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px")

# prepare loadings for plotting
fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()
# build dataframe for plotting
dat <- cbind(colgroups_l, fa_pats)


p <- 1 # 1 is for community severance index (manuscript Figure 4) and 2 for the other pattern (manuscript Figure S1)
# Plot loadings
loading_plot <- print_patterns(
  dat[, c("MR1", "MR2")],
  colgroups = dat[, c("column_names", "family")],
  pat_type = "factor",
  n = p,
  title = "FA factors"
) +
  # Redraw points larger
  geom_point(size = 2.2) +
  
  # Redraw loading lines thicker
  geom_segment(aes(yend = 0, xend = chem), linewidth = 0.8) +
  
  # Make zero-reference line more visible
  geom_hline(yintercept = 0, linewidth = 0.5) +
  
  # Increase text, axis, and legend sizes
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(size = 9.5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(0.12, "cm"),
    legend.text = element_text(size = 9.5),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 11, face = "plain"),
    plot.title = element_text(size = 13, face = "plain"),
    panel.border = element_rect(linewidth = 0.5)
  )

loading_plot

# Save high-resolution figure
png(paste0(main.folder, output.folder, "_l_fa_c", p, "_patterns_", name_short, ".png"),
    width = 1500, height = 800, res = 150)

print(loading_plot)

dev.off()

# save normalized scores
dat_scores <- cbind(built_social_block_comm_sev_m, scores)
dat_scores$GEOID20 <- geoids
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
dat_scores$MR1_norm <- normalize(dat_scores$MR1)

saveRDS(dat_scores, paste0(main.folder, generated.folder.CSI, "comm_sev_fa_scores_SF_", name_short, "_dta_us.rds"))


