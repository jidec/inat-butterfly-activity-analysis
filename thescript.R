# Load and filter raw observations data
library(dplyr)
library(ggplot2)
library(tidyr)
library(mgcv)
library(purrr)

# Load data
obs_data <- readRDS("_targets/objects/obs_gridded")
obs_data <- subset(obs_data, select = -c(geometry))
obs_data <- filter(obs_data, local_hour >= 7 & local_hour <= 20)

# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Bootstrap function
my_bootstrap <- function(hours, inv_efforts, nbootstraps=50){
  bootstrap_medians <- numeric(nbootstraps)
  
  for (i in 1:nbootstraps) {
    sampled_hours <- sample(hours, replace=TRUE, size=length(hours), prob=inv_efforts)
    bootstrap_medians[i] <- median(sampled_hours)
  }
  
  median_lower <- quantile(bootstrap_medians, probs = 0.05)
  median_value <- median(bootstrap_medians)
  median_upper <- quantile(bootstrap_medians, probs = 0.95)
  
  return(list(median_lower = median_lower, 
              median_value = median_value, 
              median_upper = median_upper))
}

# STEP 1: Calculate sampling effort for ALL cell-season combinations
cat("Calculating sampling effort for all cell-season combinations...\n")

sampling_effort <- obs_data %>%
  group_by(season, cell, local_hour) %>%
  summarise(n_obs = n(), .groups = 'drop')

# Get unique cell-season combinations
cell_season_combos <- sampling_effort %>%
  distinct(season, cell)

cat("Found", nrow(cell_season_combos), "unique cell-season combinations\n")

# STEP 2: Fit GAMs and predict effort for each cell-season
# We'll do this in chunks to manage memory
effort_predictions_list <- list()
chunk_size <- 10

for(i in seq(1, nrow(cell_season_combos), by = chunk_size)) {
  end_idx <- min(i + chunk_size - 1, nrow(cell_season_combos))
  cat("Processing cell-season combinations", i, "to", end_idx, "\n")
  
  chunk_combos <- cell_season_combos[i:end_idx,]
  
  chunk_predictions <- list()
  
  for(j in 1:nrow(chunk_combos)) {
    combo <- chunk_combos[j,]
    
    cell_season_data <- sampling_effort %>%
      filter(season == combo$season, cell == combo$cell)
    
    if(nrow(cell_season_data) > 3) {
      tryCatch({
        gam_model <- mgcv::gam(n_obs ~ s(local_hour), 
                               data = cell_season_data, 
                               family = "poisson")
        
        # Predict for all hours in this cell-season
        predictions <- data.frame(
          season = combo$season,
          cell = combo$cell,
          local_hour = cell_season_data$local_hour,
          effort = predict(gam_model, 
                           newdata = data.frame(local_hour = cell_season_data$local_hour), 
                           type = "response")
        )
        
        chunk_predictions[[length(chunk_predictions) + 1]] <- predictions
        
      }, error = function(e) {
        # If GAM fails, use raw counts
        chunk_predictions[[length(chunk_predictions) + 1]] <- cell_season_data %>%
          mutate(effort = n_obs) %>%
          dplyr::select(season, cell, local_hour, effort)
      })
    } else {
      # Not enough data for GAM, use raw counts
      chunk_predictions[[length(chunk_predictions) + 1]] <- cell_season_data %>%
        mutate(effort = n_obs) %>%
        dplyr::select(season, cell, local_hour, effort)
    }
  }
  
  effort_predictions_list[[length(effort_predictions_list) + 1]] <- bind_rows(chunk_predictions)
  
  # Clean up memory
  rm(chunk_predictions)
  if(i %% 50 == 0) gc()
}

# Combine all effort predictions
all_effort_predictions <- bind_rows(effort_predictions_list)
#rm(effort_predictions_list, sampling_effort)
#gc()

cat("Effort predictions calculated for", nrow(all_effort_predictions), "cell-season-hour combinations\n")

# STEP 3: Find the most well-sampled species-season-cell combinations
min_per_ssc <- 30
top_n_to_analyze <- 30  # Analyze top 30 combinations

top_combinations <- obs_data %>%
  group_by(species, season, cell) %>%
  summarise(n_obs = n(), .groups = 'drop') %>%
  filter(n_obs >= min_per_ssc) %>%
  arrange(desc(n_obs)) %>%
  head(top_n_to_analyze)

cat("\nAnalyzing top", nrow(top_combinations), "species-season-cell combinations\n")
print(head(top_combinations, 10))

# STEP 4: Process each species-season-cell combination with effort weights
results_list <- list()

for(i in 1:nrow(top_combinations)) {
  combo <- top_combinations[i,]
  
  if(i %% 5 == 0) {
    cat("\nProcessing", i, "of", nrow(top_combinations), ":",
        combo$species, "-", combo$season, "- Cell", combo$cell, "\n")
  }
  
  # Get data for just this species-season-cell combination
  combo_data <- obs_data %>%
    filter(species == combo$species, 
           season == combo$season, 
           cell == combo$cell)
  
  # Merge with effort predictions
  combo_data <- combo_data %>%
    left_join(all_effort_predictions, 
              by = c("season", "cell", "local_hour")) %>%
    filter(!is.na(effort)) %>%
    mutate(inverse_effort = 1/effort)
  
  if(nrow(combo_data) == 0) {
    cat("No effort data for", combo$species, combo$season, combo$cell, "\n")
    next
  }
  
  # Calculate metrics
  unadj_median <- median(combo_data$local_hour)
  
  # Bootstrap
  boot_results <- my_bootstrap(combo_data$local_hour, 
                               combo_data$inverse_effort)
  
  # Store results
  results_list[[length(results_list) + 1]] <- data.frame(
    species = combo$species,
    season = combo$season,
    cell = combo$cell,
    n_obs = combo$n_obs,
    unadj_median = unadj_median,
    adj_median = boot_results$median_value,
    adj_median_lower = boot_results$median_lower,
    adj_median_upper = boot_results$median_upper,
    stringsAsFactors = FALSE
  )
  
  # Clean up memory after each iteration
  rm(combo_data)
  if(i %% 10 == 0) gc()
}

# Combine results
bootstrap_results <- bind_rows(results_list)
#rm(results_list)
#gc()

# Display results
cat("\n\nBootstrap results:\n")
print(bootstrap_results)

# STEP 5: Create visualization comparing raw vs adjusted
p1 <- bootstrap_results %>%
  head(20) %>%  # Show top 20 for readability
  mutate(combo_label = paste(substr(species, 1, 15), season, cell, sep="\n")) %>%
  ggplot(aes(x = reorder(combo_label, unadj_median))) +
  geom_point(aes(y = unadj_median), color = "blue", size = 2) +
  geom_point(aes(y = adj_median), color = "red", size = 2) +
  geom_errorbar(aes(ymin = adj_median_lower, ymax = adj_median_upper), 
                color = "red", width = 0.3, alpha = 0.5) +
  coord_flip() +
  labs(title = "Top 20 Species-Season-Cell Combinations: Peak Activity Times",
       subtitle = "Blue = Raw median, Red = Effort-adjusted median with 90% CI",
       x = "Species / Season / Cell",
       y = "Peak Activity Hour") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(7, 20, 1))

print(p1)

# STEP 6: Show the adjustment effect
bootstrap_results <- bootstrap_results %>%
  mutate(adjustment = adj_median - unadj_median)

p2 <- bootstrap_results %>%
  ggplot(aes(x = n_obs, y = adjustment)) +
  geom_point(aes(color = season), size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Effect of Effort Adjustment on Peak Activity Time",
       subtitle = "Positive = adjusted peak is later, Negative = adjusted peak is earlier",
       x = "Number of Observations",
       y = "Adjustment (hours)") +
  theme_minimal() +
  scale_color_viridis_d()

print(p2)

# STEP 7: Summary by season
season_summary <- bootstrap_results %>%
  group_by(season) %>%
  summarise(
    mean_adjustment = mean(adjustment),
    sd_adjustment = sd(adjustment),
    n = n()
  )

cat("\n\nSeason summary of adjustments:\n")
print(season_summary)

# STEP 8: Create detailed plot for ONE well-sampled example
top_combo <- bootstrap_results[1,]

cat("\n\nCreating detailed plot for:", top_combo$species, 
    "in", top_combo$season, "at cell", top_combo$cell, "\n")

# Get just this combination's data with effort
example_data <- obs_data %>%
  filter(species == top_combo$species,
         season == top_combo$season,
         cell == top_combo$cell) %>%
  left_join(all_effort_predictions, 
            by = c("season", "cell", "local_hour")) %>%
  filter(!is.na(effort)) %>%
  mutate(inverse_effort = 1/effort)

# Create detailed comparison plot
p3 <- ggplot() +
  # Raw histogram
  geom_histogram(data = example_data, 
                 aes(x = local_hour, y = after_stat(density)), 
                 bins = 14, fill = "lightblue", alpha = 0.7) +
  # Weighted histogram
  geom_histogram(data = example_data,
                 aes(x = local_hour, weight = inverse_effort, y = after_stat(density)),
                 bins = 14, fill = "coral", alpha = 0.5) +
  # Add vertical lines for medians
  geom_vline(xintercept = top_combo$unadj_median, 
             color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = top_combo$adj_median, 
             color = "red", linetype = "solid", size = 1) +
  labs(title = paste("Detailed view:", top_combo$species),
       subtitle = paste(top_combo$season, "at cell", top_combo$cell, 
                        "(n =", top_combo$n_obs, "observations)"),
       x = "Hour of day", 
       y = "Density",
       caption = "Light blue = raw data, Coral = effort-adjusted\nDashed blue = raw median, Solid red = adjusted median") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7, 20, 1))

print(p3)

# Clean up
#rm(obs_data, all_effort_predictions)
#gc()

cat("\n\nAnalysis complete! Memory usage should be much lower.\n")
cat("Processed", nrow(bootstrap_results), "species-season-cell combinations\n")










# STEP 8: Create detailed plot for ONE well-sampled species cell across all seasons
top_species_cell <- bootstrap_results %>%
  count(species, cell, sort = TRUE) %>%
  slice(4) %>%
  dplyr::select(species, cell)

cat("\n\nCreating detailed plot for species:", top_species_cell$species, 
    "at cell", top_species_cell$cell, "across all seasons\n")

# Get all season data for this species-cell
example_data <- obs_data %>%
  filter(species == top_species_cell$species,
         cell == top_species_cell$cell) %>%
  left_join(all_effort_predictions, 
            by = c("season", "cell", "local_hour")) %>%
  filter(!is.na(effort)) %>%
  mutate(inverse_effort = 1/effort)

# Compute density curves (raw + adjusted) for each season
curves <- example_data %>%
  group_by(season) %>%
  nest() %>%
  mutate(
    raw_density = map(data, ~{
      dens <- density(.x$local_hour, from = 7, to = 20)
      tibble(local_hour = dens$x, density = dens$y, type = "Raw")
    }),
    adj_density = map(data, ~{
      dens <- density(.x$local_hour, from = 7, to = 20,
                      weights = .x$inverse_effort/sum(.x$inverse_effort))
      tibble(local_hour = dens$x, density = dens$y, type = "Adjusted")
    })
  ) %>%
  dplyr::select(season, raw_density, adj_density) %>%
  pivot_longer(cols = c(raw_density, adj_density),
               names_to = "curve_type",
               values_to = "densities") %>%
  unnest(densities)

# Plot as 2x4 grid: rows = type (Raw vs Adjusted), cols = season
p3 <- ggplot(curves, aes(x = local_hour, y = density, color = season)) +
  geom_line(size = 1) +
  facet_grid(type ~ season, scales = "free_y") +
  labs(title = paste("Density curves for", top_species_cell$species,
                     "at cell", top_species_cell$cell),
       x = "Hour of day", y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7, 20, 1))

print(p3)
















# STEP 8: Create detailed plot for ONE well-sampled species cell across all seasons
top_species_cell <- bootstrap_results %>%
  count(species, cell, sort = TRUE) %>%
  slice(4) %>%
  dplyr::select(species, cell)

cat("\n\nCreating detailed plot for species:", top_species_cell$species, 
    "at cell", top_species_cell$cell, "across all seasons\n")

# Get all season data for this species-cell
example_data <- obs_data %>%
  filter(species == top_species_cell$species,
         cell == top_species_cell$cell) %>%
  left_join(all_effort_predictions, 
            by = c("season", "cell", "local_hour")) %>%
  filter(!is.na(effort)) %>%
  mutate(inverse_effort = 1/effort)

# Bin hours and calculate counts (raw and adjusted) for each season
curves <- example_data %>%
  mutate(hour_bin = floor(local_hour)) %>%  # bin by hour
  group_by(season, hour_bin) %>%
  summarise(
    raw_count = n(),
    adj_count = sum(inverse_effort),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(raw_count, adj_count),
               names_to = "type", values_to = "count") %>%
  mutate(type = recode(type, raw_count = "Raw", adj_count = "Adjusted"))

# Plot as 2x4 grid: rows = type (Raw vs Adjusted), cols = season
p3 <- ggplot(curves, aes(x = hour_bin, y = count, fill = season)) +
  geom_col(position = "dodge") +
  facet_grid(type ~ season, scales = "free_y") +
  labs(title = paste("Hourly counts for", top_species_cell$species,
                     "at cell", top_species_cell$cell),
       x = "Hour of day", y = "Number of observations") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7, 20, 1))

print(p3)








# STEP 8: Create detailed plot for ONE well-sampled species cell across all seasons
top_species_cell <- bootstrap_results %>%
  count(species, cell, sort = TRUE) %>%
  slice(5) %>%
  dplyr::select(species, cell)

cat("\n\nCreating detailed plot for species:", top_species_cell$species, 
    "at cell", top_species_cell$cell, "across all seasons\n")

# Get all season data for this species-cell
example_data <- obs_data %>%
  filter(species == top_species_cell$species,
         cell == top_species_cell$cell) %>%
  left_join(all_effort_predictions, 
            by = c("season", "cell", "local_hour")) %>%
  filter(!is.na(effort)) %>%
  mutate(inverse_effort = 1/effort)

# Bin hours and calculate counts (raw and adjusted) for each season
curves <- example_data %>%
  mutate(hour_bin = floor(local_hour)) %>%
  group_by(season, hour_bin) %>%
  summarise(
    raw_count = n(),
    adj_weighted = sum(inverse_effort),
    .groups = "drop"
  ) %>%
  group_by(season) %>%
  mutate(
    # scale adjusted so totals match raw totals within each season
    adj_count = adj_weighted * sum(raw_count) / sum(adj_weighted)
  ) %>%
  dplyr::select(season, hour_bin, raw_count, adj_count) %>%
  pivot_longer(cols = c(raw_count, adj_count),
               names_to = "type", values_to = "count") %>%
  mutate(type = recode(type, raw_count = "Raw", adj_count = "Adjusted"))

# Plot as 2x4 grid: rows = type (Raw vs Adjusted), cols = season
p3 <- ggplot(curves, aes(x = hour_bin, y = count, fill = season)) +
  geom_col(position = "dodge") +
  facet_grid(type ~ season, scales = "free_y") +
  labs(title = paste("Hourly counts for", top_species_cell$species,
                     "at cell", top_species_cell$cell),
       x = "Hour of day", y = "Number of observations") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7, 20, 1))

print(p3)