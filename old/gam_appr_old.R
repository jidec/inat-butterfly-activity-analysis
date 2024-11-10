#GAM Approach 2 - one GAM per season-cell combination

{r}

data$season_cell <- paste0(data$season, "-", data$cell)
sampling_effort$season_cell <- paste0(sampling_effort$season, "-", sampling_effort$cell)

effort_gam <- lapply(unique(data$season_cell), function(s) {
  
  df <- dplyr::filter(sampling_effort, season_cell == s)
  print(nrow(df))
  if(nrow(df) > 1000){
    
    m <- mgcv::gam(n_obs~local_hour, data=df, family = "poisson")
    pred_df <- data.frame(local_hour = 1:24)
    pred <- predict(m, pred_df)
    out <- data.frame(pred_df, effort = pred, season = s)
    return(out)
  }
}) %>%
  bind_rows()

View(effort_gam)

Check for a well sampled species

{r}

### Subsample -- test species. ----
testspecies <- allElevationData_effort %>%
  dplyr::filter(common_name == "Indian Blackbird")

test <- testspecies %>%
  group_by(season) %>%
  dplyr::sample_n(size = 10000, replace = T, weight = inverse_cum_min)

# Raw observation data.
testspecies %>%
  ggplot() +
  geom_bar(aes(elev), color = "black") +
  facet_wrap(~season)

# subsampled observation data.
test %>%
  ggplot() +
  geom_bar(aes(elev), color = "black") +
  facet_wrap(~season)


