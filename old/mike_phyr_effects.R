library(tidyverse)
library(phyr)
library(gridExtra)
library(INLA)

## read in data
mdf <- read.csv("mdf_removeOutliersResiduals_wSeasonalityTrait.csv") %>% 
  dplyr::filter(overwinteringStage != "None")

mdf <- mdf %>% 
  mutate(annualTemp = scale(annualTemp),
         tempSeas = scale(tempSeas),
         annualPrec = scale(annualPrec),
         precSeas = scale(precSeas),
         year = scale(year),
         dstdoy = scale(dstdoy),
         ndstcol = scale(ndstcol)) %>% 
  mutate(dur = q95 - q5,
         dur_CI = mean(c(q5_CI, q95_CI))) %>% 
  mutate(on_w = 1/(q5_CI + 1),
         off_w = 1/(q95_CI + 1),
         peak_w = 1/(q95_CI +1),
         dur_w = 1/(dur_CI + 1)) %>% 
  mutate(voltinism = if_else(condition = voltinism == "F",
                             true = "M",
                             false = voltinism))

# phylogenetic model time
library(phyr)
library(ape)
# function to capitalize spp names
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

tt <- read.tree("insect_tree_wBranches.tre")
tt$tip.label <- stringr::str_replace(tt$tip.label, pattern = "_", " ")
tt$tip.label <- firstup(tt$tip.label)
phylo_list <- read.csv('phy_spp_names.csv')

phylo_list <- phylo_list %>% 
  mutate(search_string2 = firstup(phylo_list$search_string)) %>% 
  dplyr::select(search_string, unique_name, ott_id, search_string2)

mdf_phylo <- left_join(mdf, phylo_list, by = c("validName" = "search_string2"))

mdf_phylo <- mdf_phylo %>% 
  filter(validName != "Polites sonora") %>% 
  filter(validName != "Nadata gibossa")

## interaction between voltinism and annual temp
inla_mdf <- mdf_phylo %>% 
  dplyr::select(dur, annualTemp, precSeas, annualPrec, dstdoy, voltinism, overwinteringStage,
                diurnality, Seas, unique_name, id_cells, year)

pred_vals <- -3:3



term1 <- data.frame(
  dur = rep(NA, length(pred_vals)),
  annualTemp = pred_vals,
  precSeas = rep(NA, length(pred_vals)),
  annualPrec = rep(-1, length(pred_vals)),
  dstdoy = rep(NA, length(pred_vals)),
  voltinism = rep(NA, length(pred_vals)),
  overwinteringStage = rep(NA, length(pred_vals)),
  diurnality = rep(NA, length(pred_vals)),
  Seas = rep(NA, length(pred_vals)),
  unique_name = rep(NA, length(pred_vals)),
  id_cells = rep(NA, length(pred_vals)),
  year = rep(NA, length(pred_vals)))

term2 <- data.frame(
  dur = rep(NA, length(pred_vals)),
  annualTemp = pred_vals,
  precSeas = rep(NA, length(pred_vals)),
  annualPrec = rep(0, length(pred_vals)),
  dstdoy = rep(NA, length(pred_vals)),
  voltinism = rep(NA, length(pred_vals)),
  overwinteringStage = rep(NA, length(pred_vals)),
  diurnality = rep(NA, length(pred_vals)),
  Seas = rep(NA, length(pred_vals)),
  unique_name = rep(NA, length(pred_vals)),
  id_cells = rep(NA, length(pred_vals)),
  year = rep(NA, length(pred_vals)))

term3 <- data.frame(
  dur = rep(NA, length(pred_vals)),
  annualTemp = pred_vals,
  precSeas = rep(NA, length(pred_vals)),
  annualPrec = rep(1, length(pred_vals)),
  dstdoy = rep(NA, length(pred_vals)),
  voltinism = rep(NA, length(pred_vals)),
  overwinteringStage = rep(NA, length(pred_vals)),
  diurnality = rep(NA, length(pred_vals)),
  Seas = rep(NA, length(pred_vals)),
  unique_name = rep(NA, length(pred_vals)),
  id_cells = rep(NA, length(pred_vals)),
  year = rep(NA, length(pred_vals)))

pred.df.1 <- rbind(inla_mdf, term1)
pred.df.2 <- rbind(inla_mdf, term2)
pred.df.3 <- rbind(inla_mdf, term3)


pglmm1 <- pglmm(formula = dur ~ annualTemp + annualPrec + precSeas +
                  dstdoy +
                  voltinism + overwinteringStage + diurnality + Seas +
                  annualTemp:voltinism +
                  annualTemp:Seas + 
                  annualTemp:annualPrec +
                  (1|unique_name__) + (1|id_cells),
                data = pred.df.1, 
                cov_ranef = list(unique_name = tt), 
                bayes = TRUE)

pglmm2 <- pglmm(formula = dur ~ annualTemp + annualPrec + precSeas +
                  dstdoy +
                  voltinism + overwinteringStage + diurnality + Seas +
                  annualTemp:voltinism +
                  annualTemp:Seas + 
                  annualTemp:annualPrec +
                  (1|unique_name__) + (1|id_cells),
                data = pred.df.2, 
                cov_ranef = list(unique_name = tt), 
                bayes = TRUE)

pglmm3 <- pglmm(formula = dur ~ annualTemp + annualPrec + precSeas +
                  dstdoy +
                  voltinism + overwinteringStage + diurnality + Seas +
                  annualTemp:voltinism +
                  annualTemp:Seas + 
                  annualTemp:annualPrec +
                  (1|unique_name__) + (1|id_cells),
                data = pred.df.3, 
                cov_ranef = list(unique_name = tt), 
                bayes = TRUE)

## make df of results
rdf1 <- pglmm1$inla.model$summary.linear.predictor[(nrow(mdf_phylo)+1):nrow(pred.df.1),] %>% 
  mutate(V1 = pred_vals,
         V2 = "Low")

rdf2 <- pglmm2$inla.model$summary.linear.predictor[(nrow(mdf_phylo)+1):nrow(pred.df.1),] %>% 
  mutate(V1 = pred_vals,
         V2 = "Mid")

rdf3 <- pglmm3$inla.model$summary.linear.predictor[(nrow(mdf_phylo)+1):nrow(pred.df.1),] %>% 
  mutate(V1 = pred_vals,
         V2 = "High")

rdf_total <- rbind(rdf1, rdf2, rdf3)
rdf_total$V2 <- factor(rdf_total$V2, levels = c("Low", "Mid", "High"))

dpt <- ggplot(rdf_total, mapping = aes(x = V1, y = mean)) +
  geom_ribbon(mapping = aes(ymin = `0.025quant`, ymax = `0.975quant`,
                            fill = V2), 
              alpha = 0.15) +
  geom_line(mapping = aes(color = V2), size = 1.05) +
  labs(x = "Temperature", y = "Duration", fill = "Precipitation", color = "Precipitation") + 
  scale_color_manual(values = c("brown", "cyan", "Blue")) +
  scale_fill_manual(values = c("brown", "cyan", "Blue")) +
  theme_bw()

dpt

save(dpt, file = "singleInteractionFigures/dur_prec_temp.Rdata")