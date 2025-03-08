library(phyr)
library(colorEvoHelpers)

sscs_and_tree <- readRDS("sscs_and_tree.rds")

sscs <- sscs_and_tree[[1]]
tree <- sscs_and_tree[[2]]

m <- pglmm(q10_value ~ temp_sc + wingspan_sc + daylength_sc + (1|cell) + (0+temp_sc|cell) + (1|species__) + (0+temp_sc|species__), data = sscs, cov_ranef = list(species=tree), bayes=TRUE)
plot_bayes(m)

p <- plotPglmmEffect(m,sscs, response_colname = "q10_value", pred_colname="daylength_sc", other_pred_colnames = c("temp_sc","wingspan_sc"))
p + labs(x = "Daylength", y = "Onset") + geom_point(data = sscs, aes(x = daylength_sc, y = q10_value),alpha=0.05)


pglmm = m
data = sscs
response_colname="q10_value"
pred_colname="daylength_sc"
other_pred_colnames = c("temp_sc","wingspan_sc")

plotPglmmEffect <- function(pglmm, data, tree, response_colname="",pred_colname="", other_pred_colnames=""){
  library(tidyverse)
  library(dplyr)
  library(phyr)
  
  #' so to make a figure showing the effect of a variable, sjPlot style,
  #' you need to hold the rest of your predictors constant, I hold them at mean values,
  #' and I put random effects as NA, so the response is not informed by species
  #' then you vary your variable of interest across your range of interst (rarely would you want to predict outside the range of data you have)
  #'
  #' Below is the predict dataframe that varies daylength, and holds the other predictors constant
  #' random effects are NA
  #' duration is NA b/c that's what we are predictingg for
  
  pred_df_parse <- "data.frame("
  pred_df_parse <- paste0(pred_df_parse, response_colname, " = rep(NA,7),")
  
  pred_df_parse <- paste0(pred_df_parse, pred_colname, " = -3:3,")
  
  for(pred_colname2 in other_pred_colnames){
    pred_df_parse <- paste0(pred_df_parse, pred_colname2," = rep(mean(data$", pred_colname2, ", na.rm=T),7),")
  }
  
  pred_df_parse <- paste0(pred_df_parse,"species = rep(NA,7))")
  
  
  pred_df <- eval(parse(text=pred_df_parse))
  pred_df$cell <- rep(NA,7)
  #pred_df$season <- rep(NA,7)

  # now we combine our predict dataframe with our data, dataframe.
  #' basically we are fitting the model with the information from the data dataframe
  #' and predicting onto the pred_df
  #'
  
  comb_df <- data %>%
    select(c(response_colname,pred_colname,other_pred_colnames,"species","cell")) %>%
    bind_rows(pred_df)
  
  #' then fit the model, you'll get duration estimates for all rows, but we are
  #' interested in the last 7. The ones we hold the rest of the variables constant
  #' note you didn't send the phylogeny, so your results might change some when you fit
  #' the model with the phylogenetic effects
  #'
  pred_model_parse <- paste0("pglmm(", response_colname, "~")
  pred_model_parse <- paste0(pred_model_parse, pred_colname, "+")
  for(pred_colname2 in other_pred_colnames){
    pred_model_parse <- paste0(pred_model_parse, pred_colname2, "+")
  }
  
  pred_model_parse2 <- paste0(pred_model_parse, "(1|species), data = comb_df, bayes=TRUE)")
  pred_model_parse2
  pred_model_parse2 <- paste0(pred_model_parse, "(1|cell) + (0+temp_sc|cell) + (1|species__) + (0+temp_sc|species__), data = comb_df, cov_ranef = list(species=tree), bayes=TRUE)")
  pred_model_parse2
  
  pred_model <- eval(parse(text=pred_model_parse2))
  
  #pred_model <- pglmm(duration ~ tmax + daylength + wingspan +
  #                        (1|species),
  #                    data = comb_df,
  #                    bayes = TRUE)
  
  rdf1_parse <- paste0("pred_model$inla.model$summary.linear.predictor[(nrow(data)+1):nrow(comb_df),] %>%
        mutate(", pred_colname, "=-3:3)")
  
  rdf1 <- eval(parse(text = rdf1_parse))
  
  
  p_parse <- paste0("ggplot(rdf1, mapping = aes(x = ",pred_colname, ",y = mean)) +
        geom_ribbon(mapping = aes(ymin = `0.025quant`, ymax = `0.975quant`),
                    alpha = 0.15) +
        geom_line(mapping = aes(), linewidth = 1.05)")
  
  p <- eval(parse(text=p_parse))
  p
  return(p)
}