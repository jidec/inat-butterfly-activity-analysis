
pglmm = readRDS("pglmm.rds")
data = readRDS("data.rds")
tree = readRDS("tree.rds")
response_colname = "duration"
response_colname = "q90_value"
pred_colname = "daylength"
other_pred_colnames = c("tmax","wingspan")
random_effect_colnames="species"
random_effects_formula="(1|species__)"
# this is for one predictor, but need to plot interactions somehow

# changes by mike
pred_colname = "tmax" # changed from daylength since this seems to be one of the interacting terms you're interested in
interactive_colname = "wingspan" # added this parameter
other_pred_colnames = "daylength" # changed to daylength

## now we need to refit the model, because the pglmm model you sent me does

trimDfToTree <- function(df,tree){
    library(ape)
    tree <- drop.tip(tree,tree$tip.label[!tree$tip.label %in% unique(df$clade)])
    df <- df[df$clade %in% tree$tip.label,]
    tree$edge.length[which(is.na(tree$edge.length))] <- 0
    return(list(df,tree))
}


plotPglmmEffect_Intercation <- function(pglmm, data, tree, response_colname, pred_colname,
                            interactive_colname,
                            other_pred_colnames = character(), 
                            random_effect_colnames = "species", 
                            random_effects_formula = "(1|species__)") {
    library(tidyverse)
    library(dplyr)
    library(phyr)

    # make sure the number of clades is equal between data and tree
    data_and_tree <- trimDfToTree(data, tree)
    data <- data_and_tree[[1]]
    tree <- data_and_tree[[2]]

    # Create the data frame for prediction
    pred_df <- data.frame(matrix(nrow = 7, ncol = 0))
    pred_df[[response_colname]] <- NA
    pred_df[[pred_colname]] <- -3:3
    pred_df[[interactive_colname]] <- -3:3
    for (pred_colname2 in other_pred_colnames) {
        pred_df[[pred_colname2]] <- rep(mean(data[[pred_colname2]], na.rm = TRUE), 7)
    }
    pred_df[[random_effect_colnames]] <- NA
    
    ### restructure pred_df to have a low, mid, and high value of continuous interactive effect
    pred_df_low <- pred_df
    pred_df_low[[interactive_colname]] <- -1
    pred_df_mid <- pred_df
    pred_df_mid[[interactive_colname]] <- 0
    pred_df_high <- pred_df
    pred_df_high[[interactive_colname]] <- 1
    
    # Combine data with prediction data frame
    comb_df_low <- data %>%
        select(c(response_colname, pred_colname, all_of(interactive_colname), all_of(other_pred_colnames), random_effect_colnames)) %>%
        bind_rows(pred_df_low)
    
    comb_df_mid<- data %>%
      select(c(response_colname, pred_colname,  all_of(interactive_colname), all_of(other_pred_colnames), random_effect_colnames)) %>%
      bind_rows(pred_df_mid)
    
    comb_df_high <- data %>%
      select(c(response_colname, pred_colname, all_of(interactive_colname), all_of(other_pred_colnames), random_effect_colnames)) %>%
      bind_rows(pred_df_high)

    # Construct the model formula
    fixed_effects <- c(pred_colname, other_pred_colnames, interactive_colname)
    formula_str <- paste(response_colname, "~", paste(fixed_effects, collapse = " + "), 
                         "+", paste0(pred_colname, ":", interactive_colname),
                         "+", random_effects_formula)
    formula <- as.formula(formula_str)
    

    # Fit the model
    pred_model_low <- pglmm(formula, data = comb_df_low, cov_ranef = list(species = tree), bayes = TRUE, optimizer = "bobyqa")
    pred_model_mid <- pglmm(formula, data = comb_df_mid, cov_ranef = list(species = tree), bayes = TRUE, optimizer = "bobyqa")
    pred_model_high <- pglmm(formula, data = comb_df_high, cov_ranef = list(species = tree), bayes = TRUE, optimizer = "bobyqa")
    
    # Get the predictions for the response
    rdf1_low <- pred_model_low$inla.model$summary.linear.predictor[(nrow(data) + 1):nrow(comb_df_low), ] %>%
        mutate(!!pred_colname := -3:3) %>% 
        mutate(!!interactive_colname := -1)
    
    rdf1_mid <- pred_model_mid$inla.model$summary.linear.predictor[(nrow(data) + 1):nrow(comb_df_mid), ] %>%
      mutate(!!pred_colname := -3:3) %>% 
      mutate(!!interactive_colname := 0)
    
    rdf1_high <- pred_model_high$inla.model$summary.linear.predictor[(nrow(data) + 1):nrow(comb_df_high), ] %>%
      mutate(!!pred_colname := -3:3) %>% 
      mutate(!!interactive_colname := 1)
    
    rdf <- bind_rows(rdf1_low, rdf1_mid, rdf1_high) 
    
    rdf[[interactive_colname]] <- as.factor(rdf[[interactive_colname]])
    
    # Plotting
    p <- ggplot(rdf, aes_string(x = pred_colname, y = "mean", color = interactive_colname)) +
      geom_line() +
      geom_ribbon(aes_string(ymin = "`0.025quant`", ymax = "`0.975quant`", 
                      fill = interactive_colname), alpha = 0.15)
      
    return(p)
}

summary(pglmm)

plotPglmmEffect_Intercation(pglmm = pglmm, data = data, tree = tree,
                response_colname = response_colname, 
                pred_colname = pred_colname,
                interactive_colname = interactive_colname)
