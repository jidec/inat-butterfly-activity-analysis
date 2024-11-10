# m is the pglmm
# data is my data

preds <- m$inla.model$summary.linear.predictor
colnames(preds) <- c("mean","sd", "quant1","quant2","quant3","mode")

data <- m$data

preds$tmax <- data$tmax

ggplot(preds, aes(x = tmax,y=mean)) + 
  geom_smooth() + 
  geom_ribbon(aes(ymin = quant1, ymax = quant3), alpha = 0.2) +
  labs(x = "Predictor", y = "Prediction", title = "Effect of Predictor on Model Prediction") +
  theme_minimal()


write.csv(preds,'preds.csv')

dpt <- ggplot(preds , mapping = aes(x = V1, y = mean)) +
  geom_ribbon(mapping = aes(ymin = `0.025quant`, ymax = `0.975quant`,
                            fill = V2), 
              alpha = 0.15) +
  geom_line(mapping = aes(color = V2), size = 1.05) +
  labs(x = "Temperature", y = "Duration", fill = "Precipitation", color = "Precipitation") + 
  scale_color_manual(values = c("brown", "cyan", "Blue")) +
  scale_fill_manual(values = c("brown", "cyan", "Blue")) +
  theme_bw()

dpt


# Assuming you've read your CSV file into a dataframe called 'data'
library(ggplot2)

colnames(preds) <- c("mean","sd", "quant1","quant2","quant3","mode","kld")

# Plotting
ggplot(preds, aes(x = as.numeric(gsub("Predictor.", "", Unnamed: 0)), y = mean)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = quant1, ymax = quant3), alpha = 0.2) +
  labs(x = "Predictor", y = "Prediction", title = "Effect of Predictor on Model Prediction") +
  theme_minimal()

ggplot(preds, aes(x = PredictorNumber, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.2) +
  labs(x = "Predictor Number", y = "Mean Prediction", title = "Effect of Different Predictors on Model Prediction") +
  theme_minimal()










data <- sscs_scaled
data$pred <- predict(m, newdata = new_data, type = "response")

new_data <- data.frame(x = seq(min(sscs_scaled$tmax,na.rm=TRUE), max(sscs_scaled$tmax,na.rm=TRUE), length.out = 100))

predictions <- predict(m, newdata = new_data, type = "response")

min(sscs_scaled$tmax)
min(sscs_scaled$tmax,na.rm=TRUE)

ggplot(new_data, aes(x = x, y = predictions)) +
  geom_line() +
  labs(x = "Predictor", y = "Predicted Response") +
  theme_minimal()
#