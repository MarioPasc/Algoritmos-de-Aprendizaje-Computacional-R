library(caret)
library(ggplot2)
library(pROC)
library(nnet)
library(e1071)

train_test_split <- function(data, train_ratio, target_var, save_path) {

  data[[target_var]] <- as.numeric(as.character(data[[target_var]]))
  
  # Crear partición de datos
  set.seed(42)  # Para reproducibilidad
  train_index <- createDataPartition(data[[target_var]], p = train_ratio, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Guardar archivos
  write.csv(train_data, file.path(save_path, "train.csv"), row.names = FALSE)
  write.csv(test_data, file.path(save_path, "test.csv"), row.names = FALSE)
  
  # Crear gráfica
  data_summary <- data.frame(
    Dataset = rep(c("Train", "Test"), each = 2),
    Class = rep(c("Positivo", "Negativo"), 2),
    Count = c(
      sum(train_data[[target_var]]), nrow(train_data) - sum(train_data[[target_var]]),
      sum(test_data[[target_var]]), nrow(test_data) - sum(test_data[[target_var]])
    )
  )
  blue = '#377eb8'
  red = '#e41a1c'
  plot <- ggplot(data_summary, aes(x = Dataset, y = Count, fill = Class)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("Positivo" = blue, "Negativo" = red)) +
    labs(title = "Distribución de Datos", x = "Conjunto", y = "Número de muestras", fill = "Class") +
    theme_minimal()
  
  
  # Devolver resultados
  list(
    train_index = train_index,
    plot = plot,
    data_summary = data_summary
  )
}


repeated_holdout <- function(train_data, val_ratio, target_var, n_iterations, threshold = 0.5, vars = NULL, model_func = NULL) {
  if (is.null(vars)) {
    vars <- "."
  } else {
    vars <- paste(vars, collapse = " + ")
  }
  
  formula <- as.formula(paste(target_var, "~", vars))
  
  results <- data.frame(Iteration = integer(0), TP = integer(0), TN = integer(0), FP = integer(0), FN = integer(0), stringsAsFactors = FALSE)
  
  for (i in 1:n_iterations) {
    val_index <- sample(nrow(train_data), round(nrow(train_data) * val_ratio))
    val_data <- train_data[val_index, ]
    train_subset <- train_data[-val_index, ]
    
    model <- model_func(formula, train_subset)
    
    # Check if the model is an SVM
    if (inherits(model, "svm")) {
      # Transform validation data for SVM
      x_val <- model.matrix(formula, val_data)
      predictions <- predict(model, newdata = x_val, probability = TRUE)
      predictions <- attr(predictions, "probabilities")[, 2]
    } else {
      # Use original prediction code for other models
      pred_type <- "raw"
      predictions <- predict(model, newdata = val_data, type = pred_type)
    }
    
    actual_classes <- val_data[[target_var]]
    predicted_classes <- ifelse(predictions > threshold, 1, 0)
    
    tp <- sum(predicted_classes == 1 & actual_classes == 1)
    tn <- sum(predicted_classes == 0 & actual_classes == 0)
    fp <- sum(predicted_classes == 1 & actual_classes == 0)
    fn <- sum(predicted_classes == 0 & actual_classes == 1)
    
    results <- rbind(results, data.frame(Iteration = i, TP = tp, TN = tn, FP = fp, FN = fn))
  }
  
  list(results = results)
}






evaluate_holdout <- function(results) {
  # Calcular las métricas por iteración
  metrics <- data.frame(
    Iteration = results$Iteration,
    Accuracy = (results$TP + results$TN) / (results$TP + results$TN + results$FP + results$FN),
    Precision = results$TP / (results$TP + results$FP),
    Recall = results$TP / (results$TP + results$FN),
    F1Score = 2 * (results$TP / (results$TP + results$FP) * results$TP / (results$TP + results$FN)) /
      (results$TP / (results$TP + results$FP) + results$TP / (results$TP + results$FN))
  )
  
  # Calcular las métricas promedio
  avg_metrics <- list(
    Accuracy = mean(metrics$Accuracy),
    Precision = mean(metrics$Precision),
    Recall = mean(metrics$Recall),
    F1Score = mean(metrics$F1Score)
  )
  
  # Crear la gráfica
  metrics_long <- reshape2::melt(metrics, id.vars = "Iteration", variable.name = "Metric", value.name = "Value")
  plot <- ggplot(metrics_long, aes(x = Iteration, y = Value, color = Metric)) +
    geom_line() +
    scale_color_manual(values = c("Accuracy" = "blue", "Precision" = "red", "Recall" = "green", "F1Score" = "purple")) +
    labs(title = "Metrics by Iteration", x = "Iteration", y = "Value") +
    theme_minimal() +
    ylim(0, 1) +
    theme(legend.position = "bottom")
  
  # Devolver los resultados
  list(metrics = metrics, avg_metrics = avg_metrics, plot = plot)
}

