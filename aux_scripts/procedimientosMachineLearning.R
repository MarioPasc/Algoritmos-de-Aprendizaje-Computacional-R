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

evaluate_aparent_performance_model <- function(data, target_var, model_func, vars = NULL, threshold = 0.5) {
  if (is.null(vars)) {
    vars <- setdiff(names(data), target_var)
  }
  
  # Convertir la variable objetivo en un factor con niveles "0" y "1"
  data[[target_var]] <- factor(data[[target_var]], levels = c(0, 1))
  
  formula <- as.formula(paste(target_var, "~", paste(vars, collapse = "+")))
  
  # Entrenar el modelo con todo el conjunto de datos
  model <- model_func(formula, data)
  
  # Realizar predicciones con el mismo conjunto de datos
  if (inherits(model, "svm")) {
    predictions <- predict(model, newdata = data, probability = TRUE)
    predictions <- attr(predictions, "probabilities")[, 2]
  } else if (inherits(model, "nnet")) {
    predictions <- predict(model, newdata = data, type = "raw")
  } else if (inherits(model, "rpart")) {
    predictions <- predict(model, newdata = data, type = "prob")[, 2]
  } else {
    predictions <- predict(model, newdata = data, type = "response")
  }
  
  # Calcular la matriz de confusión
  actual_classes <- data[[target_var]]
  predicted_classes <- ifelse(predictions > threshold, 1, 0)
  
  confusion_matrix <- table(predicted_classes, actual_classes)
  
  # Inicializar métricas de evaluación
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  
  # Actualizar métricas de evaluación según la matriz de confusión
  if ("1" %in% rownames(confusion_matrix) && "1" %in% colnames(confusion_matrix)) {
    tp <- confusion_matrix["1", "1"]
  }
  if ("0" %in% rownames(confusion_matrix) && "0" %in% colnames(confusion_matrix)) {
    tn <- confusion_matrix["0", "0"]
  }
  if ("1" %in% rownames(confusion_matrix) && "0" %in% colnames(confusion_matrix)) {
    fp <- confusion_matrix["1", "0"]
  }
  if ("0" %in% rownames(confusion_matrix) && "1" %in% colnames(confusion_matrix)) {
    fn <- confusion_matrix["0", "1"]
  }
  
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  f1_score <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  # Calcular la curva ROC utilizando pROC
  roc_obj <- pROC::roc(actual_classes, predictions)
  
  # Devolver los resultados
  list(confusion_matrix = confusion_matrix, accuracy = accuracy, precision = precision, 
       recall = recall, f1_score = f1_score, roc_curve = roc_obj)
}



# =================== INNER VALIDATION ==========================


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
    
    if (inherits(model, "svm")) {
      # Transform validation data for SVM
      x_val <- model.matrix(formula, val_data)
      predictions <- predict(model, newdata = x_val, probability = TRUE)
      predictions <- attr(predictions, "probabilities")[, 2]
    } else if (inherits(model, "nnet")) {
      # Use original prediction code for NNET models
      pred_type <- "raw"
      predictions <- predict(model, newdata = val_data, type = pred_type)
    } else if (inherits(model, "rpart")) {
      # Use prediction code for rpart models
      predictions <- predict(model, newdata = val_data, type = "prob")[, 2]
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

library(caret)

double_cross_validation <- function(data, target_var, outer_folds, inner_folds, threshold = 0.5, vars = NULL, model_func = NULL, hyperparams = NULL) {
  if (is.null(vars)) {
    vars <- "."
  } else {
    vars <- paste(vars, collapse = " + ")
  }
  
  formula <- as.formula(paste(target_var, "~", vars))
  
  results <- data.frame(Fold = integer(0), TP = integer(0), TN = integer(0), FP = integer(0), FN = integer(0), BestParams = list(), stringsAsFactors = FALSE)
  
  # Create outer folds using createFolds
  outer_fold_ids <- createFolds(data[[target_var]], k = outer_folds, list = TRUE, returnTrain = TRUE)
  
  # Outer cross-validation loop
  for (i in 1:outer_folds) {
    # Split data into training and test sets using outer fold ids
    train_val_data <- data[outer_fold_ids[[i]], ]
    test_data <- data[-outer_fold_ids[[i]], ]
    
    best_model <- NULL
    best_performance <- 0
    best_params <- NULL
    
    # Create inner folds using createFolds
    inner_fold_ids <- createFolds(train_val_data[[target_var]], k = inner_folds, list = TRUE, returnTrain = TRUE)
    
    # Inner cross-validation loop
    for (params in hyperparams) {
      val_performance <- 0
      
      for (j in 1:inner_folds) {
        # Split data into training and validation sets using inner fold ids
        train_data <- train_val_data[inner_fold_ids[[j]], ]
        val_data <- train_val_data[-inner_fold_ids[[j]], ]
        
        model <- do.call(model_func, c(list(formula = formula, data = train_data), params))
        
        if (inherits(model, "svm")) {
          # Transform validation data for SVM
          x_val <- model.matrix(formula, val_data)
          predictions <- predict(model, newdata = x_val, probability = TRUE)
          predictions <- attr(predictions, "probabilities")[, 2]
        } else if (inherits(model, "nnet")) {
          # Use original prediction code for NNET models
          pred_type <- "raw"
          predictions <- predict(model, newdata = val_data, type = pred_type)
        } else if (inherits(model, "rpart")) {
          # Use prediction code for rpart models
          predictions <- predict(model, newdata = val_data, type = "prob")[, 2]
        }
        
        actual_classes <- val_data[[target_var]]
        predicted_classes <- ifelse(predictions > threshold, 1, 0)
        
        performance <- sum(predicted_classes == actual_classes) / length(actual_classes)
        val_performance <- val_performance + performance
      }
      
      val_performance <- val_performance / inner_folds
      
      if (val_performance > best_performance) {
        best_model <- model
        best_performance <- val_performance
        best_params <- params
      }
    }
    
    actual_classes <- test_data[[target_var]]
    predicted_classes <- ifelse(predict(best_model, newdata = test_data, type = "response") > threshold, 1, 0)
    
    tp <- sum(predicted_classes == 1 & actual_classes == 1)
    tn <- sum(predicted_classes == 0 & actual_classes == 0)
    fp <- sum(predicted_classes == 1 & actual_classes == 0)
    fn <- sum(predicted_classes == 0 & actual_classes == 1)
    
    results <- rbind(results, data.frame(Fold = i, TP = tp, TN = tn, FP = fp, FN = fn, BestParams = list(best_params)))
  }
  
  list(results = results)
}





#====================EVALUATE=====================================


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

evaluate_double_cross_validation <- function(results) {
  # Calcular las métricas por fold
  metrics <- data.frame(
    Fold = results$Fold,
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
  metrics_long <- reshape2::melt(metrics, id.vars = "Fold", variable.name = "Metric", value.name = "Value")
  plot <- ggplot(metrics_long, aes(x = Fold, y = Value, color = Metric)) +
    geom_line() +
    scale_color_manual(values = c("Accuracy" = "blue", "Precision" = "red", "Recall" = "green", "F1Score" = "purple")) +
    labs(title = "Metrics by Fold", x = "Fold", y = "Value") +
    theme_minimal() +
    ylim(0, 1) +
    theme(legend.position = "bottom")
  
  # Devolver los resultados
  list(metrics = metrics, avg_metrics = avg_metrics, plot = plot)
}



