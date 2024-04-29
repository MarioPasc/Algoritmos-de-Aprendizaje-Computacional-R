library(caret)
library(ggplot2)

train_test_split <- function(data, val_ratio, train_ratio, target_var, save_path) {
  # Verificar que los porcentajes sumen 1
  if (val_ratio + train_ratio != 1) {
    stop("Los porcentajes de validación y entrenamiento deben sumar 1.")
  }
  data[[target_var]] <- as.numeric(as.character(data[[target_var]]))
  
  # Crear partición de datos
  set.seed(123)  # Para reproducibilidad
  train_index <- createDataPartition(data[[target_var]], p = train_ratio, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Crear partición de validación
  val_index <- createDataPartition(train_data[[target_var]], p = val_ratio, list = FALSE)
  val_data <- train_data[val_index, ]
  train_data <- train_data[-val_index, ]
  
  # Guardar archivos
  write.csv(train_data, file.path(save_path, "train.csv"), row.names = FALSE)
  write.csv(val_data, file.path(save_path, "val.csv"), row.names = FALSE)
  write.csv(test_data, file.path(save_path, "test.csv"), row.names = FALSE)
  
  # Crear gráfica
  data_summary <- data.frame(
    Dataset = rep(c("Train", "Validation", "Test"), each = 2),
    Class = rep(c("Positivo", "Negativo"), 3),
    Count = c(
      sum(train_data[[target_var]]), nrow(train_data) - sum(train_data[[target_var]]),
      sum(val_data[[target_var]]), nrow(val_data) - sum(val_data[[target_var]]),
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
    val_index = val_index,
    plot = plot,
    data_summary = data_summary
  )
}
