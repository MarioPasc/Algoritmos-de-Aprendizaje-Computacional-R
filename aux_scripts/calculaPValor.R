# Mario Pascual Gonzalez

# Variable categórica - categórica
# Chi-cuadrado
# Si tiene alguna instancia menor que 5 -> Fisher

# Variable categórica - numérica
# Dos categorías:
#      Distribución normal: t-test
#      Distribución diferente: wilcox test
# Más de dos categorías:
#      Distribución normal: análisis de la varianza
#      Distribución diferente: Kruskal test

# Variable numérica - numérica
# Distribución normal: correlación de Pearson
# Distribución diferente: correlación de Spearman


calculaPValor <- function(datos, var1, var2) {
  # Calculamos las variables categóricas del dataset
  varNum <- colnames(datos)[sapply(datos, is.numeric)]
  varCat <- colnames(datos)[!colnames(datos) %in% varNum]
  
  # Comprobamos los tipos de variables que son var1 y var2
  var1.cat <- var1 %in% varCat
  var2.cat <- var2 %in% varCat
  vector.vars <- c(var1, var2)
  
  # Calculamos la tabla de contingencia utilzando xtabs y construyendo una 
  # fórmula dinámica con los argumentos de la función
  formula.text <- paste("~", var1, "+", var2, sep = " ")
  tabla.contingencia <- xtabs(formula = as.formula(formula.text), 
                              data = datos)
  
  if (var1.cat && var2.cat) { # Ambas son categóricas
    # Se ha añadido un trycatch ya que hay veces que da error por el tamaño del
    # workspace. Parece que para variables categóricas con demasiados tipos 
    # únicos la carga computacional del test de fisher excede la memoria alojada
    # en el entorno de trabajo. 
    
    tryCatch({
      if (any(tabla.contingencia) < 5) { # Si hay al menos una variable que tiene <5
        
        fish.test <- fisher.test(tabla.contingencia, simulate.p.value=TRUE)
        return(list(fish.test$p.value, "Fisher test"))
        
      } else { # Ambas variables tienen más de 5 valores por clase
        
        chi.test <- chisq.test(tabla.contingencia, simulate.p.value=TRUE)
        return(list(chi.test$p.value, "ChiSQ test"))
        
      }
      
    }, error = function(e) {
      message("Se ha producido un error", e)
      return(NA)
    })
    
  } else if (!var1.cat && !var2.cat) { # Ambas son numéricas
    
    if (
      shapiro.test(datos[[var1]])$p.value > 0.05 &&
      shapiro.test(datos[[var2]])$p.value > 0.05
    ) {
      return(list(cor.test(datos[[var1]], datos[[var2]], method = "pearson")$p.value,
                  "Pearson test"))
    } else {
      return(list(cor.test(datos[[var1]], datos[[var2]], method = "spearman")$p.value,
                  "Spearman test"))
    }
    
  } else { # Una de las dos es numérica
    
    # Almacenamos la que es numérica y la que no
    if (var1.cat) {
      numerica <- var2
      categorica <- var1
      formula.text <- paste(var2, "~", var1, sep = " ")
    } else {
      numerica <- var1
      categorica <- var2
      formula.text <- paste(var1, "~", var2, sep = " ")
    }
  
    
    if (length(unique(na.omit(datos[[categorica]]))) == 2) {
      
      if (shapiro.test(datos[[numerica]])$p.value > 0.05) {
        # 2 tipos, distribucion normal -> t test
        ttest.test <- t.test(formula = as.formula(formula.text), 
                             data = datos)
        return(list(ttest.test$p.value, "T test"))
      } else {
        # 2 tipos, distribucion diferente -> Wilcoxon test
        wilcoxon.test <- wilcox.test(formula = as.formula(formula.text),
                                     data = datos)
        return(list(wilcoxon.test$p.value, "Wilcoxon test"))
      }
      
    } else {
      
      if (shapiro.test(datos[[numerica]])$p.value > 0.05) {
        # Más de 2 tipos, distribución normal -> AOV
        aov.test <- summary(aov(formula = as.formula(formula.text), 
                                data = datos))
        return(list(aov.test[[1]][["Pr(>F)"]][1], "AOV test"))
        
      } else {
        # Más de dos datos, distribución diferente -> Kruskal
        kruskal.test <- kruskal.test(formula = as.formula(formula.text), 
                                     data = datos)
        return(list(kruskal.test$p.value, "Kurskal test"))
      }
      
    }
  }
}

aplicaCalculaPValorATodosLosPares <- function(datos) {
  # Obtén todos los nombres de las variables del dataset
  nombres_variables <- names(datos)
  
  # Inicializa un vector para almacenar los resultados
  resultados_p_valores <- numeric(0) # Vector vacío
  test_types <- character(0)
  variable_1 <- character(0) 
  variable_2 <- character(0)
  
  # Itera sobre cada combinación única de pares de variables
  for (i in 1:(length(nombres_variables) - 1)) {
    for (j in (i + 1):length(nombres_variables)) {
      # Nombres de las variables actuales
      var1 <- nombres_variables[i]
      var2 <- nombres_variables[j]
      
      # Llama a calculaPValor para el par de variables actual
      resultado_completo <- calculaPValor(datos, var1, var2)
      resultado <- resultado_completo[[1]]
      test_type <- resultado_completo[[2]]
      
      # Almacena el resultado p en el vector
      resultados_p_valores <- c(resultados_p_valores, resultado)
      test_types <- c(test_types, test_type)
      
      # Guarda el nombre del par de variables
      variable_1 <- c(variable_1, paste(var1))
      variable_2 <- c(variable_2, paste(var2))
      
    }
  }
  
  resultados_df <- data.frame(VariableX = variable_1, VariableY = variable_2, 
                              Valor = resultados_p_valores, Test = test_types)
  return(resultados_df)
}

ajustarModeloLogistico <- function(datos, variable_objetivo, predictoras = ".", control = list()) {
  # Asegurarse de que la variable objetivo está en formato correcto
  if(!is.factor(datos[[variable_objetivo]])) {
    datos[[variable_objetivo]] <- as.factor(datos[[variable_objetivo]])
  }
  
  # Eliminar filas con valores NA
  datos <- na.omit(datos)
  
  # Formula inicial con las variables predictoras especificadas
  formula_inicial <- as.formula(paste(variable_objetivo, "~", predictoras))
  
  # Ajustar modelo de regresión logística
  modelo_inicial <- glm(formula = formula_inicial, data = datos, family = binomial())
  
  # Aplicar stepAIC para selección de características
  modelo_optimizado <- stepAIC(modelo_inicial, direction = "both", trace = FALSE, scope = list(lower = formula_inicial, upper = formula_inicial), k = 2)
  
  # Extraer las variables del modelo final
  variables_finales <- names(coef(modelo_optimizado))
  
  # Retornar el modelo optimizado y las variables seleccionadas
  return(list(modelo = modelo_optimizado, variables_seleccionadas = variables_finales))
}


plot_p_valores <- function(data, notation) {
  blue = '#377eb8'
  red = '#e41a1c'
  df <- aplicaCalculaPValorATodosLosPares(data)
  
  p_matrix <- dcast(df, VariableX ~ VariableY, value.var = "Valor")
  p_matrix[is.na(p_matrix)] <- 1
  
  # Crear la matriz transpuesta intercambiando las columnas VariableX y VariableY
  p_matrix_transposed <- dcast(df, VariableY ~ VariableX, value.var = "Valor")
  p_matrix_transposed[is.na(p_matrix_transposed)] <- 1
  
  # Combinar p_matrix y p_matrix_transposed para obtener la matriz simétrica
  full_p_matrix <- pmin(p_matrix, p_matrix_transposed)
  
  # Asegurarse de que las variables en ambos ejes estén en el mismo orden
  full_p_matrix <- full_p_matrix[order(rownames(full_p_matrix)), order(colnames(full_p_matrix))]
  
  # Transformar la matriz en un formato adecuado para ggplot
  melted_data <- melt(full_p_matrix, id.vars = "VariableX")
  melted_data["value"] <- sapply(melted_data["value"], function(x) { as.double(x)})
  
  plot <- ggplot(melted_data, aes(x = VariableX, y = variable, fill = value)) +
    geom_tile() +  
    geom_text(aes(label = sprintf("%.2g", value)), color = "white", size = 3) +
    scale_fill_gradient(low = red, high = blue, limits = c(0, 1), space = "Lab", na.value = "grey50") +
    labs(title = "Matriz de Calor de P-valores", x = "Variable X", y = "Variable Y", fill = "P-valor") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}





