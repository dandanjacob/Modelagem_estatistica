# Instale e carregue os pacotes necessários
# Assumindo que os pacotes já estão instalados
# Instale o lattice, se ainda não estiver instalado
# Carregue o lattice antes dos outros pacotes

library(lattice)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(lme4)
library(performance)
library(viridis)
library(ggcorrplot)
library(gridExtra)
library(dplyr)
library(tidyr)


getwd()

# Carregar os dados
data <- read.csv("C:/Users/Daniel/OneDrive/Área de Trabalho/7período/ME/final_project/Modelagem_estatistica/insurance.csv")
print(head(data))

#-------------- TRANSFORM DATA --------------#
# Transform sex in binary
data$sex_male <- as.numeric(data$sex == "male")
data <- data[, -which(names(data) == "sex")] #remover coluna categórica

#Transform smoker in binary
data$smoker_yes <- as.numeric(data$smoker == "yes")
data <- data[, -which(names(data) == "smoker")] #remove coluna categórica

# Transformar a coluna 'region' em quatro colunas
region_dummies <- model.matrix(~region - 1, data = data)
colnames(region_dummies) <- c("northeast", "northwest", "southeast", "southwest")
data <- cbind(data, region_dummies)
data <- data[, -which(names(data) == "region")]#remover categórica

#  Add colunm binary for imc>30
data$obese <- ifelse(data$bmi >= 30, 1, 0)


#add colunm square_age 
data$square_age <- (data$age)**2
data <- data[, -which(names(data) == "age")]#remover age


# #add colunm scale_charge
# scale_charge <- log(data$charges)
# data <- data[, -which(names(data) == "charges")]#remover age
# data$charges <- scale_charge

print(head(data))

#-------------- EXPLORING DATA --------------#
# #plot covariance matrix
# corr_matrix <- cor(data)
# melted_corr_matrix <- melt(corr_matrix)
# ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
#  geom_tile(color = "white") +
#  scale_fill_gradient2(low = "darkblue", high = "red", mid = "white",
#                       midpoint = 0, limit = c(-1,1), space = "Lab",
#                       name="Correlation") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1,
#                                   size = 12, hjust = 1)) +
#  coord_fixed() +
#  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4)

data_fumantes <- subset(data, smoker_yes == 1)
data_nao_fumantes <- subset(data, smoker_yes == 0)

# # Plot age x imc where color is charges in non smokers
# ggplot(data_nao_fumantes, aes(x = age, y = bmi, color = charges)) +
#   geom_point() +  # Adiciona pontos ao gráfico
#   labs(title = "Idade vs IMC para Não Fumantes",
#        x = "Idade",
#        y = "IMC",
#        color = "Custos") +
#   scale_color_viridis_c() +  # Escolhe uma paleta de cores para representar os custos
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5)  # Centraliza o título horizontalmente
#   )

# # Plot age x imc where color is charges in  smokers
# ggplot(data_fumantes, aes(x = age, y = bmi, color = charges)) +
#   geom_point() +  # Adiciona pontos ao gráfico
#   labs(title = "Idade vs IMC para Fumantes",
#        x = "Idade",
#        y = "IMC",
#        color = "Custos") +
#   scale_color_viridis_c() +  # Escolhe uma paleta de cores para representar os custos
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5)  # Centraliza o título horizontalmente
#   )

# # plot boxplot fumantesxcharges
# ggplot(data, aes(x = factor(smoker_yes), y = charges)) +
# geom_boxplot() +
# labs(title = "Boxplot de Charges por Fumantes e Não Fumantes",
#      x = "Fumante (0 = Não, 1 = Sim)",
#      y = "Charges") +
# scale_fill_manual(values = c("blue", "red")) +  # Escolha cores personalizadas
# theme_minimal()

# Plot de idade vs charges, separando por smoker_yes
# ggplot(data, aes(x = age, y = charges, color = factor(smoker_yes))) +
#   geom_point() +  # Adiciona pontos ao gráfico
#   labs(title = "Relação entre Idade e Custo Médio por Hábito de Fumar",
#        x = "Idade",
#        y = "Custo Médio",
#        color = "Fumante (0 = Não, 1 = Sim)") +
#   scale_color_manual(values = c("blue", "red")) +  # Define as cores manualmente
#   theme_minimal()


# # Plot de idade vs charges para fumantes, separando por obese
# ggplot(data_fumantes, aes(x = age, y = charges, color = factor(obese))) +
#   geom_point() +  # Adiciona pontos ao gráfico
#   labs(title = "Relação entre Idade e Custo Médio para Fumantes, por Obesidade",
#        x = "Idade",
#        y = "Custo Médio",
#        color = "Obeso (0 = Não, 1 = Sim)") +
#   scale_color_manual(values = c("blue", "red")) +  # Define as cores manualmente
#   theme_minimal()
# 
# #plotagem do gráfico de dispersão com cores e formas diferentes
# ggplot(data, aes(x = age, y = charges, color = factor(smoker_yes), shape = factor(obese))) +
#   geom_point(size = 3) +  # Adiciona pontos ao gráfico
#   labs(title = "Idade vs Custos por Status de Fumante e Obesidade",
#        x = "Idade",
#        y = "Custos",
#        color = "Fumante",
#        shape = "Obesidade") +
#   scale_color_manual(values = c("blue", "red"), labels = c("Não Fumante", "Fumante")) +  # Define cores e rótulos para fumantes
#   scale_shape_manual(values = c(1, 16), labels = c("Não Obeso", "Obeso")) +  # Define formas e rótulos invertidos para obesidade
#   guides(color = guide_legend(title = "Status de Fumante", override.aes = list(shape = c(15, 15)))) +  # Ajusta a legenda de cores com um título personalizado e formas
#   theme_minimal() + 
#   theme(plot.title = element_text(hjust = 0.5))

# # Plotar age x log_charges
# ggplot(data, aes(x = square_age, y = charges)) +
#   geom_point() +  # Adiciona os pontos
#   labs(x = "Idade", y = "Charges", title = "Influência da idade sobre os custos") +
#   theme_minimal() + 
#   theme(plot.title = element_text(hjust = 0.5))


#-------------- SIMPLE REGRESSION --------------#
model <- lm(charges ~   square_age + bmi + children + sex_male+ northeast+ northwest + southeast +southwest +
                smoker_yes + obese, data = data)

summary_model <- summary(model)
print(summary_model)

aic_value <- AIC(model)
print(paste("AIC:", aic_value))

bic_value <- BIC(model)
print(paste("BIC:", bic_value))

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

print(coef(model))
print(confint(model))
# Obter os valores preditos
predicted <- predict(model, data)

# Definir cores
# colors <- ifelse(data$smoker_yes == 0, "blue",
#                 ifelse(data$obese == 1, "pink", "red"))
# colors <-  viridis(100)[cut(data$age, breaks = 100)]
data_predicted <- data.frame(data$charges, predicted, data$smoker_yes, data$obese)
# Definir cores com base na variável obese
colors <- ifelse(data_predicted$data.obese == "0", "blue", "red")

# Converter variáveis para fatores no dataframe
data_predicted$data.smoker_yes <- factor(data_predicted$data.smoker_yes, levels = c(0, 1), labels = c("Não Fumante", "Fumante"))
data_predicted$data.obese <- factor(data_predicted$data.obese, levels = c(0, 1), labels = c("Não Obeso", "Obeso"))

# Gráfico ggplot com adição da linha y = x e legenda
ggplot(data_predicted, aes(x = data.charges, y = predicted, color = factor(data.smoker_yes), shape = factor(data.obese))) +
  geom_point(size = 3) +  # Adiciona pontos ao gráfico
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Adiciona a linha y = x
  labs(title = "Idade vs Custos por Status de Fumante e Obesidade",
       x = "Valor real",
       y = "Valor predito",
       color = "Fumante",
       shape = "Obesidade") +
  scale_color_manual(values = c("blue", "red"), labels = c("Não Fumante", "Fumante")) +  # Define cores e rótulos para fumantes
  scale_shape_manual(values = c(1, 16), labels = c("Não Obeso", "Obeso")) +  # Define formas e rótulos invertidos para obesidade
  guides(color = guide_legend(title = "Status de Fumante", override.aes = list(shape = c(15, 15))),
         shape = guide_legend(title = "Obesidade", override.aes = list(color = "black", linetype = "dashed"))) +  # Ajusta a legenda de cores com um título personalizado e formas
  annotate("text", x = 60000, y = 52000, label = "Reta x=y", color = "gray", hjust = 1.1, vjust = 1.1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Calcular os resíduos
residuos <- residuals(model)

# Plotar os resíduos
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")


#-------------- SUBNIVEL REGRESSION --------------#
#aplly multinível regresson
model <- lmer(charges ~   square_age + bmi + children + sex_male+ northeast+ northwest + southeast +southwest   + (1 | smoker_yes:obese), data = data)

summary_model <- summary(model)
print(summary_model)
#saveRDS(model, file = "model.rds")
#capture.output(summary_model, file = "model_summary.txt")

aic_value <- AIC(model)
print(paste("AIC:", aic_value))

bic_value <- BIC(model)
print(paste("BIC:", bic_value))

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

#write(paste("AIC:", aic_value, "\nR²:", r2_value), file = "model_metrics.txt")

print(coef(model))
print(confint(model))
# Obter os valores preditos
predicted <- predict(model, data)

# Definir cores
#colors <- ifelse(data$smoker_yes == 0, "blue", 
#                 ifelse(data$obese == 1, "pink", "red"))
data_predicted <- data.frame(data$charges, predicted,data$smoker_yes, data$obese)
# print(head(data_predicted))
colors <- ifelse(data$obese == 0, "blue", "red")


# Gráfico ggplot com adição da linha y = x e legenda
# Gráfico ggplot com adição da linha y = x e legenda
ggplot(data_predicted, aes(x = data.charges, y = predicted, color = factor(data.smoker_yes), shape = factor(data.obese))) +
  geom_point(size = 3) +  # Adiciona pontos ao gráfico
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Adiciona a linha y = x
  labs(title = "Idade vs Custos por Status de Fumante e Obesidade",
       x = "Valor real",
       y = "Valor predito",
       color = "Fumante",
       shape = "Obesidade") +
  scale_color_manual(values = c("blue", "red"), labels = c("Não Fumante", "Fumante")) +  # Define cores e rótulos para fumantes
  scale_shape_manual(values = c(1, 16), labels = c("Não Obeso", "Obeso")) +  # Define formas e rótulos invertidos para obesidade
  guides(color = guide_legend(title = "Status de Fumante", override.aes = list(shape = c(15, 15))),
         shape = guide_legend(title = "Obesidade", override.aes = list(color = "black", linetype = "dashed"))) +  # Ajusta a legenda de cores com um título personalizado e formas
  annotate("text", x = 60000, y = 52000, label = "Reta x=y", color = "gray", hjust = 1.1, vjust = 1.1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calcular os resíduos
residuos <- residuals(model)

# Plotar os resíduos
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")

dotplot(ranef(model))
#-------------- SUBNIVEL REGRESSION  NO OUTLIERS--------------#

# Função para remover outliers
remove_outliers <- function(df) {
  Q1 <- quantile(df$charges, 0.25)
  Q3 <- quantile(df$charges, 0.75)
  IQR <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  # Contar observações antes da remoção
  n_before <- nrow(df)
  
  # Remover outliers
  df_cleaned <- df[df$charges >= limite_inferior & df$charges <= limite_superior, ]
  
  # Contar observações após a remoção
  n_after <- nrow(df_cleaned)
  
  # Calcular percentual de remoção
  percent_removed <- (n_before - n_after) / n_before * 100
  
  return(list(df_cleaned, percent_removed))
}


# Data frame de exemplo
data_frame <- data

# Data frame vazio para armazenar os dados limpos
cleaned_data <- data.frame()
# Lista para armazenar os percentuais de remoção por grupo
percent_removed_list <- list()

# Percorrer cada combinação de smoker_yes e obese
for (smoker in unique(data_frame$smoker_yes)) {
  for (obesity in unique(data_frame$obese)) {
    # Filtrar os dados para o grupo específico
    group_data <- data_frame[data_frame$smoker_yes == smoker & data_frame$obese == obesity, ]
    
    # Remover outliers e obter dados limpos e percentual removido
    result <- remove_outliers(group_data)
    cleaned_group_data <- result[[1]]
    percent_removed <- result[[2]]
    
    # Armazenar o percentual removido na lista
    percent_removed_list[[paste("smoker_", smoker, "_obese_", obesity, sep = "")]] <- percent_removed
    
    # Adicionar os dados limpos ao data frame final
    cleaned_data <- rbind(cleaned_data, cleaned_group_data)
  }
}

# Exibir o percentual de dados removidos por grupo
for (key in names(percent_removed_list)) {
  print(paste("Percentual de dados removidos para o grupo", key, ":", percent_removed_list[[key]], "%"))
}

#aplly multinível regresson
model <- lmer(charges ~   square_age + bmi + children + sex_male+ northeast+ northwest + southeast +southwest   + (1 | smoker_yes:obese), data = cleaned_data)

summary_model <- summary(model)
print(summary_model)
#saveRDS(model, file = "model.rds")
#capture.output(summary_model, file = "model_summary.txt")

aic_value <- AIC(model)
print(paste("AIC:", aic_value))

bic_value <- BIC(model)
print(paste("BIC:", bic_value))

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

#write(paste("AIC:", aic_value, "\nR²:", r2_value), file = "model_metrics.txt")
print(coef(model))
print(confint(model))
# Obter os valores preditos
predicted <- predict(model, cleaned_data)

# Definir cores
#colors <- ifelse(data$smoker_yes == 0, "blue", 
#                 ifelse(data$obese == 1, "pink", "red"))
data_predicted <- data.frame(cleaned_data$charges, predicted,cleaned_data$smoker_yes, cleaned_data$obese)
# print(head(data_predicted))
colors <- ifelse(cleaned_data$obese == 0, "blue", "red")


# Gráfico ggplot com adição da linha y = x e legenda
# Gráfico ggplot com adição da linha y = x e legenda
ggplot(data_predicted, aes(x = cleaned_data.charges, y = predicted, color = factor(cleaned_data.smoker_yes), shape = factor(cleaned_data.obese))) +
  geom_point(size = 3) +  # Adiciona pontos ao gráfico
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Adiciona a linha y = x
  labs(title = "Idade vs Custos por Status de Fumante e Obesidade",
       x = "Valor real",
       y = "Valor predito",
       color = "Fumante",
       shape = "Obesidade") +
  scale_color_manual(values = c("blue", "red"), labels = c("Não Fumante", "Fumante")) +  # Define cores e rótulos para fumantes
  scale_shape_manual(values = c(1, 16), labels = c("Não Obeso", "Obeso")) +  # Define formas e rótulos invertidos para obesidade
  guides(color = guide_legend(title = "Status de Fumante", override.aes = list(shape = c(15, 15))),
         shape = guide_legend(title = "Obesidade", override.aes = list(color = "black", linetype = "dashed"))) +  # Ajusta a legenda de cores com um título personalizado e formas
  annotate("text", x = 60000, y = 52000, label = "Reta x=y", color = "gray", hjust = 1.1, vjust = 1.1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calcular os resíduos
residuos <- residuals(model)

# Plotar os resíduos
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")

dotplot(ranef(model))
