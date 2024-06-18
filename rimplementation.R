# Instale e carregue os pacotes necessários
# Assumindo que os pacotes já estão instalados
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(lme4)
library(performance)
library(viridis)
library(ggcorrplot)
library("gridExtra")
library(dplyr)

getwd()

# Carregar os dados
data <- read.csv("C:\\Users\\Daniel\\OneDrive\\Área de Trabalho\\7período\\ME\\projeto_final\\insurance.csv")
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

#-------------- REMOÇÃO DE OUTLIERS --------------#

# Calcular os limites superior e inferior usando os quartis dos dados
limite_inferior <- quantile(data$charges, probs = 0.25) - 1.5 * IQR(data$charges)
limite_superior <- quantile(data$charges, probs = 0.75) + 1.5 * IQR(data$charges)

# Identificar outliers com base nos resíduos
outliers <- residuos < limite_inferior | residuos > limite_superior

# Remover outliers do conjunto de dados
data <- data[!outliers, ]

#-------------- SIMPLE REGRESSION --------------#
model <- lm(charges ~   square_age + bmi + children + sex_male+ northeast+ northwest + southeast +southwest +
                smoker_yes + obese, data = data)

summary_model <- summary(model)
print(summary_model)

aic_value <- AIC(model)
print(paste("AIC:", aic_value))

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

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

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

#write(paste("AIC:", aic_value, "\nR²:", r2_value), file = "model_metrics.txt")


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
print(coef(model))
# Calcular os resíduos
residuos <- residuals(model)

# Plotar os resíduos
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")

#-------------- SUBNIVEL REGRESSION  - NON OUTLIERS --------------#
# data_no_smoker_outliers <- data %>%
#   group_by(smoker_yes) %>%
#   filter(!(smoker_yes == 0 & charges < quantile(charges, 0.25) - 1.5 * IQR(charges) | 
#              charges > quantile(charges, 0.75) + 1.5 * IQR(charges)))
# 
# data_smoker_no_obese_outliers <- data %>%
#   group_by(smoker_yes, obese) %>%
#   filter(!(smoker_yes == 1 & obese == 0 & charges < quantile(charges, 0.25) - 1.5 * IQR(charges) | 
#              charges > quantile(charges, 0.75) + 1.5 * IQR(charges)))
# 
# data_smoker_obese_outliers <- data %>%
#   group_by(smoker_yes, obese) %>%
#   filter(!(smoker_yes == 1 & obese == 1 & charges < quantile(charges, 0.25) - 1.5 * IQR(charges) | 
#              charges > quantile(charges, 0.75) + 1.5 * IQR(charges)))
# 
# dados <- bind_rows(data_no_smoker_outliers, data_smoker_no_obese_outliers, data_smoker_obese_outliers)
# #plot boxplot

remove_outliers <- function(df, group_var, value_var) {
  df %>%
    group_by({{group_var}}) %>%
    mutate(
      q1 = quantile({{value_var}}, 0.25),
      q3 = quantile({{value_var}}, 0.75),
      iqr = q3 - q1,
      lower_limit = q1 - 1.5 * iqr,
      upper_limit = q3 + 1.5 * iqr
    ) %>%
    filter({{value_var}} >= lower_limit & {{value_var}} <= upper_limit) %>%
    ungroup()
}

# Aplicar a função para remover outliers no grupo de não fumantes
dados <- remove_outliers(data, smoker_yes, charges)

ggplot(dados, aes(x = factor(smoker_yes), y = charges)) +
geom_boxplot() +
labs(title = "Boxplot de Charges por Fumantes e Não Fumantes",
     x = "Fumante (0 = Não, 1 = Sim)",
     y = "Charges") +
scale_fill_manual(values = c("blue", "red")) +  # Escolha cores personalizadas
theme_minimal()

#aplly multinível regresson
model <- lmer(charges ~   square_age + bmi + children + sex_male+ northeast+ northwest + southeast +southwest   + (1 | smoker_yes:obese), data = dados)


summary_model <- summary(model)
print(summary_model)
#saveRDS(model, file = "model.rds")
#capture.output(summary_model, file = "model_summary.txt")

aic_value <- AIC(model)
print(paste("AIC:", aic_value))

r2_value <- r2(model)
print(paste("R²:", r2_value[[1]]))

#write(paste("AIC:", aic_value, "\nR²:", r2_value), file = "model_metrics.txt")


# Obter os valores preditos
predicted <- predict(model, dados)

# Definir cores
#colors <- ifelse(data$smoker_yes == 0, "blue", 
#                 ifelse(data$obese == 1, "pink", "red"))
data_predicted <- data.frame(dados$charges, predicted,dados$smoker_yes, dados$obese)
# print(head(data_predicted))
colors <- ifelse(dados$obese == 0, "blue", "red")


# Gráfico ggplot com adição da linha y = x e legenda
# Gráfico ggplot com adição da linha y = x e legenda
ggplot(data_predicted, aes(x = dados.charges, y = predicted, color = factor(dados.smoker_yes), shape = factor(dados.obese))) +
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
print(coef(model))
# Calcular os resíduos
residuos <- residuals(model)

# Plotar os resíduos
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos", col = "skyblue")
