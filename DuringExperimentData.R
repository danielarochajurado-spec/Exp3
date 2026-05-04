# - librerias _------
library(tools)
library(tidyr)
library(GGally)
library(ggpubr)
library(ez)

library(stringr)
library(ggplot2)
library(writexl)
library(minpack.lm)
library(dplyr)
library(readxl)
library(performance)
library(ggeffects)
library(emmeans)

library(lme4)


theme_set(theme_classic())
# subir los datos ----
setwd("/Users/user/Desktop/Doctorado/TESIS/Experiment3/Resultados")
archivos <- list.files(pattern = "\\.xlsx$")

rawData  <- archivos[grepl("^DDGame_results_", archivos)]
participants <- str_extract(rawData, "(?<=results_)\\d+")

rawData <- lapply(seq_along(rawData), function(i) {
  df <- read_excel(rawData[i])
  df$participant <- participants[i]
  return(df)
})
rawData <- bind_rows(rawData)
rawData <- rawData %>%
  filter(!is.na(framing))


rawDataFree <- rawData %>%
  filter(choice != "forced") %>%
  mutate(
    choice = ifelse(choice == "large", 1, 0),
    condition = condition/1000)
    

df_prob <- rawDataFree %>%
  group_by(condition, framing) %>%
  summarise(
    p_choice = mean(choice),
    n = n(),
    .groups = "drop",
    se = sd(choice) / sqrt(n()),
    lower = p_choice - 1.96 * se,
    upper = p_choice + 1.96 * se
  )
ggplot(df_prob, aes(x = condition, y = p_choice, color = framing)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, alpha = 0.3)+
  geom_line(aes(group = framing)) +
  labs(
    title = "Choice large probability",
    y = "P(large)",
    color = "Framing"
  )+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  scale_x_continuous(breaks = seq(0, 8, by = 1))

# sacar punto de indiferencia ----
ip <- rawData %>%
  mutate(condition = condition / 1000) %>%
  group_by(participant, condition, framing) %>%
  summarise(ip = mean(delay_large) / 1000, .groups = "drop")



# eliminar datos no sistematicos ----
# process_participant <- function(df, key) {
#   
#   participant_id <- key$participant
#   framing_value <- key$framing
#   
#   NS_all <- tibble()   # acumulador
#   NSTotal <- 0
#   
#   repeat {
#     df <- df %>%
#       arrange(condition) %>%
#       mutate(
#         ip_last = lag(ip),
#         NS_flag = !is.na(ip_last) &
#           ip < ip_last &
#           (ip_last - ip) / ip_last > 0.19
#       )
#     
#     ns_rows <- df %>%
#       filter(NS_flag) %>%
#       mutate(
#         participant = participant_id,
#         framing = framing_value
#       )
#     
#     if (nrow(ns_rows) == 0) break
#     
#     NS_all <- bind_rows(NS_all, ns_rows)
#     NSTotal <- NSTotal + nrow(ns_rows)
#     
#     df <- df %>% filter(!NS_flag)
#   }
#   
#   list(
#     clean = df %>%
#       mutate(
#         participant = participant_id,
#         framing = framing_value,
#         NSTotal = NSTotal
#       ),
#     NS = NS_all
#   )
# }
# ipCleaning <- ipraw %>%
#   group_by(participant, framing) %>%
#   group_map(~ process_participant(.x, .y))
# 
# 
# ipCleaned <- bind_rows(lapply(ipCleaning, `[[`, "clean"))
# 
# ParticipantsRemoved <- ipCleaned %>%
#   group_by(participant, framing) %>%
#   summarise(NSTotal = max(NSTotal), .groups = "drop") %>%
#   filter(NSTotal >= 4)
# ipNS <- bind_rows(lapply(ipCleaning, `[[`, "NS"))
# 
# 
# 
# ip <- ipCleaned %>%
#   filter(NSTotal < 3)


plotIP <- ggplot(ip, aes(condition, ip, color = framing)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = framing, alpha = 0.3))+
  facet_wrap(~ participant, scales = "free") +
  labs(title = "Indifference Points", color = "Framing")

plotIP



# Ajuste linear por participante----

linearModel <- ip %>%
  group_by(participant, framing) %>%
  do(model = lm(ip ~ condition, data = .)) %>%
  mutate(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    R2 = summary(model)$r.squared,
  ) %>%
  dplyr::select(-model)

## intercepto ----

plotIntercept <- ggplot(linearModel, aes(framing, intercept)) +
  geom_point(alpha = 0.5) +
  geom_line(group = 1) +
  facet_wrap(~ participant, scales = "fixed") +
  labs(title = "intercept")

plotIntercept

ggplot(linearModel, aes(x = intercept)) +
  geom_histogram(fill = "white", color = "black", bins = 5)+
  facet_wrap(~ framing)+
  labs(title = " Intercept distribution")


## Pendiente ----

plotSlope <- ggplot(linearModel, aes(framing,slope)) +
  geom_point(alpha = 0.5) +
  geom_line(group = 1) +
  facet_wrap(~ participant, scales = "fixed") +
  labs(title = "slope")

plotSlope

ggplot(linearModel, aes(x = slope)) +
  geom_histogram(fill = "white", color = "black", bins = 5)+
  facet_wrap(~ framing)+
  labs(title = " Slope distribution")
  
# Hyperbola----

 RperMin <- ip %>%
   mutate(RperMin = 60/ ip)

 scale_minmax <- function(x) {
   round(range(x, na.rm = TRUE), 1)
 }
 
 Hyperbola <- ggplot(RperMin, aes(condition, RperMin, color = framing)) +
   geom_point(alpha = 0.2) +
   facet_wrap(~ participant, scales = "free")+
   labs(
     y = "Reinforcer per Minute",
     x = "Condition",
     color = "framing"
   )
 
  Hyperbola
  
## Ajuste ----
 Hyperbola <- RperMin %>%
  group_by(participant, framing) %>%
  group_modify(~ {
    df <- na.omit(.x)
    df <- df[df$RperMin >= 0, ]

    m <- try(
      nlsLM(
        RperMin ~ 60 / (1 + (k * ip)),
        data = df,
        start = list( k = 0.1)
      ),
      silent = TRUE
    )
    if (inherits(m, "try-error")) {
      return(tibble(

        k = NA,
        AIC = NA,
        BIC = NA,
        RSE = NA,
        R2_nl = NA
      ))
    }
    R2_nl <- 1 - (sum(residuals(m)^2) / sum((df$RperMin - mean(df$RperMin))^2))
    tibble(

      k      = coef(m)[["k"]],
      AIC    = AIC(m),
      BIC    = BIC(m),
      RSE    = sigma(m),
      R2_nl  = R2_nl
    )
  })

  
  plotK <- ggplot(Hyperbola, aes(framing,k)) +
    geom_point() +
    facet_wrap(~ participant, scales = "fixed") +
    labs(title = "k")
  
  plotK
  
  
  ggplot(Hyperbola, aes(x = k)) +
    geom_histogram(fill = "white", color = "black", bins = 8)+
    facet_wrap(~ framing)+
    labs(title = " k distribution")


# modelos BORRADOR TODO ----

 condition_sequence <- seq(min(rawDataFree$condition),
                  max(rawDataFree$condition),
                  length.out = 50)
  
  model <- glmer(choice ~ condition * framing + (1 + condition | participant), 
                 family = binomial, data = rawDataFree)
  
  library(lattice)
  dotplot(ranef(model, condVar = TRUE))
  
  emm <- emmeans(
    model,
    ~ framing | condition,
    at = list(condition = condition_sequence),
    type = "response"  
  )

  df_emm <- as.data.frame(emm)  
  
  ggplot(df_emm, aes(x = condition, y = prob, color = framing)) +
    geom_line() +
    geom_point(df_prob, aes ())
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = framing),
                alpha = 0.2, color = NA) +
    facet_wrap(~ framing)+
    labs(
      y = "P(elegir large)",
      x = "Condition (centrada)",
      title = "Curvas predichas del modelo"
    )

