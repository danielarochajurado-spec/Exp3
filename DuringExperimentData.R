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

theme_set(theme_classic())

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

# sacar punto de indiferencia ----
ip <- rawData %>%
  group_by(participant, condition, framing) %>%
  summarise(ip = mean(delay_large), .groups = 'drop')

plotIP <- ggplot(ip, aes(condition, ip, color = framing)) +
  geom_point() +
  geom_line(aes(group = framing))+
  facet_wrap(~ participant, scales = "free") +
  labs(title = "Raw Data", color = "Framing")

plotIP

linearModel <- ip %>%
   group_by(participant, framing) %>%
   do(model = lm(ip ~ condition, data = .)) %>%
   mutate(
     intercept = coef(model)[1],
     slope = coef(model)[2],
     R2 = summary(model)$r.squared,
   ) %>%
   dplyr::select(-model)




# eliminar datos no sistematicos ----
# process_participant <- function(df, key) {
#   
#   participant_id <- key$participant
#   
#   NS <- tibble()
#   NSTotal <- 0
#   
#   repeat {
#     df <- df %>%
#       arrange(condition) %>%
#       mutate(
#         ip_next = lead(ip),
#         NS = !is.na(ip_next) &
#           ip > ip_next &
#           (ip - ip_next) / ip_next > 0.19
#       )
#     
#     ns_rows <- df %>%
#       filter(NS) %>%
#       mutate(participant = participant_id)
#     
#     if (nrow(ns_rows) == 0) break
#     
#     NS <- bind_rows(NS, ns_rows)
#     NSTotal <- NSTotal + nrow(ns_rows)
#     
#     df <- df %>% filter(!NS)
#   }
#   
#   list(
#     clean = df %>%
#       mutate(
#         participant = participant_id,
#         NSTotal = NSTotal
#       ),
#     NS = NS
#   )
# }
# 
# ipCleaning <- ip %>%
#   group_by(participant, framing) %>%
#   group_map(~ process_participant(.x, .y))
# 
# ParticipantsRemoved <- ipCleaned %>%
#   group_by(participant, framing) %>%
#   summarise(NSTotal = max(NSTotal), .groups = "drop") %>%
#   filter(NSTotal >= 2)
# 
# ipCleaned <- bind_rows(lapply(ipCleaning, `[[`, "clean"))
# 
# ipNS <- bind_rows(lapply(ipCleaning, `[[`, "NS"))
# 
# 
# 
# ip <- ipCleaned %>%
#   filter(NSTotal < 2)
# 
# # ajustar curva de indiferencia (linear)
# linearModel <- ip %>%
#   group_by(participant, framing) %>%
#   
#   do(model = lm(ip ~ condition, data = .)) %>%
#   mutate(
#     intercept = coef(model)[1],
#     slope = coef(model)[2],
#     R2 = summary(model)$r.squared,
#     AIC = AIC(model),
#     BIC = BIC(model),
#     k = 2 / intercept
#   ) %>%
#   dplyr::select(-model)
# 
# labels <- linearModel %>%
#   mutate(
#     label = paste0(
#       "y = ",
#       round(slope, 2), "x + ",
#       round(intercept, 2),
#       "\nR² = ",
#       round(R2, 2)
#     )
#   )
# 
# plotln <- ggplot(ip, aes(condition, ip), col = framing) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, linetype = "dotted", color = "black") +
#   facet_wrap(~ participant, scales = "free") +
#   labs(
#     y = "Delay large",
#     x = "Delay small",
#     title = "Framing"
#   ) +
#   scale_y_continuous(breaks = function(x) pretty(x, n = 3)) +
#   scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
#   geom_text(
#     data = labelsGame,
#     aes(
#       x = -Inf,
#       y = Inf,
#       label = label
#     ),
#     inherit.aes = FALSE,
#     hjust = -0.1,
#     vjust = 1.1,
#     size = 3
#   )
# 
# plotln
# 
# # Hyperbola
# 
# RperMin <- ip %>%
#   mutate(RperMin = 60/ ip)
# 
# scale_minmax <- function(x) {
#   round(range(x, na.rm = TRUE), 1)
# }
# 
# Hyperbola <- ggplot(ip, aes(ip, RperMin)) +
#   geom_point() +
#   facet_wrap(~ participant, scales = "free")+
#   labs(
#     y = "Reinforcer per Minute",
#     x = "Condition"
#   )+
#   scale_y_continuous(breaks = function(x) pretty(x, n = 3))+
#   scale_x_continuous(breaks = function(x) pretty(x, n = 3))
# 
# Hyperbola
# 
# # Ajuste ----
# HyperbolaNivel1 <- ipGameRperMin %>% 
#   group_by(participant) %>%
#   group_modify(~ {
#     
#     df <- na.omit(.x)
#     df <- df[df$RperMin >= 0, ]
#     
#     m <- try(
#       nlsLM(
#         RperMin ~ 40 / (1 + (k * ip)),
#         data = df,
#         start = list( k = 0.1)
#       ),
#       silent = TRUE
#     )
#     
#     # Si falla el ajuste → devolver NAs
#     if (inherits(m, "try-error")) {
#       return(tibble(
# 
#         k = NA,
#         AIC = NA,
#         BIC = NA,
#         RSE = NA,
#         R2_nl = NA
#       ))
#     }
#     R2_nl <- 1 - (sum(residuals(m)^2) / sum((df$RperMin - mean(df$RperMin))^2))
#     tibble(
# 
#       k      = coef(m)[["k"]],
#       AIC    = AIC(m),
#       BIC    = BIC(m),
#       RSE    = sigma(m),
#       R2_nl  = R2_nl
#     )
#   })
# 
# 
# expoBolic <- ipGameRperMin %>% 
#   group_by(participant) %>%
#   group_modify(~ {
#     
#     df <- na.omit(.x)
#     df <- df[df$RperMin >= 0, ]
#     
#     m <- try(
#       nlsLM(
#         RperMin ~ a * exp(-b * ip),
#         data = df,
#         start = list(
#           a = max(df$RperMin),
#           b = 0.5
#         )
#       ),
#       silent = TRUE
#     )
#     
#     # Si falla el ajuste → devolver NAs
#     if (inherits(m, "try-error")) {
#       return(tibble(
#         a = NA,
#         b = NA,
#         AIC = NA,
#         BIC = NA,
#         RSE = NA,
#         R2_nl = NA
#       ))
#     }
#     
#     R2_nl <- 1 - (sum(residuals(m)^2) / sum((df$RperMin - mean(df$RperMin))^2))
#     
#     tibble(
#       a      = coef(m)[["a"]],
#       b      = coef(m)[["b"]],
#       AIC    = AIC(m),
#       BIC    = BIC(m),
#       RSE    = sigma(m),
#       R2_nl  = R2_nl
#     )
#   })
# 
# # Plot-----
# hyperbolic_fun <- function(x, k) {
#   40 / (1 + (k * x))
# }
# hyperboloid_fun <- function(x, k, s) {
#   300 / (1 + (k * x)^s)
# }
# bolic_game <- ipGameRperMin %>%
#   left_join(gameBolic, by = "participant") %>%
#   filter(!is.na(k)) %>%
#   group_by(participant) %>%
#   mutate(RperMin_pred = hyperbolic_fun(ip, k))
# 
# ggplot() +
#   geom_point(data = ipGameRperMin, aes(x = ip, y = RperMin), alpha = 0.3)+
#   lapply(unique(ipGameRperMin$participant), function(p) {
#     k_val <- gameBolic %>% filter(participant == p) %>% pull(k)
#     if (length(k_val) > 0 && !is.na(k_val)) {
#       stat_function(
#         fun = function(x) hyperbolic_fun(x, k = k_val),
#         linetype = "dashed",
#         alpha = 0.5
#       )
#     }
#   }) +
#   labs(
#     x = "Delay (s)",
#     y = "Reinforcer per minute",
#     title = "Experiential task"
#   ) 
# 
# 
# # --- Hypothetical ----
# 
# hypo <- lapply(seq_along(hypoFiles), function(i) {
#   df <- read_excel(hypoFiles[i])
#   df <- mutate(df, across(everything(), as.character))
#   df$participant <- participantsHypo[i]
#   return(df)
# })
# hypo <- bind_rows(hypo)
# 
# hypo <- hypo %>%
#   mutate(
#     delay_large = as.numeric(delay_large) / 1000,
#     condition = as.numeric(condition)/1000,
#     A_l= as.numeric(A_l)/1000,
#     A_s=as.numeric(A_s)/1000
#   )
# 
# ipHypoRaw <- hypo %>%
#   group_by(participant, condition) %>%
#   summarise(ip = mean(delay_large), .groups = 'drop')
# 
# plotHypo <- ggplot(ipHypoRaw, aes(condition, ip)) +
#   geom_point() +
#   facet_wrap(~ participant, scales = "free") 
# plotHypo
# 
# ipHypoCleaning <- ipHypoRaw %>%
#   group_by(participant) %>%
#   group_map(~ process_participant(.x, .y))
# 
# ipHypoCleaned <- bind_rows(lapply(ipHypoCleaning, `[[`, "clean"))
# 
# ipHypoNS <- bind_rows(lapply(ipHypoCleaning, `[[`, "NS"))
# 
# hypoParticipantsRemoved <- ipHypoCleaned %>%
#   distinct(participant, NSTotal) %>%
#   filter(NSTotal >= 2) %>%
#   pull(participant)
# 
# ipHypo <- ipHypoCleaned %>%
#   filter(!participant %in% hypoParticipantsRemoved)
# 
# plotHypoClean <- ggplot(ipHypo, aes(condition, ip)) +
#   geom_point() +
#   facet_wrap(~ participant, scales = "free")+
#   labs(
#     y = "Delay large",
#     x = "Delay Small",
#     title = "Hypothetical Task"
#   )+
#   scale_y_continuous(breaks = function(x) pretty(x, n = 3))+
#   scale_x_continuous(breaks = function(x) pretty(x, n = 3))
# 
# 
# 
# plotHypoClean
# 
# # 4. --- Ajustar con modelo linear (D_l vs D_s)----
# # Hipotética----
# hypoBolicln <- ipHypo %>%
#   group_by(participant) %>%
#   do(model = lm(ip ~ condition, data = .)) %>%
#   mutate(
#     intercept = coef(model)[1],
#     slope = coef(model)[2],
#     R2 = summary(model)$r.squared,
#     AIC = AIC(model),
#     BIC = BIC(model),
#     k = 2 / intercept
#   ) %>%
#   ungroup() %>%         
#   dplyr::select(-model)
# 
# labelsHypo <- hypoBolicln %>%
#   mutate(
#     label = paste0(
#       "y = ",
#       round(slope, 2), "x + ",
#       round(intercept, 2),
#       "\nR² = ",
#       round(R2, 2)
#     )
#   )
# 
# plotHypoln <- ggplot(ipHypo, aes(condition, ip)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, linetype = "dotted", color = "black") +
#   facet_wrap(~ participant, scales = "free") +
#   labs(
#     y = "Delay large",
#     x = "Delay small",
#     title = "Hypothetical Task"
#   ) +
#   scale_y_continuous(breaks = function(x) pretty(x, n = 3)) +
#   scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
#   geom_text(
#     data = labelsHypo,
#     aes(
#       x = -Inf,
#       y = Inf,
#       label = label
#     ),
#     inherit.aes = FALSE,
#     hjust = -0.1,
#     vjust = 1.1,
#     size = 1.7
#   )
# 
# plotHypoln
# 
# 
# # graficar lineas----
# 
# hypotheticalPlot<- ggplot(ipHypo, aes(x = condition, y = ip)) +
#   geom_point(size = 2) +
#   labs(
#     x = "Condition (seconds)",
#     y = "Indifference Point (seconds)",
#     title = "Hypothetical task"
#   ) +
#   theme_classic()
# line_layers <- lapply(unique(ipHypo$participant), function(p_id) {
#   intercept <- hypoBolicln %>% filter(participant == p_id) %>% pull(intercept)
#   slope <- hypoBolicln %>% filter(participant == p_id) %>% pull(slope)
#   stat_function(
#     fun = linear_fun(intercept, slope),
#     color = "black",
#     linetype = "dashed",
#     alpha = 0.5,
#     inherit.aes = FALSE
#   )
# })
# for (layer in line_layers) {
#   hypotheticalPlot<-hypotheticalPlot+ layer
# }
# 
# hypotheticalPlot
# 
# # Transformar a RperMin----
# ipHypoRperMin <- ipHypo%>%
#   mutate(RperMin = 60/condition)
# 
# plotipHypoRperMin <- ggplot(ipHypoRperMin, aes(ip, RperMin)) +
#   geom_point() +
#   facet_wrap(~ participant, scales = "free")+
#   labs(
#     y = "Reinforcer per Minute",
#     x = "Indifference Point",
#     title = "Experiential Task"
#   )+
#   scale_y_continuous(breaks = function(x) pretty(x, n = 3))+
#   scale_x_continuous(breaks = function(x) pretty(x, n = 3))
# 
# plotipHypoRperMin
# 
# 
# 
# #unir interceptos---- 
# intercept_all <- gameBolicln %>% 
#   dplyr::select(participant, Game = intercept) %>%
#   left_join(
#     hypoBolicln %>% 
#       dplyr::select(participant, Hypothetical = intercept),
#     by = "participant"
#   )
# 
# # 3. --- Ajustes no lineares ----
# # 3.1. Hipotética----
# # 3.1.1 Hiperbolica----
# hypoBolic <- ipHypoRperMin %>% 
#   group_by(participant) %>%
#   group_modify(~ {
#     df <- na.omit(.x)
#     df <- df[df$RperMin >= 0, ]
#     m <- try(
#       nlsLM(
#         RperMin ~ 180 / (1 + (k * ip)),
#         data = df,
#         start = list(k = 0.1), control = nls.lm.control(maxiter = 200)
#       ),
#       silent = TRUE
#     )
#     
#     # Si falla el ajuste → devolver NAs
#     if (inherits(m, "try-error")) {
#       return(tibble(
#         k = NA,
#         AIC = NA,
#         BIC = NA,
#         RSE = NA,
#         R2_nl = NA
#       ))
#     }
#     R2_nl <- 1 - (sum(residuals(m)^2) / sum((df$RperMin - mean(df$RperMin))^2))
#     tibble(
#       k      = coef(m)[["k"]],
#       AIC    = AIC(m),
#       BIC    = BIC(m),
#       RSE    = sigma(m),
#       R2_nl  = R2_nl
#     )
#   })
# bolic_hypo <- ipHypoRperMin %>%
#   left_join(hypoBolic, by = "participant") %>%
#   filter(!is.na(k)) %>%
#   group_by(participant) %>%
#   mutate(RperMin_pred = hyperbolic_fun(ip, k))
# 
# ggplot() +
#   geom_point(data = ipHypoRperMin, aes(x = ip, y = RperMin), alpha = 0.4)+
#   lapply(unique(ipHypoRperMin$participant), function(p) {
#     k_val <- hypoBolic %>% filter(participant == p) %>% pull(k)
#     if (length(k_val) > 0 && !is.na(k_val)) {
#       stat_function(
#         fun = function(x) hyperbolic_fun(x, k = k_val),
#         linetype = "dashed",
#         alpha = 0.5
#       )
#     }
#   }) +
#   labs(
#     x = "Delay (s)",
#     y = "Reinforcer per minute",
#     title = "Hypothetical task"
#   ) 
# 
# 
# # Distribución -------
# intercepts <-gameBolicln%>%
#   dplyr::select(participant, Experiential = intercept) %>%
#   left_join(
#     hypoBolicln%>%
#       dplyr::select(participant, Hypothetical = intercept),
#     by= "participant")
# 
# slopes <-gameBolicln%>%
#   dplyr::select(participant, Experiential = slope) %>%
#   left_join(
#     hypoBolicln%>%
#       dplyr::select(participant, Hypothetical = slope),
#     by= "participant")
# 
# interceptsLong <- bind_rows(
#   gameBolicln  %>%dplyr:: select(participant, intercept) %>% mutate(task = "Experiential"),
#   hypoBolicln  %>%dplyr:: select(participant, intercept) %>% mutate(task = "Hypothetical")
# )
# slopesLong <- bind_rows(
#   gameBolicln  %>%dplyr:: select(participant, slope) %>% mutate(task = "Experiential"),
#   hypoBolicln  %>%dplyr:: select(participant, slope) %>% mutate(task = "Hypothetical")
# )
# 
# ggplot(interceptsLong, aes(x = task, y = intercept)) +
#   geom_boxplot(fill = "white", color = "black")+
#   labs(title = "Intercept")
# 
# ggplot(slopesLong, aes(x = task, y = slope)) +
#   geom_boxplot(fill = "white", color = "black")+
#   labs(title = "Slope")
# 
# ggplot(intercepts, aes(x = Experiential)) +
#   geom_histogram(fill = "white", color = "black", bins = 25)+
#   labs(title = "Game Intercept distribution")
# 
# ggplot(intercepts, aes(x = Hypothetical)) +
#   geom_histogram(fill = "white", color = "black", bins = 25)+
#   labs(title = "Hypothetical Intercept distribution")
# 
# ggplot(slopes, aes(x = Experiential)) +
#   geom_histogram(fill = "white", color = "black", bins = 25) +
#   labs(title = "Game Slope distribution")
# ggplot(slopes, aes( x= Hypothetical))+
#   geom_histogram(fill= "white", color = "black", bins = 25)+
#   labs(title = "Hypothetical slope distribution")