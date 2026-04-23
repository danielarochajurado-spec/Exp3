simulate_trials <- function(
    delay_small,
    A_l = 19.5,
    A_s = 6.5,
    step_init = 0.5,
    ITI = 1,
    response_latency = 2,
    max_trials = 8,
    max_reversals = 3,
    max_small = 6,
    strategy = "random"
) {
  
  valid_strategies <- c("always_large", "always_small", "random", "switch")
  if (!(strategy %in% valid_strategies)) stop("Invalid strategy")
  
  delay_large <- 3 * delay_small
  step <- step_init
  
  prev_choice <- NA
  reversals <- 0
  small_count <- 0
  cum_time <- 0
  
  out <- list()
  
  for (t in 1:max_trials) {
    
    # ---- decisión ----
    choice <- switch(strategy,
                     "always_large" = 1,
                     "always_small" = 0,
                     "random" = sample(c(0,1), 1),
                     "switch" = ifelse(t %% 2 == 0, 1, 0)
    )
    
    # ---- cambio de preferencia ----
    changed <- (!is.na(prev_choice) && choice != prev_choice)
    
    if (changed) {
      reversals <- reversals + 1
      step <- max(step - 2, 1)
    }
    
    if (choice == 0) small_count <- small_count + 1
    
    # ---- tiempos ----
    delay <- ifelse(choice == 1, delay_large, delay_small)
    play_time <- ifelse(choice == 1, A_l, A_s)
    
    trial_time <- response_latency + delay + play_time + ITI
    cum_time <- cum_time + trial_time
    
    # ---- guardar estado antes del update ----
    out[[t]] <- data.frame(
      trial = t,
      strategy = strategy,
      delay_small = delay_small,
      delay_large = delay_large,
      step = step,
      choice = choice,
      changed = changed,
      reversals = reversals,
      small_count = small_count,
      delay_used = delay,
      play_time = play_time,
      trial_time = trial_time,
      cum_time = cum_time
    )
    
    # ---- ajuste delay ----
    if (choice == 1) {
      delay_large <- delay_large + step
    } else {
      delay_large <- max(delay_small, delay_large - step)
    }
    
    # ---- ajuste dinámico del step ----
    if (!is.na(prev_choice) && choice == 1 && prev_choice == 1) {
      step <- min(step * 1.5, 4 * delay_small)
    }
    
    prev_choice <- choice
    
    # ---- criterios de parada ----
    if (reversals >= max_reversals && t >= 3) break
    if (small_count >= max_small) break
  }
  
  do.call(rbind, out)
}

results$total_time <- mapply(
  simulate_experiment,
  delay_small = results$delay_small,
  strategy = results$strategy
)
library(dplyr)

experimentDuration <- results %>%
  group_by(strategy) %>%
  summarise(
    total_time = sum(total_time)/60,
    .groups = "drop"
  )

library(dplyr)

delays <- c(0.5, 0.875, 1.531, 2.679, 4.689, 8.203)
strategies <- c("always_large", "always_small", "random", "switch")

df_trials <- expand.grid(
  delay_small = delays,
  strategy = strategies,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  do(simulate_trials(.$delay_small, strategy = .$strategy)) %>%
  ungroup()

simulate_game <- function(n_moves = 3600,
                          p = c(0.65, 0.30, 0.05),
                          values = c(3, 15, 45),
                          idle_ratio = 0.2) {
  
  # score por movimiento
  score_moves <- sample(values, n_moves, replace = TRUE, prob = p)
  gain <- sum(score_moves)
  
  # penalización por tiempo idle
  idle_time <- n_moves * 0.5 * idle_ratio
  loss <- idle_time / 0.5  # 1 punto cada 0.5s
  
  gain - loss
}

# Monte Carlo
set.seed(1)
res <- replicate(10000, simulate_game())

mean(res)
quantile(res, c(0.05, 0.5, 0.95))


getDelay <- function(n) {
  k <- 500
  exponent <- 1.5
  round(k * (n ^ exponent))
}
n <- 1:15
delays <- getDelay(n)
delays

