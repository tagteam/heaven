#' @title simulate_trial
#'
#' @description
#' Simulate data for a trial with baseline covariates, time-dependent treatment
#' variables, and outcomes.
#'
#' @author Christian Torp-Pedersen
#'
#' @param n Number of individuals.
#' @param followup Years of follow-up.
#' @param prop_Y Probability of outcome.
#' @param prop_censor Probability of censoring.
#' @param prop_compete Probability of competing risk.
#' @param prop_discontinue Probability of discontinuation.
#' @param prop_switch_if_discontinue Probability of switching treatment after discontinuation.
#' @param prop_T Probability of the random time-dependent variable `T`.
#' @param seed Optional random seed.
#'
#' @return
#' A list of three datasets corresponding to baseline, time-dependent variables
#' (`timedep`), and outcomes.
#'
#' @examples
#' library(data.table)
#' simulate_trial(seed = 2, n = 10)
#'
#' @export

simulate_trial <- function(n,
                           followup = 5,           
                           prop_Y = 0.50,
                           prop_censor = 0.30,
                           prop_compete = 0.20,
                           prop_discontinue = 0.30,
                           prop_switch_if_discontinue = 0.50,
                           prop_T = 0.50,
                           seed = 1234) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(abs(prop_Y + prop_censor + prop_compete - 1) < 1e-8)
  stopifnot(n >= 1)
  # ---- Baseline ----
  baseline <- data.table(
    ID = 1:n,
    sex = sample(c("F","M"), n, replace = TRUE),   # roughly equal
    age = runif(n, min = 50, max = 80)
  )
  baseline[, baseline_treatment := sample(c("A","B"), .N, replace = TRUE)]
  
  # ---- Discontinuation / switching ----
  disc <- data.table(ID = 1:n)
  disc[, discontinue := rbinom(.N, 1, prop_discontinue)]
  disc[discontinue == 1, disc_time := runif(.N, 0.01, followup - 0.01)]
  disc[discontinue == 0, disc_time := NA_real_]
  disc[, switch := ifelse(discontinue == 1,
                          rbinom(.N, 1, prop_switch_if_discontinue), 0)]
  
  # ---- Treatment intervals (A/B) ----
  treat_intervals <- list()
  
  for (i in 1:n) {
    trt0 <- baseline$baseline_treatment[i]
    disc_flag <- disc$discontinue[i]
    disc_time <- disc$disc_time[i]
    switch_flag <- disc$switch[i]
    
    if (disc_flag == 0) {
      # 70%: continue baseline treatment full 5 years
      treat_intervals[[i]] <- data.table(ID = i, start = 0, end = followup,
                                         variable = trt0, value = 1L)
    } else {
      # 30%: discontinuers
      if (switch_flag == 1) {
        # 15%: switch at disc_time
        other <- ifelse(trt0 == "A", "B", "A")
        treat_intervals[[i]] <- rbind(
          data.table(ID = i, start = 0, end = disc_time,
                     variable = trt0, value = 1L),
          data.table(ID = i, start = disc_time, end = followup,
                     variable = other, value = 1L)
        )
      } else {
        # 15%: discontinue completely at disc_time
        treat_intervals[[i]] <- data.table(ID = i, start = 0, end = disc_time,
                                           variable = trt0, value = 1L)
      }
    }
  }
  treat_intervals <- rbindlist(treat_intervals)
  setorder(treat_intervals, ID, start)
  
  # ---- T intervals ----
  T_intervals <- data.table()
  who_T <- if (prop_T > 0) sample(1:n, size = round(n * prop_T)) else integer(0)
  if (length(who_T) > 0) {
    T_intervals <- data.table(ID = who_T)
    T_intervals[, len := runif(.N, 0.8, 1.2)]
    T_intervals[, start := runif(.N, 0, followup - len)]
    T_intervals[, end := start + len]
    T_intervals <- T_intervals[end > start + 1e-8, .(ID, start, end)]
    T_intervals[, variable := "T"]
    T_intervals[, value := 1L]
  }
  
  # ---- Combined time-dependent dataset ----
  timedep <- rbindlist(list(
    treat_intervals[, .(ID, start, end, value, variable)],
    if (nrow(T_intervals) > 0) T_intervals[, .(ID, start, end, value, variable)] else NULL
  ), use.names = TRUE, fill = TRUE)
  setorder(timedep, ID, start)
  
  # ---- Outcome assignment ----
  evt_types <- sample(c("Y", "Censor", "Compete"), size = n, replace = TRUE,
                      prob = c(prop_Y, prop_censor, prop_compete))
  y_times <- rbeta(n, 2, 5) * followup        # skewed toward later times
  censor_times <- runif(n, 0, followup)
  compete_times <- runif(n, 0, followup)
  
  outcome_dt <- data.table(
    ID = 1:n,
    assigned_event = evt_types,
    time_Y = y_times,
    time_Censor = censor_times,
    time_Compete = compete_times
  )
  outcome_dt[, Y := ifelse(assigned_event == "Y", time_Y, NA_real_)]
  outcome_dt[, Censor := ifelse(assigned_event == "Censor", time_Censor, NA_real_)]
  outcome_dt[, Compete := ifelse(assigned_event == "Compete", time_Compete, NA_real_)]
  
  # ---- Apply treatment effect (A halves Y risk) ----
  Y_events <- outcome_dt[assigned_event == "Y", .(ID, Y_time = time_Y)]
  if (nrow(Y_events) > 0) {
    matched <- treat_intervals[Y_events, on = .(ID, start <= Y_time, end > Y_time), nomatch = 0L,
                               .(ID, trt_at_time = variable, Y_time)]
    onA <- matched[trt_at_time == "A"]
    if (nrow(onA) > 0) {
      prevent_flags <- runif(nrow(onA)) < 0.5
      if (any(prevent_flags)) {
        prevented <- onA[prevent_flags]
        for (r in seq_len(nrow(prevented))) {
          idr <- prevented$ID[r]
          t_r <- prevented$Y_time[r]
          outcome_dt[ID == idr & assigned_event == "Y", `:=`(assigned_event = "Censor",
                                                             Censor = t_r,
                                                             Y = NA_real_)]
        }
      }
    }
  }
  
  outcome_final <- outcome_dt[, .(ID, Y, Censor, Compete)]
  baseline_final <- baseline[, .(ID, sex, age, baseline_treatment)]
  
  return(list(
    baseline = baseline_final,
    outcome = outcome_final,
    timedep = timedep
  ))
}
