#' Fits GAM model
#' @param hospital_name Character with hospital name that is consistent with the hospitals available in input_df column "hospital"
#' @param input_df Input data.frame for model. Data.frame needs columns hospital (`character`), date (`Date`), hour (`integer`)
#' @import mgcv
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom stats binomial
#' @importFrom stats na.omit
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @export


runLocationModel <- function(hospital_name, input_df){
  daily_hist_df <- input_df %>%
    group_by(.data$hospital) %>%
    mutate(start_date=min(.data$date),
           diffdate=as.numeric(.data$date-.data$start_date)) %>%
    filter(.data$hospital==hospital_name) %>%
    ungroup()

  pilot_ct <- daily_hist_df %>%
    count(.data$diffdate, .data$hospital, .data$hour) %>%
    mutate(getx=.data$diffdate/365,
           hospital=as.factor(.data$hospital)) %>%
    na.omit()

  system.time({
    mod_hr <- gam(
      list(
        n ~
          diffdate +
          s(getx) +
          s(hour),
        ~s(hour)
      ),
      family=gaulss,
      data=pilot_ct)
  })

  expected_hr <- predict(mod_hr)

  exp_hr_df <- pilot_ct %>%
    mutate(pred_mean=expected_hr[,1],
           pred_sd=.01 + exp(expected_hr[,2])) %>%
    mutate(q_lower=qnorm(0.10, .data$pred_mean, .data$pred_sd),
           q_lower = if_else(.data$q_lower < 0, 0, .data$q_lower),
           q_upper=qnorm(0.90, .data$pred_mean, .data$pred_sd))

  ### PREPARE FOR LOGISTIC MODEL
  date_hr <- pilot_ct %>%
    select(.data$diffdate, .data$hospital) %>%
    unite(.data$diffhos, c("diffdate", "hospital")) %>%
    distinct()


  log_df <- expand.grid(hour=1:24,
                        diffhos=date_hr$diffhos) %>%
    separate(.data$diffhos,
             c("diffdate", "hospital"),
             extra = "merge") %>%
    mutate(diffdate=as.integer(.data$diffdate)) %>%
    as_tibble()

  pilot_ct <- pilot_ct %>%
    mutate(hospital=as.character(.data$hospital))

  log_df_poss <- log_df %>%
    left_join(
      select(pilot_ct,
             .data$diffdate,
             .data$hospital,
             .data$hour,
             .data$n)
      ) %>%
    mutate(n=ifelse(is.na(n), 0, 1),
           n=ifelse(n >= 1, 1, 0)) %>%
    as_tibble()

  system.time({
    mod_log_hos <-
      gam(n ~
            s(hour),
          family=binomial,
          data=log_df_poss)
  })


  hospitals <- unique(as.character(pilot_ct$hospital))

  df_1y <- expand.grid(
    hour=1:24,
    hospital=hospitals
  )


  projected_dates <- pilot_ct %>%
    select(.data$hospital, .data$diffdate) %>%
    group_by(.data$hospital) %>%
    summarize(diffdate=max(.data$diffdate)) %>%
    left_join(df_1y) %>%
    group_split(.data$hospital) %>%
    map(~expand(.data, diffdate=seq(unique(.data$diffdate)+1, unique(.data$diffdate)+365))) %>%
    map2(.x=.data,
         .y=sort(unique(df_1y$hospital)),
         .f=function(x, y) mutate(x, hospital=y)) %>%
    bind_rows()

  df_1y <- df_1y %>%
    left_join(projected_dates) %>%
    mutate(getx=.data$diffdate/365) %>%
    as_tibble()


  ### now we make the predictions
  pred_hos <- predict(object=mod_hr,
                      newdata=df_1y,
                      type = "link")

  probs_hos <- predict(mod_log_hos,
                       df_1y,
                       type="response")

  df1y_pred <- df_1y %>%
    mutate(pred_mean=pred_hos[,1],
           pred_sd=0.01+exp(pred_hos[,2]),
           probs=probs_hos) %>%
    mutate(q_lower=qnorm(0.10, .data$pred_mean, .data$pred_sd),
           q_lower = if_else(.data$q_lower < 0, 0, .data$q_lower),
           q_upper=qnorm(0.90, .data$pred_mean, .data$pred_sd)) %>%
    mutate(shift=case_when(.data$hour>=7 & .data$hour <15 ~ 1,
                           .data$hour>=15 & .data$hour <23 ~ 2,
                           .data$hour>=23 ~ 3,
                           .data$hour>=0 & .data$hour <7 ~ 3))

  df1y_pred <- df1y_pred %>%
    as_tibble()

  obs <- daily_hist_df %>%
    mutate(start_date=min(.data$date),
           diffdate=as.numeric(.data$date-.data$start_date)) %>%
    select(.data$hospital, .data$date) %>%
    group_by(.data$hospital) %>%
    summarize(start_date=min(.data$date)) %>%
    left_join(pilot_ct) %>%
    mutate(date=.data$start_date+.data$diffdate) %>%
    select(-.data$getx, -.data$diffdate,
           -.data$start_date)

  list(
    preds=df1y_pred,
    obs=obs
  )
}


