#### PREAMBLE ####
library(tidyverse)
library(lubridate)
library(stargazer)
library(sandwich)
library(fixest)
library(scales)
library(ggtext)
library(lmtest)
library(broom)
library(zoo)
source("theme_materials/theme_stigler.R")
`%notin%` <- Negate(`%in%`)

##### REFERENCE VALUES #####
today_dt <- today()

wmata_digital_interval <- interval(
    ymd("2023-01-19"), ymd("2023-02-15")
)
wmata_static_interval <- interval(
    ymd("2023-01-16"), ymd("2023-02-12")
)
wmata_general_interval <- interval(
    ymd("2023-01-16"), ymd("2023-02-15")
)
economist_interval <- interval(
    ymd("2021-04-01"), ymd("2021-04-30")
)
vox_interval <- interval(
    ymd("2021-06-01"), ymd("2021-06-15")
)



#### READING DATA IN ####
dmv_data_in <- read_csv(
    "wmataCampaignAnalysis/states_cities_DMV.csv"
)

episodes_core_data_in <- read_csv(
    paste(
        "episodes_downloads-", today_dt, ".csv",
        sep = ""
    )
)

episode_locations_data_in <- read_csv(
    paste(
        "us_cities_episode_locations-", today_dt, ".csv",
        sep = ""
    )
)

#### REFERENCE FUNCTIONS ####

recent_n_episode_ids <- function(
    n = 0,
    release_dates_df
) {
    episode_ids <- release_dates_df %>%
        dplyr::arrange(
            dplyr::desc(release_date)
        ) %>%
        head(n) %>%
        dplyr::pull(episode_id)

    return(episode_ids)
}

released_since_episode_ids <- function(
    cutoff_date_str,
    release_dates_df
) {
    episode_ids <- release_dates_df %>%
        dplyr::arrange(
            dplyr::desc(release_date)
        ) %>%
        dplyr::filter(
            release_date >= lubridate::ymd(cutoff_date_str)
        ) %>%
        dplyr::pull(episode_id)

    return(episode_ids)
}

released_between_episode_ids <- function(
    cutoff_period_start_str,
    cutoff_period_end_str,
    release_dates_df
) {
    episode_ids <- release_dates_df %>%
        dplyr::arrange(
            dplyr::desc(release_date)
        ) %>%
        dplyr::filter(
            release_date %within% lubridate::interval(
                lubridate::ymd(cutoff_period_start_str),
                lubridate::ymd(cutoff_period_end_str)
            )
        ) %>%
        dplyr::pull(episode_id)

    return(episode_ids)
}



#### REFERENCES DFs  ####
    ######## Reference info:
    ######## WMATA Digital (train platforms) : 01/19/2023 -- 02/15/2023
    ######## WMATA Static (train cars): 01/16/2023 -- 02/12/2023
    ######## Economist Media Group 04-01-2021 -- 04-30-2021
    ######## Vox Media: 06-01-2021 -- 06-15-2021

titles_ids_df <- episodes_core_data_in %>%
    select(episode_id, title) %>%
    distinct()

release_dates_df <- episodes_core_data_in %>%
    select(episode_id, interval) %>%
    mutate(interval = as.Date(interval)) %>%
    group_by(episode_id) %>%
    summarise(
        release_date = min(interval)
    )

campaign_dates_df <- episodes_core_data_in %>%
    select(interval) %>%
    rename(
        "date" = "interval"
    ) %>%
    distinct() %>%
    mutate(
        date = as.Date(date),
        any_ad_strict = ifelse(
            date %within% interval(ymd("20230116"), ymd("20230215")),
            1, 0
        ),
        digital_ad_strict = ifelse(
            date %within% interval(ymd("20230119"), ymd("20230215")),
            1, 0
        ),
        static_ad_strict = ifelse(
            date %within% interval(ymd("20230116"), ymd("20230212")),
            1, 0
        ),
        static_ad_lax = ifelse(
            date %within% interval(ymd("20230116"), today_dt),
            1, 0
        )
    )

dmv_cities_df <- dmv_data_in %>%
    filter(
        state_id %in% c(
            4138106, # DC
            6254928, # VA
            4361885 # MD
        )
    ) %>%
    rename(
        "city_name" = "name",
        "city_id" = "id",
        "relevant_msa" = "within relevant MSA"
    ) %>%
    select(
        city_name, city_id, state_id, relevant_msa
    )

#### CONTENT-FUL DATA ####
daily_downloads_df <- episodes_core_data_in %>%
    rename(
        "date" = "interval",
        "daily_downloads" = "downloads_total"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    group_by(episode_id) %>%
    arrange(date) %>%
    mutate(
        days_since_release = row_number(),
        cumulative_downloads = cumsum(daily_downloads)
    )

podcast_daily_downloads_df <- episodes_core_data_in %>%
    rename(
        "date" = "interval",
        "daily_downloads" = "downloads_total"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    group_by(date) %>%
    mutate(
        gap_to_release = as.integer(
            interval(start = date, end = release_date)
        )
    ) %>%
    arrange(date, desc(gap_to_release)) %>%
    mutate(
        ordered_from_closest_release = row_number(),
        active_back_expired_catalog = case_when(
            ordered_from_closest_release == 1 ~ 1,
            (ordered_from_closest_release >= 2) & (ordered_from_closest_release <= 6) ~ 2,
            ordered_from_closest_release >= 7 ~ 3
        )
    ) %>%
    select(
        date, daily_downloads, active_back_expired_catalog
    ) %>%
    group_by(date) %>%
    mutate(
        all_daily_downloads = sum(daily_downloads),
    ) %>%
    group_by(date, active_back_expired_catalog) %>%
    mutate(
        grouped_daily_downloads = sum(daily_downloads)
    ) %>%
    distinct(
        date, active_back_expired_catalog,
        all_daily_downloads, grouped_daily_downloads
    ) %>%
    arrange(active_back_expired_catalog, date) %>%
    group_by(active_back_expired_catalog) %>%
    mutate(
        avg_grouped_daily_downloads_14 = rollmean(
            grouped_daily_downloads,
            k = 14,
            fill = NA,
            align = "right"
        ),
        avg_all_daily_downloads_14 = rollmean(
            all_daily_downloads,
            k = 14,
            fill = NA,
            align = "right"
        )
    )

episode_locations_downloads_df <- episode_locations_data_in %>%
    left_join(
        dmv_cities_df %>%
            select(-"city_name"),
        by = c("city_id", "state_id"),
        multiple = "all"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    pivot_longer(
        cols = -c(
            "city_name",
            "city_id",
            "state_id",
            "episode_id",
            "relevant_msa",
            "release_date"
        ),
        names_to = "date",
        values_to = "cumulative_downloads"
    ) %>%
    mutate(
        date = ymd(date),
        relevant_msa = ifelse(
            is.na(relevant_msa),
            0, relevant_msa
        )
    ) %>%
    group_by(city_id, episode_id) %>%
    mutate(
        days_since_release = as.numeric(as.period(
            interval(release_date, date),
            unit = "days"
        )) / as.numeric(days(1)),
        log_days_since_release = log(days_since_release)
    )

### REGRESSION ###
##### SUMMARY STATISTICS #####
episode_level_summ_df <- daily_downloads_df %>%
    select(
        episode_id, daily_downloads, days_since_release, cumulative_downloads
    ) %>%
    group_by(episode_id) %>%
    mutate(
        downloads_t_14 = ifelse(
            days_since_release == 14,
            cumulative_downloads,
            NA
        ),
        downloads_t_28 = ifelse(
            days_since_release == 28,
            cumulative_downloads,
            NA
        )
    ) %>%
    summarise(
        days_since_release = max(days_since_release, na.rm = TRUE),
        downloads_t_14 = max(downloads_t_14, na.rm = TRUE),
        downloads_t_28 = max(downloads_t_28, na.rm = TRUE)
    ) %>%
    mutate(
        downloads_t_14 = ifelse(
            downloads_t_14 == -Inf,
            NA, downloads_t_14
        ),
        downloads_t_28 = ifelse(
            downloads_t_28 == -Inf,
            NA, downloads_t_28
        )
    )

stargazer(
    as.data.frame(episode_level_summ_df),
    summary.stat = c("min", "p25", "sd", "mean", "median", "p75", "max", "n"),
    digits = 1,
    style = "qje"
)

recent_20_episode_level_summ_df <- daily_downloads_df %>%
    select(
        episode_id, daily_downloads, days_since_release, cumulative_downloads
    ) %>%
    filter(
        episode_id %in% recent_n_episode_ids(
            n = 20,
            release_dates_df
        )
    ) %>%
    group_by(episode_id) %>%
    mutate(
        downloads_t_14 = ifelse(
            days_since_release == 14,
            cumulative_downloads,
            NA
        ),
        downloads_t_28 = ifelse(
            days_since_release == 28,
            cumulative_downloads,
            NA
        )
    ) %>%
    summarise(
        days_since_release = max(days_since_release, na.rm = TRUE),
        downloads_t_14 = max(downloads_t_14, na.rm = TRUE),
        downloads_t_28 = max(downloads_t_28, na.rm = TRUE)
    ) %>%
    mutate(
        downloads_t_14 = ifelse(
            downloads_t_14 == -Inf,
            NA, downloads_t_14
        ),
        downloads_t_28 = ifelse(
            downloads_t_28 == -Inf,
            NA, downloads_t_28
        ),
    )

stargazer(
    as.data.frame(recent_20_episode_level_summ_df),
    summary.stat = c("min", "p25", "sd", "mean", "median", "p75", "max", "n"),
    digits = 1,
    style = "qje"
)


##### NAIVE T=14 EPISODE-LEVEL OLS #####
t_14_ols_df <- daily_downloads_df %>%
    select(
        episode_id, date, daily_downloads,
        days_since_release, cumulative_downloads,
        release_date
    ) %>%
    ungroup() %>%
    mutate(
        downloads_t_14 = ifelse(
            days_since_release == 14,
            cumulative_downloads, NA
        )
    ) %>%
    filter(!is.na(downloads_t_14)) %>%
    mutate(
        trailing5_t_14_avg = rollmean(
            downloads_t_14,
            k = 5,
            fill = NA,
            align = "left"
        ),
        aired_wmata_general_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_general_interval
            ),
            1, 0
        ),
        aired_wmata_digital_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_digital_interval
            ),
            1, 0
        ),
        aired_wamta_static_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_static_interval
            ),
            1, 0
        ),
        aired_economist_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                economist_interval
            ),
            1, 0
        ),
        aired_vox_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                vox_interval
            ),
            1, 0
        )
    ) %>%
    group_by(episode_id) %>%
    mutate(
        aired_first_ad_experiment = ifelse(
            1 %in% c(aired_economist_ad, aired_vox_ad),
            1, 0
        ),
        aired_any_ad_experiment = ifelse(
            1 %in% c(aired_wmata_general_ad, aired_first_ad_experiment),
            1, 0
        )
    )

t_14_ols_trailing_only <- lm(
    downloads_t_14 ~ trailing5_t_14_avg,
    data = t_14_ols_df
)
t_14_ols_trailing_only_RSE <- sqrt(
    diag(vcovHC(t_14_ols_trailing_only))
)

t_14_ols_trailing_wmata_general <- lm(
    downloads_t_14 ~ trailing5_t_14_avg + aired_wmata_digital_ad,
    data = t_14_ols_df
)
t_14_ols_trailing_wmata_general_RSE <- sqrt(
    diag(vcovHC(t_14_ols_trailing_wmata_general))
)

t_14_ols_trailing_first_ad_experiment <- lm(
    downloads_t_14 ~ trailing5_t_14_avg + aired_wmata_digital_ad + aired_first_ad_experiment,
    data = t_14_ols_df
)
t_14_ols_trailing_first_ad_experiment_RSE <- sqrt(
    diag(vcovHC(t_14_ols_trailing_first_ad_experiment))
)

t_14_ols_models <- list(
    t_14_ols_trailing_only,
    t_14_ols_trailing_wmata_general,
    t_14_ols_trailing_first_ad_experiment
)
t_14_ols_RSEs <- list(
    t_14_ols_trailing_only_RSE,
    t_14_ols_trailing_wmata_general_RSE,
    t_14_ols_trailing_first_ad_experiment_RSE
)


stargazer(
    t_14_ols_models,
    dep.var.labels = c("Cumulative downloads ($t=14$)"),
    covariate.labels = c("Trailing Avg. ($n=5$)", "WMATA Ad.", "Econ./Vox Ad."),
    omit.stat = c("ser", "f", "rsq"),
    no.space = TRUE,
    se = t_14_ols_RSEs
)


##### NAIVE T=28 EPISODE-LEVEL OLS #####
t_28_ols_df <- daily_downloads_df %>%
    select(
        episode_id, date, daily_downloads,
        days_since_release, cumulative_downloads,
        release_date
    ) %>%
    ungroup() %>%
    mutate(
        downloads_t_14 = ifelse(
            days_since_release == 14,
            cumulative_downloads, NA
        ),
        downloads_t_28 = ifelse(
            days_since_release == 28,
            cumulative_downloads, NA
        )
    ) %>%
    group_by(episode_id) %>%
    mutate(
        downloads_t_14 = max(downloads_t_14, na.rm = TRUE),
        downloads_t_28 = max(downloads_t_28, na.rm = TRUE),
        downloads_t_14 = ifelse(
            downloads_t_14 == -Inf,
            NA, downloads_t_14
        ),
        downloads_t_28 = ifelse(
            downloads_t_28 == -Inf,
            NA, downloads_t_28
        )
    ) %>%
    distinct(episode_id, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(
        trailing5_t_14_avg = rollmean(
            downloads_t_14,
            k = 5,
            fill = NA,
            align = "left"
        ),
        trailing5_t_28_avg = rollmean(
            downloads_t_28,
            k = 5,
            fill = NA,
            align = "left"
        ),
        aired_wmata_general_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_general_interval
            ),
            1, 0
        ),
        aired_wmata_digital_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_digital_interval
            ),
            1, 0
        ),
        aired_wamta_static_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                wmata_static_interval
            ),
            1, 0
        ),
        aired_economist_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                economist_interval
            ),
            1, 0
        ),
        aired_vox_ad = ifelse(
            interval(release_date, date) %>% int_overlaps(
                vox_interval
            ),
            1, 0
        )
    ) %>%
    group_by(episode_id) %>%
    mutate(
        aired_first_ad_experiment = ifelse(
            1 %in% c(aired_economist_ad, aired_vox_ad),
            1, 0
        ),
        aired_any_ad_experiment = ifelse(
            1 %in% c(aired_wmata_general_ad, aired_first_ad_experiment),
            1, 0
        )
    )

t_28_ols_trailing_only <- lm(
    downloads_t_28 ~ trailing5_t_28_avg,
    data = t_28_ols_df
)
t_28_ols_trailing_only_RSE <- sqrt(
    diag(vcovHC(t_28_ols_trailing_only))
)

t_28_ols_trailing_wmata_general <- lm(
    downloads_t_28 ~ trailing5_t_28_avg + aired_wmata_digital_ad,
    data = t_28_ols_df
)
t_28_ols_trailing_wmata_general_RSE <- sqrt(
    diag(vcovHC(t_28_ols_trailing_wmata_general))
)

t_28_ols_trailing_first_ad_experiment <- lm(
    downloads_t_28 ~ trailing5_t_28_avg + aired_wmata_digital_ad + aired_first_ad_experiment,
    data = t_28_ols_df
)
t_28_ols_trailing_first_ad_experiment_RSE <- sqrt(
    diag(vcovHC(t_28_ols_trailing_first_ad_experiment))
)

t_28_ols_trailing_autoreg <- lm(
    downloads_t_28 ~ trailing5_t_28_avg + downloads_t_28,
    data = t_28_ols_df
)
t_28_ols_trailing_autoreg_RSE <- sqrt(
    diag(vcovHC(t_28_ols_trailing_autoreg))
)

t_28_ols_trailing_twice_autoreg <- lm(
    downloads_t_28 ~ trailing5_t_14_avg + trailing5_t_28_avg,
    data = t_28_ols_df
)
t_28_ols_trailing_twice_autoreg_RSE <- sqrt(
    diag(vcovHC(t_28_ols_trailing_twice_autoreg))
)

t_28_ols_models <- list(
    t_28_ols_trailing_only,
    t_28_ols_trailing_wmata_general,
    t_28_ols_trailing_first_ad_experiment,
    t_28_ols_trailing_autoreg,
    t_28_ols_trailing_twice_autoreg
)
t_28_ols_RSEs <- list(
    t_28_ols_trailing_only_RSE,
    t_28_ols_trailing_wmata_general_RSE,
    t_28_ols_trailing_first_ad_experiment_RSE,
    t_28_ols_trailing_autoreg_RSE,
    t_28_ols_trailing_twice_autoreg_RSE
)

stargazer(
    t_28_ols_models,
    dep.var.labels = c("Cumulative downloads ($t=28$)"),
    covariate.labels = c("Trailing Avg. ($n=5, t=14$)", "Trailing Avg. ($n=5, t=28$)", "WMATA Ad.", "Econ./Vox Ad."),
    omit.stat = c("ser", "f", "rsq"),
    no.space = TRUE,
    se = t_28_ols_RSEs
)



##### DAILY DOWNLOADS DISCONTINUITY/KINK ####
daily_slope_kink_df <- daily_downloads_df %>%
    mutate(
        in_wmata_general_ad = ifelse(
            date %within% wmata_general_interval,
            1, 0
        ),
        in_wmata_digital_ad = ifelse(
            date %within% wmata_digital_interval,
            1, 0
        ),
        in_wmata_static_ad = ifelse(
            date %within% wmata_static_interval,
            1, 0
        ),
        in_economist_ad = ifelse(
            date %within% economist_interval,
            1, 0
        ),
        in_vox_ad = ifelse(
            date %within% vox_interval,
            1, 0
        ),
        in_first_experiment = ifelse(
            1 %in% c(in_vox_ad, in_economist_ad),
            1, 0
        ),
        in_any_ad = ifelse(
            1 %in% c(in_first_experiment, in_wmata_general_ad),
            1, 0
        )
    ) %>%
    group_by(episode_id) %>%
    mutate(
        log_days_since_release = log(days_since_release)
    )


daily_slope_kink_baseline <- lm(
    cumulative_downloads ~ log_days_since_release,
    data = daily_slope_kink_df
)

daily_slope_kink_advertisement_binary_any_ad <- lm(
    cumulative_downloads ~ log_days_since_release + in_any_ad,
    data = daily_slope_kink_df
)

daily_slope_kink_advertisement_binary_any_interaction <- lm(
    cumulative_downloads ~ log_days_since_release + in_any_ad + log_days_since_release*in_any_ad,
    data = daily_slope_kink_df
)

daily_slope_kink_advertisement_binary_wmata_general_interaction <- lm(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release*in_wmata_general_ad,
    data = daily_slope_kink_df
)

recent_20_daily_slope_kink_advertisement_binary_wmata_general_interaction <- lm(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release*in_wmata_general_ad,
    data = daily_slope_kink_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(n = 20, release_dates_df)
        )
)

recent_20_daily_slope_kink_advertisement_fe <- feols(
    cumulative_downloads ~ log_days_since_release + in_wmata_digital_ad + log_days_since_release:in_wmata_general_ad | episode_id,
    data = daily_slope_kink_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(n =  20, release_dates_df)
        )
)

feols(
    cumulative_downloads ~ log_days_since_release + in_wmata_digital_ad + log_days_since_release:in_wmata_general_ad | episode_id,
    data = daily_slope_kink_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(n =  25, release_dates_df)
        )
)

feols(
    cumulative_downloads ~ log_days_since_release + in_wmata_digital_ad + log_days_since_release:in_wmata_general_ad | episode_id,
    data = daily_slope_kink_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(n =  39, release_dates_df),
            episode_id %notin% recent_n_episode_ids(n = 9, release_dates_df)
        )
)

daily_kink_results_df <- data.frame()
daily_kink_no_intercept_results_df <- data.frame()

for (episode in daily_slope_kink_df$episode_id %>% unique()) {
    episode_title <- titles_ids_df %>%
        filter(episode_id == episode) %>%
        select(title) %>%
        pull()

    df <- daily_slope_kink_df %>%
        filter(
            episode_id == episode
        )
    # print("#####################################################")
    # print(episode_title)

    daily_model <- lm(
        cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release:in_wmata_general_ad,
        data = df
    )

    daily_model_no_intercept <- lm(
        cumulative_downloads ~ log_days_since_release + log_days_since_release:in_wmata_general_ad,
        data = df
    )

    daily_RSE_results <- coeftest(
        daily_model,
        vcov. = vcovHC(daily_model, type = "HC2")
    ) %>%
    tidy() %>%
    mutate(
        episode_id = episode,
        title = episode_title
    )

    # coeftest(
    #     daily_model,
    #     vcov. = vcovHC(daily_model, type = "HC2")
    # ) %>%
    # print()
    # print("--------------------------------------------------")
    # coeftest(
    #     daily_model_no_intercept,
    #     vcov. = vcovHC(daily_model_no_intercept, type = "HC2")
    # ) %>%
    # print()

    daily_no_intercept_RSE_results <- coeftest(
        daily_model_no_intercept,
        vcov. = vcovHC(daily_model_no_intercept, type = "HC2")
    ) %>%
    tidy() %>%
    mutate(
        episode_id = episode,
        title = episode_title
    )


    daily_kink_results_df <- daily_kink_results_df %>%
        rbind(daily_RSE_results)

    daily_kink_no_intercept_results_df <- daily_kink_no_intercept_results_df %>%
        rbind(daily_no_intercept_RSE_results)
}

daily_kink_results_df <- daily_kink_results_df %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    )

daily_kink_no_intercept_results_df <- daily_kink_no_intercept_results_df %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    )


alltime_daily_slope_kink_advertisement_fe <- feols(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release:in_wmata_general_ad | episode_id,
    data = daily_slope_kink_df 
)

wmata_treated_only_daily_slope_kink_advertisement_binary_wamata_general_interaction <- lm(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release*in_wmata_general_ad,
    data = daily_slope_kink_df %>%
        filter(
            episode_id %in% released_between_episode_ids(
            cutoff_period_start_str = "2022-11-01",
            cutoff_period_end_str = "2023-01-01",
            release_dates_df
            )
        )
)

tswift_model <- lm(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + log_days_since_release:in_wmata_general_ad,
        data = daily_slope_kink_df %>%
            filter(
                title == "Taylor Swift, Ticketmaster, and Chokepoint Capitalism with Cory Doctorow"
            )
)

tswift_fitted_df <- data.frame(
    cumulative_downloads_pred = predict(tswift_model, daily_slope_kink_df %>%
            filter(
                title == "Taylor Swift, Ticketmaster, and Chokepoint Capitalism with Cory Doctorow"
            )),
    log_days_since_release = daily_slope_kink_df %>%
            filter(
                title == "Taylor Swift, Ticketmaster, and Chokepoint Capitalism with Cory Doctorow"
            ) %>%
            select(log_days_since_release) %>%
            pull(),
    in_wmata_general_ad = daily_slope_kink_df %>%
            filter(
                title == "Taylor Swift, Ticketmaster, and Chokepoint Capitalism with Cory Doctorow"
            ) %>%
            select(in_wmata_general_ad) %>%
            pull()
)

ggplot(
    daily_slope_kink_df %>%
        filter(
            title == "Taylor Swift, Ticketmaster, and Chokepoint Capitalism with Cory Doctorow"
        )
) +
    geom_point(
        aes(
            x = log_days_since_release,
            y = cumulative_downloads
        )
    ) +
    geom_line(
        data = tswift_fitted_df,
        aes(
            x = log_days_since_release,
            y = cumulative_downloads_pred,
            color = as.factor(in_wmata_general_ad)
        ),
        linewidth = 1.2
    ) +
    theme_minimal()


ggplot(daily_slope_kink_df %>%
    filter(
        episode_id %in% released_between_episode_ids(
            cutoff_period_start_str = "2022-11-01",
            cutoff_period_end_str = "2023-01-01",
            release_dates_df
        )
    )
) +
    geom_point(
        aes(
            x = log_days_since_release,
            y = cumulative_downloads,
            color = in_wmata_general_ad
        )
    ) +
    theme_minimal()

##### DMV/WMATA DIFF-IN-DIFF #####

# Generate the episode IDs once because otherwise, for 4million+
# observations, the function gets called every time and it gets SLOW
# Even as it is, the mutate takes quite a bit of time
wmata_treated_episode_ids <- released_between_episode_ids(
    "2022-07-07",
    "2023-01-16",
    release_dates_df
)
dmv_wmata_did_df <- episode_locations_downloads_df %>%
    filter(
        episode_id %in% wmata_treated_episode_ids,
        !is.na(log_days_since_release),
        log_days_since_release != -Inf
    ) %>%
    mutate(
        in_wmata_general_ad = ifelse(
            date %within% wmata_general_interval,
            1, 0
        )
    )


lm(
    cumulative_downloads ~ log_days_since_release + in_wmata_general_ad + relevant_msa+ in_wmata_general_ad:relevant_msa,
    data = dmv_wmata_did_df
) %>%
summary()

##### REGRESSION TABLES #####
###### NAIVE OLS MODELS ######
naive_ols_models <- list(
    t_14_ols_trailing_only,
    t_14_ols_trailing_wmata_general,
    t_14_ols_trailing_first_ad_experiment,
    t_28_ols_trailing_only,
    t_28_ols_trailing_wmata_general,
    t_28_ols_trailing_first_ad_experiment,
    t_28_ols_trailing_autoreg,
    t_28_ols_trailing_twice_autoreg
)

naive_ols_models_RSEs <- list(
    t_14_ols_trailing_only_RSE,
    t_14_ols_trailing_wmata_general_RSE,
    t_14_ols_trailing_first_ad_experiment_RSE,
    t_28_ols_trailing_only_RSE,
    t_28_ols_trailing_wmata_general_RSE,
    t_28_ols_trailing_first_ad_experiment_RSE,
    t_28_ols_trailing_autoreg_RSE,
    t_28_ols_trailing_twice_autoreg_RSE
)

stargazer(
    naive_ols_models,
    dep.var.labels = c("Downloads ($t=14$)", "Downloads ($t=28$)"),
    order = c("trailing5_t_14_avg", "trailing5_t_28_avg"),
    covariate.labels = c("Trailing Avg. ($n=5, t=14$)", "Trailing Avg. ($n=5, t=28$)", "WMATA Ad.", "Economist/Vox Ad."),
    no.space = TRUE,
    omit.stat = c("ser", "f", "rsq"),
    se = naive_ols_models_RSEs
)

### PLOTS ###

##### REFERENCE GGPLOT ELEMENTS ##### 
ad_period_shade_geom <- geom_rect(
    aes(
        xmin = ymd("2023-01-16"),
        xmax = ymd("2023-02-15"),
        ymin = 0,
        ymax = Inf,
    ),
    fill = "grey",
    alpha = 0.05
)
ad_period_line_start_geom <- geom_segment(
    aes(
        x = ymd("2023-01-16"),
        xend = ymd("2023-01-16"),
        y = 0,
        yend = Inf
    ),
    color = "grey48"
)
ad_period_line_end_geom <- geom_segment(
    aes(
        x = ymd("2023-02-15"),
        xend = ymd("2023-02-15"),
        y = 0,
        yend = Inf
    ),
    color = "grey48"
)

##### MOVING AVERAGE DECOMPOSITION GGPLOTS ####
recent_podcast_moving_avg_decomp <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date", "active_back_expired_catalog"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c(
            "avg_all_daily_downloads_14"
        ),
        date >= ymd("2022-10-01")
    )
) +
    geom_area(
        aes(
            x = date,
            y = value,
            fill = as.factor(active_back_expired_catalog),
            group = active_back_expired_catalog
        ),
        position = "stack",
        alpha = 1
    ) +
    ad_period_line_start_geom +
    ad_period_line_end_geom +
    scale_fill_stigler(
        breaks = c(1, 2, 3),
        labels = c("Most-recent", "Next Five", "Older"),
        name = ""
    ) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0)),
        position = "right"
    ) +
    scale_x_date(
        name = "Date",
        breaks = date_breaks("1 month"),
        labels = label_date_short(format = c("%y", "%b")),
        expand = expand_scale(mult = c(0,0.05))
    ) +
    labs(
        title = "**Capitalisn't: Composition of daily-downloads moving average**", 
        subtitle = "14-day leading average, since Oct. 1st, 2022",
        tag = "Figure 2A"
    ) +
    theme_stigler()
recent_podcast_moving_avg_decomp

alltime_podcast_moving_avg_decomp <- ggplot(
    podcast_daily_downloads_df %>%
    pivot_longer(
        cols = -c("date", "active_back_expired_catalog"),
        names_to = "download_type",
        values_to = "value"
    ) %>%
    filter(
        download_type %in% c(
            "avg_all_daily_downloads_14"
        )
    )
) +
    geom_area(
        aes(
            x = date,
            y = value,
            fill = as.factor(active_back_expired_catalog),
            group = active_back_expired_catalog
        ),
        position = "stack",
        alpha = 1
    ) +
    ad_period_line_start_geom +
    ad_period_line_end_geom +
    scale_fill_stigler(
        breaks = c(1, 2, 3),
        labels = c("Most-recent", "Next Five", "Older"),
        name = ""
    ) +
    scale_y_continuous(
        labels = scales::comma,
        expand = expand_scale(mult = c(0, 0)),
        position = "right"
    ) +
    scale_x_date(
        name = "Date",
        breaks = date_breaks("3 month"),
        labels = label_date_short(format = c("%y", "%b")),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        title = "**Capitalisn't: Composition of daily-downloads moving average**", 
        subtitle = "14-day leading average, all time",
        tag = "Figure 2B"
    ) +
    theme_stigler()
alltime_podcast_moving_avg_decomp

##### CUMULATIVE DOWNLOADS GGPLOTS #####
recent_20_episodes_cumul_perf <- ggplot(
    daily_downloads_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 20,
                release_dates_df
            )
        )
) +
    geom_line(
        aes(
            x = days_since_release,
            y = cumulative_downloads,
            group = episode_id,
            color = as.numeric(release_date)
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        "blues_2",
        # "red_to_blue",
        reverse = TRUE,
        discrete = FALSE,
        name = "",
        guide = "none"
    ) +
    scale_x_continuous(
        name = "Days since release",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 250)
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 25000)
    ) +
    labs(
        title = "**Capitalisn't: Cumulative daily downloads**",
        subtitle = "20 most recent episodes",
        tag = "Figure 3A"
    ) +
    theme_stigler()
recent_20_episodes_cumul_perf

alltime_episodes_cumul_perf <- ggplot(
    daily_downloads_df %>%
        mutate(release_date = as.numeric(release_date))
) +
    geom_line(
        aes(
            x = days_since_release,
            y = cumulative_downloads,
            group = episode_id,
            color = release_date
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        "blues_2",
        reverse = TRUE,
        discrete = FALSE,
        guide = "none"
    ) +
    scale_x_continuous(
        name = "Days since release",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 2200)
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        expand = expand_scale(mult = c(0, 0)),
        limits = c(0, 30000)
    ) +
    labs(
        title = "**Capitalisn't: Cumulative daily downloads**",
        subtitle = "20 most recent episodes",
        tag = "Figure 3B"
    ) +
    theme_stigler()
alltime_episodes_cumul_perf

##### t={1,14,28,42} CUMULATIVE DOWNLOADS GGPLOTS #####

recent_20_1142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        episode_id %in% recent_n_episode_ids(
            n = 20,
            release_dates_df
        ),
        days_since_release %in% c(1, 14, 28, 42)
    )
) +
    ad_period_shade_geom +
    geom_segment(
        data = daily_downloads_df %>%
            filter(
                episode_id %in% recent_n_episode_ids(
                    n = 20,
                    release_dates_df
                ),
                days_since_release %in% c(1, 14, 28, 42)
            ) %>%
        pivot_wider(
            id_cols = c(episode_id, release_date),
            names_from  = days_since_release,
            values_from = cumulative_downloads,
            names_prefix = "downloads_t_"
        ),
        aes(
            x = release_date,
            xend = release_date,
            y = downloads_t_1,
            yend = downloads_t_42,
            group = episode_id
        ),
        color = "grey",
        size = 1.25
    ) +
    geom_point(
        aes(
            x = release_date,
            y = cumulative_downloads,
            group = episode_id,
            color = as.factor(days_since_release)
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        name = "Days after release *t*: "
    ) +
    scale_x_date(
        labels = label_date_short(format = c("%y", "%b")),
        breaks = date_breaks("1 month"),
        name = "Release date"
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        limits = c(0, 20000),
        expand = expand_scale(mult = c(0, 0))
    ) +
    labs(
        title = "**Capitlisnt't downloads *t* days after release**",
        tag = "Figure 1B"
    ) +
    theme_stigler()
recent_20_1142842_day_cumul_perf

all_1142842_day_cumul_perf <- ggplot(
    daily_downloads_df %>%
    filter(
        days_since_release %in% c(1, 14, 28, 42)
    )
) +
    geom_segment(
        data = daily_downloads_df %>%
            filter(
                days_since_release %in% c(1, 42)
            ) %>%
            pivot_wider(
                id_cols = c(episode_id, release_date),
                names_from  = days_since_release,
                values_from = cumulative_downloads,
                names_prefix = "downloads_t_"
            ),
        aes(
            x = release_date,
            xend = release_date,
            y = downloads_t_1,
            yend = downloads_t_42,
            group = episode_id
        ),
        color = "grey",
        size = 1
    ) +
    geom_point(
        aes(
            x = release_date,
            y = cumulative_downloads,
            group = episode_id,
            color = as.factor(days_since_release)
        ),
        size = 1.5
    ) +
    scale_color_stigler(
        name = "Days after release *t*: "
    ) +
    scale_x_date(
        labels = label_date_short(format = c("%y", "%b")),
        breaks = date_breaks("3 month"),
        name = "Release date"
    ) +
    scale_y_continuous(
        labels = scales::comma,
        position = "right",
        limits = c(0, 20000),
        expand = expand_scale(mult = c(0, 0))
    ) +
    labs(
        title = "**Capitlisnt't downloads *t* days after release**",
        tag = "Figure 1A"
    ) +
    theme_stigler()
all_1142842_day_cumul_perf





#### DAILY-KINK SIGNIFICANCE PLOTS #####
ggplot(
    daily_kink_results_df %>%
        mutate(
            stars = as.factor(
                case_when(
                    p.value < 0.01 ~ "***",
                    (p.value >= 0.01) & (p.value < 0.05) ~ "**",
                    (p.value >= 0.05) & (p.value < 0.10) ~ "*",
                    TRUE ~ ""
                )
            ),
        ) %>%
        arrange(release_date)
) +
    geom_tile(
        aes(
            x = as.factor(release_date),
            y = term,
            fill = stars
        )
    ) +
    geom_segment(
        aes(
            x = as.factor(ymd("2023-01-19")),
            xend = as.factor(ymd("2023-01-19")),
            y = Inf,
            yend = 0,
        ),
        linewidth = 1,
        color = "black"
    ) +
    scale_fill_stigler(
        palette = "reds",
        discrete = TRUE
    ) +
    theme_minimal()

#### END ####