#### PREAMBLE ####
library(tidyverse)
library(lubridate)
library(stargazer)
library(sandwich)
library(progress)
library(data.table)
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

is_a_release_date_df <- release_dates_df %>%
    mutate(is_a_release_date = 1)

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

# Converting the massive daily episode-locations and the
# considerably smaller cities_id tibbles (from tidyverse)
# to data.table format so that they can be joined together
# and manipulated much faster. Smae goes for release_dates df
episode_locations_data_dt <- data.table(
    episode_locations_data_in %>%
        filter(
            episode_id %in% released_since_episode_ids(
                cutoff_date_str = "2020-09-24",
                release_dates_df
            )
        ),
    key = c("city_id", "episode_id")
)

dmv_cities_dt <- data.table(
    dmv_cities_df %>%
        select(-c(
            "city_name",
            "state_id"
        )),
    key = "city_id"
)

release_dates_dt <- data.table(
    release_dates_df,
    key = "episode_id"
)

# Merging the relevant_msa column from the dmv_cities_dt
# onto the wide episode-locations dt so that things can
# later be filtered based on the `relevant_msa` variable.
# Also mergingin the release_dates dt so that we can filter
# out episodes that are released prior to the period of
# interest (so that we can do this while the format is)
# still wide rather than long.
episode_locations_downloads_dt <- episode_locations_data_dt %>%
    merge(
        dmv_cities_dt,
        by = "city_id", all = TRUE
    ) %>%
    setDT(key = c("episode_id", "city_id")) %>%
    merge(
        release_dates_dt,
        by = "episode_id", all = TRUE
    )

# If an episode-place observation (row) did not match to anything in the
# dmv_cities_dt, the value in the 'relevant_msa' column is coerced
# to NA. We change that back to 0.
episode_locations_downloads_dt[, "relevant_msa"][is.na(episode_locations_downloads_dt[, "relevant_msa"])] <- 0

# Dropping faulty observations (potential errors introduced by bad/time
# API calls) /RAM overflow.
episode_locations_downloads_dt <- episode_locations_downloads_dt[!is.na(episode_id),]
episode_locations_downloads_dt <- episode_locations_downloads_dt[!is.na(city_id),]

# Generating a new data.table that keeps only observations that are
# in a relevant_msa == 1 (ie in the DMV) or are in New York state
# which we will use as the untreated unit for another specification
# of the DiD.
episode_locations_dmv_ny_dt <- episode_locations_downloads_dt %>%
    .[(state_id == 5128638) | (relevant_msa == 1), ]


episode_locations_downloads_dt <- episode_locations_downloads_dt[,
    lapply(.SD, sum, na.rm = TRUE),
    by = c("episode_id", "relevant_msa"),
    .SDcols = 5:237
]

episode_locations_dmv_ny_dt <- episode_locations_dmv_ny_dt[,
    lapply(.SD, sum, na.rm = TRUE),
    by = c("episode_id", "relevant_msa"),
    .SDcols = 5:237
]


# Reshaping subset of relevant episode-level observations to be long
episode_locations_msa_downloads_df <- episode_locations_downloads_dt %>%
    as_tibble() %>%
    pivot_longer(
        cols = -c("episode_id", "relevant_msa"),
        names_to = "date",
        values_to = "cumulative_downloads"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    mutate(
        date = ymd(date),
        days_since_release = as.numeric(
            as.period(
                interval(release_date, date),
                unit = "days"
            )
        ) / as.numeric(days(1)),
    ) %>%
    filter(
        days_since_release > 0
    ) %>%
    mutate(
        log_days_since_release = log(days_since_release)
    )

episode_locations_dmv_ny_downloads_df <- episode_locations_dmv_ny_dt %>%
    as_tibble() %>%
    pivot_longer(
        cols = -c("episode_id", "relevant_msa"),
        names_to = "date",
        values_to = "cumulative_downloads"
    ) %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    mutate(
        date = ymd(date),
        days_since_release = as.numeric(
            as.period(
                interval(release_date, date),
                unit = "days"
            )
        ) / as.numeric(days(1)),
    ) %>%
    filter(
        days_since_release > 0
    ) %>%
    mutate(
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
    summary.stat = c("min", "p25", "mean", "median", "p75", "max", "n"),
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
    summary.stat = c("min", "p25", "mean", "median", "p75", "max", "n"),
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

dmv_msa_did_df <- episode_locations_msa_downloads_df %>%
    left_join(
        titles_ids_df,
        by = "episode_id",
        multiple = "all"
    )  %>%
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
        abs_days_to_ad_start = (as.numeric(days(1)) + abs(
                as.numeric(
                    as.period(
                        interval(date, ymd("2023-01-16")),
                        unit = "days"
                    )
                )
            )) / as.numeric(days(1)),
        log_days_to_ad_start = case_when(
            date < ymd("2023-01-16") ~ -log(abs_days_to_ad_start),
            date > ymd("2023-01-16") ~ log(abs_days_to_ad_start),
            date == ymd("2023-01-16") ~ 0
        ),
        rel_days_to_ad_start = case_when(
            date < ymd("2023-01-16") ~ -abs_days_to_ad_start,
            date > ymd("2023-01-16") ~ abs_days_to_ad_start,
            date == ymd("2023-01-16") ~ 0
        )
    ) %>%
    group_by(episode_id, relevant_msa) %>%
    arrange(date) %>%
    mutate(
        daily_downloads = cumulative_downloads - lag(
            cumulative_downloads,
            n = 1
        ),
        temp_date = as.numeric(date)
    ) %>%
    left_join(
        is_a_release_date_df %>%
            mutate(temp_date = as.numeric(release_date)) %>%
            select(-c("episode_id", "release_date")),
        by = "temp_date",
        multiple = "all"
    ) %>%
    mutate(
        is_a_release_date = ifelse(
            is.na(is_a_release_date),
            0, is_a_release_date
        )
    ) %>%
    view()

dmv_msa_episode_level_did_results_df <- data.frame()

for (episode in dmv_msa_did_df$episode_id %>% unique()) {

    msa_did_df <- dmv_msa_did_df %>%
        filter(
            episode_id == episode
        )

    title <- msa_did_df %>%
        select(title) %>%
        unique() %>%
        pull()

    dmv_msa_episode_did_model <- lm(
        cumulative_downloads ~ log_days_since_release +
            relevant_msa +
            in_wmata_general_ad +
            log_days_since_release:relevant_msa:in_wmata_general_ad,
            data = msa_did_df
    )

    dmv_msa_episode_did_RSE <- coeftest(
        dmv_msa_episode_did_model,
        vcov. = vcovHC(dmv_msa_episode_did_model, type = "HC2")
    ) %>%
    tidy() %>%
    mutate(
        episode_id = episode,
        title = title
    )

    dmv_msa_episode_level_did_results_df <- dmv_msa_episode_level_did_results_df %>%
        rbind(dmv_msa_episode_did_RSE)
}

dmv_msa_episode_level_did_results_df <- dmv_msa_episode_level_did_results_df %>%
    left_join(
        release_dates_df,
        by = "episode_id",
        multiple = "all"
    ) %>%
    view()

fe_log_time_to_treat_did <- feols(
    cumulative_downloads ~ log_days_since_release +
        log_days_to_ad_start +
        relevant_msa +
        i(log_days_to_ad_start, relevant_msa) |
        episode_id + as.factor(date),
    cluster = ~episode_id,
    data = dmv_msa_did_df
)

fe_log_time_to_treat_did_release_control <- feols(
    cumulative_downloads ~ log_days_since_release +
        log_days_to_ad_start +
        relevant_msa +
        is_a_release_date +
        i(log_days_to_ad_start, relevant_msa) |
        episode_id + as.factor(date),
    cluster = ~episode_id,
    data = dmv_msa_did_df
)


iplot(
    fe_log_time_to_treat_did,
    # fe_log_time_to_treat_did_release_control,
    xlab = "Relative Logged Days to Advertisement Start",
    main = "Cumulative Downloads DID Event Study (TWFE)"
)



fe_time_to_treat_did <- feols(
    cumulative_downloads ~ log_days_since_release +
        rel_days_to_ad_start +
        relevant_msa +
        i(rel_days_to_ad_start, relevant_msa) |
        episode_id + as.factor(date),
    cluster = ~episode_id,
    data = dmv_msa_did_df
)



fe_time_to_treat_did_release_control <- feols(
    cumulative_downloads ~ log_days_since_release +
        rel_days_to_ad_start +
        relevant_msa +
        is_a_release_date +
        i(rel_days_to_ad_start, relevant_msa) |
        episode_id + as.factor(date),
    cluster = ~episode_id,
    data = dmv_msa_did_df
)

iplot(
    fe_time_to_treat_did,
    xlab = "Days to Advertisement Start",
    main = 'Cumulative Downloads DIDEvent Study (TWFE)'
)



fe_time_to_treat_daily_downloads <- feols(
    daily_downloads ~ log_days_since_release +
        rel_days_to_ad_start +
        relevant_msa +
        i(rel_days_to_ad_start, relevant_msa) |
        episode_id + as.factor(date),
    cluster = ~episode_id,
    data = dmv_msa_did_df
)


iplot(
    fe_time_to_treat_daily_downloads,
    xlab = "Days to Advertisement Start",
    main = "Daily Downloads DID Event Study (TWFE)"
)






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
ggsave(
    plot = recent_podcast_moving_avg_decomp,
    filename = "wmataCampaignAnalysis/figures/recent_podcast_moving_avg_decomp.png",
    dpi = 300
)

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
ggsave(
    plot = alltime_podcast_moving_avg_decomp,
    filename = "wmataCampaignAnalysis/figures/alltime_podcast_moving_avg_decomp.png",
    dpi = 300
)

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
ggsave(
    plot = recent_20_episodes_cumul_perf,
    filename = "wmataCampaignAnalysis/figures/recent_20_episodes_cumul_perf.png",
    dpi = 300
)

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
ggsave(
    plot = alltime_episodes_cumul_perf,
    filename = "wmataCampaignAnalysis/figures/alltime_episodes_cumul_perf.png",
    dpi = 300
)

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
ggsave(
    plot = recent_20_1142842_day_cumul_perf,
    filename = "wmataCampaignAnalysis/figures/recent_20_1142842_day_cumul_perf.png",
    dpi = 300
)

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
ggsave(
    plot = all_1142842_day_cumul_perf,
    filename = "wmataCampaignAnalysis/figures/all_1142842_day_cumul_perf.png",
    dpi = 300
)




#### DAILY-KINK SIGNIFICANCE PLOTS #####
ggplot(
    daily_kink_results_df %>%
        filter(
            episode_id %in% recent_n_episode_ids(
                n = 50,
                release_dates_df
            )
        ) %>%
        mutate(
            stars = as.factor(
                case_when(
                    p.value < 0.01 ~ "***",
                    (p.value >= 0.01) & (p.value < 0.05) ~ "**",
                    (p.value >= 0.05) & (p.value < 0.10) ~ "*",
                    TRUE ~ ""
                )
            )
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

#### DMV EPISODE-LEVEL DIFF-IN-DIFF PLOTS ####

for (episode in dmv_msa_did_df$episode_id %>% unique()) {

    title <- dmv_msa_did_df %>%
        filter(episode_id == episode) %>%
        select(title) %>%
        distinct() %>%
        pull()

    plot <- ggplot(dmv_msa_did_df %>%
        filter(episode_id == episode)
    ) +
        geom_point(
            aes(
                x = log_days_since_release,
                y = cumulative_downloads,
                color = as.factor(relevant_msa),
                fill = as.factor(in_wmata_general_ad),
                group = relevant_msa
            ),
            shape = 21
        ) +
        scale_fill_stigler(
            palette = "blues_2",
        ) +
        scale_color_stigler(
            palette = "reds_2",
        ) +
        labs(
            title = title
        ) +
        theme_stigler()

    plot %>% print()

}

ggplot(dmv_msa_did_df) +
        geom_point(
            aes(
                x = log_days_since_release,
                y = cumulative_downloads,
                color = as.factor(relevant_msa),
                fill = as.factor(in_wmata_general_ad),
                group = relevant_msa
            )
        ) +
        scale_fill_stigler(
            palette = "blues_2",
        ) +
        scale_color_stigler(
            palette = "reds_2",
        ) +
        labs(
            title = title
        ) +
        theme_minimal() +
        facet_wrap(~episode_id)

#### END ####



#### SCRATCH ####


mydata <- read.dta("http://dss.princeton.edu/training/Panel101.dta") %>%
    mutate(
        time = ifelse(year >= 1994, 1, 0),
        treat = ifelse(
            country %in% c(
                "E", "F", "G"),
                1, 0 ),
        did = time * treat
    ) %>%
    view()


did_model <- lm(
    y ~ treat + time + did,
    data = mydata
)

coeftest(
    did_model,
    vcov. = vcovHC(did_model, type = "HC2")
)
