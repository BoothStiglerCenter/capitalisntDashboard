#### PREAMBLE #####
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
library(sandwich)
library(dynlm)
library(stargazer)

`%notin%` <- Negate(`%in%`)


setwd('C:/Users/Joshualevy/Documents/Capitalisnt Analysis')
downloads_file_path <- 'C:/Users/Joshualevy/Documents/Capitalisnt Analysis/podcast_download_data.csv'
chars_file_path <- 'C:/Users/Joshualevy/Documents/Capitalisnt Analysis/podcast_chars.csv'

downloads_data <- read_csv(downloads_file_path)
chars_data <- read_csv(chars_file_path)


##### CLEANING ####


downloads_data_formatted <- downloads_data %>%
  rename('daily_downloads' = 'downloads_total')%>%
  mutate(date = as_date(interval, format='%m/%d/%Y'))%>%
  group_by(episode_title) %>% 
  arrange(date) %>% 
  mutate(release_date = min(date),
         days_since_release =time_length(date - release_date, unit="days"),
         cumulative_downloads  = cumsum(daily_downloads)) %>%
  ungroup() %>%
  mutate(episode_title = ifelse(release_date == ymd("2021-12-02"), 'Is Woke Capitalism a Threat to Democracy?', episode_title))

coerce_binary <- function(x, na.rm=TRUE)(ifelse(x=='Y', '1', '0'))
coerce_zero <- function(x)(ifelse(is.na(x), '0', x))
coerce_na <- function(x, na.rm=TRUE)(ifelse(x=='n/a', NA, x))
coerce_numeric <- function(x, na.rm=TRUE)(as.numeric(x))

chars_data_formatted <- chars_data %>%
  rename('episode_title' = 'Episode Title',
         'guest_binary' = 'Guest? Y/N',
         'multiple_guest_binary' = 'Multiple Guest? Y/N',
         'guest_name' = 'Guest Name',
         'guest_twitter_followers' = 'Twitter Followers',
         'guest_linkedin_followers' = 'LinkedIn Followers',
         'guest_2_name' = 'Guest 2 Name',
         'guest_2_twitter_followers' = 'G2 Twitter Followers',
         'guest_2_linkedin_followers' = 'G2 LinkedIn Followers',
         'current_event_binary' = 'Current event? Y/N',
         'topic_tag' = 'Tag',
         'book_promo_binary' = 'Book? Y/N',
         'tenured_guest_binary' = 'At least one Tenure-Track? Y/N',
         'release_time' = 'Timing'
         )%>%
  mutate_at(c('guest_binary',
              'multiple_guest_binary',
              'current_event_binary',
              'book_promo_binary',
              'tenured_guest_binary'),coerce_binary) %>%
  mutate_at(c('guest_binary',
              'multiple_guest_binary',
              'current_event_binary',
              'book_promo_binary',
              'tenured_guest_binary'),coerce_numeric) %>%
  mutate(book_promo_binary = ifelse(is.na(book_promo_binary), 0, book_promo_binary),
         current_event_binary = ifelse(is.na(current_event_binary), 0, current_event_binary))%>%
  mutate_at(c('guest_name',
              'guest_twitter_followers',
              'guest_linkedin_followers',
              'guest_2_name',
              'guest_2_twitter_followers',
              'guest_2_linkedin_followers'
              ), coerce_na) %>%
  mutate_at(c('guest_twitter_followers',
             'guest_linkedin_followers',
             'guest_2_twitter_followers',
             'guest_2_linkedin_followers'
  ), coerce_zero) %>%
  mutate(episode_title = ifelse(guest_twitter_followers == 75800, 'Is Woke Capitalism A Threat To Democracy?', episode_title),
         guest_twitter_followers = as.numeric(guest_twitter_followers),
         guest_linkedin_followers = as.numeric(guest_linkedin_followers),
         guest_2_twitter_followers = as.numeric(guest_2_twitter_followers),
         guest_2_linkedin_followers = as.numeric(guest_2_linkedin_followers)
         ) %>% 
  mutate(total_twitter_followers = guest_twitter_followers + guest_2_twitter_followers,
         total_linkedin_followers = guest_linkedin_followers + guest_2_linkedin_followers)%>%
  select(-c('Did an ad run?', '...16'))

##### CLEANING -- MERGING DOWNLOADS WITH CHARS ####

chars_downloads_data <- left_join(downloads_data_formatted, chars_data_formatted, on='episode_title') %>%
  mutate(days_since_release_log_adjust = days_since_release +1) %>% 
  group_by(episode_title) %>%
  mutate(total_downloads = max(cumulative_downloads)) %>%
  ungroup()


chars_downloads_data_t14 <- chars_downloads_data %>%
  filter(days_since_release ==14) %>%
  mutate(t14_downloads = cumulative_downloads) %>%
  select(episode_title, t14_downloads)

chars_downloads_data <- left_join(chars_downloads_data, chars_downloads_data_t14, on=c('epsiode_title'))
rm(chars_downloads_data_t14)


##### SUMMARY STATISTICS & FIGURES ####

stargazer(chars_downloads_data %>%
            distinct(episode_title, .keep_all=TRUE)%>%
            mutate(release_date_string = as.character(release_date)) %>%
            arrange(release_date) %>%
            select(episode_title, release_date_string, total_downloads, t14_downloads),
          summary=FALSE,
          rownames=FALSE)

stargazer(chars_data_formatted %>%
            select(-c(episode_title,
                      guest_name,
                      guest_twitter_followers,
                      guest_linkedin_followers, 
                      guest_2_name,
                      guest_2_twitter_followers,
                      guest_2_linkedin_followers,
                      topic_tag, 
                      release_time)) %>%
            as.data.frame(),
          covariate.labels = c("Features guest",
                               "Features multiple guests",
                               "Current event ep.",
                               "Book promo ep.",
                               "Tenured guest",
                               "Guest(s) Twitter followers",
                               "Guest(s) LinkedIn followers"),
          out='episode_chars_summary.tex',
          title="Episode-level Charateristics: Summary Statistics",
          summary=TRUE)


  
##### SUMMARY STATISTICS --- (cumulative downloads since release) ####
#viridis_lab_dates <- pretty(chars_downloads_data$release_date)
ggplot(chars_downloads_data )+
  geom_line(aes(x=days_since_release, y=cumulative_downloads, color=release_date, group=episode_title), size=1.1)+
  labs(title="*Capitalisn't*: Cumulative downloads over time")+
  xlab("Days since release")+
  ylab("Cumulative downloads")+
  scale_y_continuous(label=scales::comma)+
  scale_color_viridis_c(breaks=c(as.numeric(min(chars_downloads_data$release_date)), as.numeric(max(chars_downloads_data$release_date))),
                      labels=c("Oldest", "Newest"),
                      name="Release date")+
  theme_minimal()+
  theme(plot.title = element_markdown(),
        legend.position="none")
ggsave('cumulative_downloads_since_release.png', plot=last_plot(), width=6, height=4, units="in", dpi=300)


##### SUMMARY STATISTICS --- (cumulative downloads  vs log(days since release)) ####
#viridis_lab_dates <- pretty(chars_downloads_data$release_date)
ggplot(chars_downloads_data)+
  geom_line(aes(x=days_since_release_log_adjust, y=cumulative_downloads, color=release_date, group=episode_title), size=1.1)+
  labs(title="Capitalisnt")+
  xlab("Log(Days since release+1)")+
  # ylab("Cumulative downloads")+
  scale_y_continuous(label=scales::comma)+
  scale_x_continuous(trans="log")+
  scale_color_viridis_c(breaks=c(as.numeric(min(chars_downloads_data$release_date)), as.numeric(max(chars_downloads_data$release_date))),
                        labels=c("Oldest", "Newest"),
                        name="Release date")+
  theme_minimal()+
  theme(plot.title = element_markdown(colour = "white"),
        axis.title.y = element_blank())
ggsave('cumulative_downloads_since_release_log.png', plot=last_plot(), width=6, height=4, units="in",dpi=300)

#### REGRESSION-READY DF: ADDING ADVERTISING DATES ####

# FIVE-EPISODE TRAILING AVERAGE (with the exception of the first four averages which use, 1,2-,3-,and 4-episode trailing averages instead)
regress_t14 <- chars_downloads_data %>%
  filter(days_since_release==14) %>%
  arrange(release_date)%>%
  mutate(past_five_down_sum = lag(cumulative_downloads, n=1) + lag(cumulative_downloads, n=2) +lag(cumulative_downloads, n=3) +lag(cumulative_downloads, n=4) +lag(cumulative_downloads, n=5),
         past_five_ave_performance = past_five_down_sum /5)%>%
  mutate(past_five_down_sum = case_when(episode_title== 'The Right And Wrong Of MMT (Modern Monetary Theory)' ~ lag(cumulative_downloads, n=1),
                                        episode_title== 'Should We Lockdown Again?' ~ lag(cumulative_downloads, n=1) + lag(cumulative_downloads, n=2),
                                        episode_title== "What Is The Alternative To Friedman's Capitalism?" ~lag(cumulative_downloads, n=1) + lag(cumulative_downloads, n=2) +lag(cumulative_downloads, n=3),
                                        episode_title== 'How The Supreme Court Influences Our Economy' ~ lag(cumulative_downloads, n=1) + lag(cumulative_downloads, n=2) +lag(cumulative_downloads, n=3) +lag(cumulative_downloads, n=4),
                                        TRUE ~ past_five_down_sum),
         past_five_ave_performance = case_when(episode_title== 'The Right And Wrong Of MMT (Modern Monetary Theory)' ~ past_five_down_sum /1,
                                               episode_title== 'Should We Lockdown Again?' ~ past_five_down_sum / 2,
                                               episode_title== "What Is The Alternative To Friedman's Capitalism?" ~ past_five_down_sum /3,
                                               episode_title== 'How The Supreme Court Influences Our Economy' ~ past_five_down_sum /4,
                                               TRUE ~ past_five_ave_performance))

## Same as regress_t14 but with more advertising-campaign-related features
regress_t14_ads <- regress_t14 %>%
  mutate(general_ad = ifelse(release_date %within% interval(ymd("2021-04-01"), ymd("2021-04-30")), 1,
                             ifelse(release_date %within% interval(ymd("2021-06-01"), ymd("2021-06-15")), 1, 0)),
         post_economist_ad = ifelse((release_date - ymd("2021-04-30"))>0, 1, 0),
         post_general_campaign = ifelse((release_date - ymd("2021-06-15"))>0, 1, 0), 
         specific_ad = ifelse(episode_title == "The Causes And Effects Of Today's Inflation, With Raghuram Rajan", 1,0),
         post_specific_campaign = ifelse((release_date -ymd('2022-02-03'))>0, 1, 0))



regress_daily_down_fe <- chars_downloads_data %>%
  mutate(general_ad = ifelse(date %within% interval(ymd("2021-04-01"), ymd("2021-04-30")), 1,
                             ifelse(date %within% interval(ymd("2021-06-01"), ymd("2021-06-15")), 1, 0)),
         economist_ad  = ifelse(date %within% interval(ymd("2021-04-01"), ymd("2021-04-30")), 1, 0),
         vox_ad = ifelse(date %within% interval(ymd("2021-06-01"), ymd("2021-06-15")), 1, 0),
         post_economist_ad = ifelse((date - ymd("2021-04-30"))>0, 1, 0),
         inter_general_ads = ifelse(date %within% interval(ymd("2021-05-01"), ymd("2021-05-31")), 1, 0),
         post_general_campaign = ifelse((date - ymd("2021-06-15"))>0, 1, 0),
         partially_treated_general_ad = ifelse(date -ymd("2021-04-01") >= 0 , 1, 0),
         specific_ad = ifelse(episode_title == "The Causes And Effects Of Today's Inflation, With Raghuram Rajan", 1,0),
         post_specific_campaign = ifelse((date - ymd('2022-02-03'))>0, 1, 0),
         release_day_of_week = factor(wday(release_date, label=TRUE), ordered=FALSE),
         day_of_week = factor(wday(date, label=TRUE), ordered=FALSE),
         month_year = factor(paste(year(date), month(date), sep='-')),
         quarter_year = factor(paste(year(date), paste('Q', quarter(date)), sep='-')))



timeseries_agg_perf <- chars_downloads_data %>%
  group_by(date) %>%
  mutate(agg_downloads = sum(daily_downloads)) %>%
  ungroup() %>%
  distinct(date, agg_downloads) %>%
  mutate(agg_downloads_lagged = lag(agg_downloads),
         general_ad = ifelse(date %within% interval(ymd("2021-04-01"), ymd("2021-04-30")), 1,
                             ifelse(date %within% interval(ymd("2021-06-01"), ymd("2021-06-15")), 1, 0)),
         post_economist_ad = ifelse((date - ymd("2021-04-30"))>0, 1, 0),
         inter_general_ads = ifelse(date %within% interval(ymd("2021-04-3"), ymd("2021-05-31")), 1, 0),
         post_general_campaign = ifelse((date - ymd("2021-06-14"))>0, 1, 0),
         post_specific_campaign = ifelse((date - ymd('2022-02-03'))>0, 1, 0),
         day_of_week = factor(wday(date, label=TRUE), ordered=FALSE))


acf(timeseries_agg_perf$agg_downloads)

timeseries <- dynlm(agg_downloads ~ L(agg_downloads, 1) + general_ad,
                        data=timeseries_agg_perf)

summary(timeseries)


ggplot(timeseries_agg_perf)+
  geom_line(aes(x=date, y=agg_downloads), size=1)+
  annotate(geom = "rect", xmin = as.Date("2021-04-01"), xmax=as.Date("2021-04-30"), ymin=0, ymax=Inf, fill="red", alpha=0.2)+
  annotate(geom="text", x= as.Date("2021-04-30"), y=7200, label="Economist Media Group", color="red")+
  annotate(geom = "rect", xmin = as.Date("2021-06-01"), xmax=as.Date("2021-06-15"), ymin=0, ymax=Inf, fill="yellow", alpha=0.2)+
  annotate(geom="text", x= as.Date("2021-06-01"), y=7800, label="Vox Media")+
  scale_y_continuous(labels=scales::comma)+
  ylab("Aggregate Downloads")+
  xlab("Date")+
  ggtitle("Daily Aggregate Total Downloads")+
  theme_minimal()
ggsave('daily_aggregate_download.png', plot=last_plot(), width =12, height=4, units="in", dpi=300)

library(ggrepel)

ggplot(chars_downloads_data %>%
         mutate(daily_alpha = 1 + log(1/days_since_release)) %>%
         arrange(release_date) %>%
         group_by(release_date) %>%
         mutate(first_date = ifelse(date == release_date, 1, 0),
                group_id = cur_group_id(),
                label_w_group_id = paste(group_id, episode_title, sep='. ')) %>%
         view())+
  geom_line(aes(x=date, y=cumulative_downloads, color=label_w_group_id, alpha=daily_alpha, group=episode_title), size=1.25)+
  geom_label_repel(data = chars_downloads_data %>%
                    arrange(release_date) %>%
                    group_by(release_date) %>%
                    mutate(first_date = ifelse(date == release_date+1, 1,0),
                           group_id = cur_group_id(),
                           label_w_group_id = paste(group_id, episode_title, sep='. ')) %>%
                    ungroup() %>%
                    filter(first_date == 1),
                  nudge_y=-500,
                  aes(x=date, y=cumulative_downloads, color=label_w_group_id, label=group_id))+
  annotate(geom = "rect", xmin = as.Date("2021-04-01"), xmax=as.Date("2021-04-30"), ymin=0, ymax=Inf, fill="red", alpha=0.2)+
  annotate(geom="text", x= as.Date("2021-04-30"), y=17000, label="Economist Media Group", color="red")+
  annotate(geom = "rect", xmin = as.Date("2021-06-01"), xmax=as.Date("2021-06-15"), ymin=0, ymax=Inf, fill="yellow", alpha=0.2)+
  annotate(geom="text", x= as.Date("2021-06-01"), y=18000, label="Vox Media")+
  scale_y_continuous(labels=scales::comma)+
  # scale_color_discrete()+
  scale_color_discrete(guide="none")+
  scale_alpha_continuous(guide="none")+
  ylab("Cumulative Downloads")+
  xlab("Date")+
  ggtitle("Daily Episode-Cumulative Downloads")+
  theme_minimal()+
  theme(
    # legend.position='bottom',
    legend.position="none"
    )
ggsave('daily_cumulative_download.png', plot=last_plot(), width =12, height=4, units="in", dpi=300)

##### NAIVE REGRESSIONS (episode-level at t=14) ######
baseline <- lm(cumulative_downloads ~ past_five_ave_performance+ total_twitter_followers + total_linkedin_followers,
               data=regress_t14)
baseline_cov <- vcovHC(baseline, type="HC2")
baseline_robust_se <- sqrt(diag(baseline_cov))



baseline_trimmed <- lm(cumulative_downloads ~ past_five_ave_performance+ total_twitter_followers + total_linkedin_followers,
                      data=regress_t14 %>% filter(episode_title %notin% c('The Right And Wrong Of MMT (Modern Monetary Theory)',
                                                                          'Should We Lockdown Again?',
                                                                          "What Is The Alternative To Friedman's Capitalism?",
                                                                          'How The Supreme Court Influences Our Economy')))
baseline_trimmed_cov <- vcovHC(baseline_trimmed, type="HC2")
baseline_trimmed_robust_se <- sqrt(diag(baseline_trimmed_cov))



baseline_ads <- lm(cumulative_downloads ~ past_five_ave_performance+ total_twitter_followers + total_linkedin_followers + general_ad + specific_ad,
                   data=regress_t14_ads)
baseline_ads_cov <- vcovHC(baseline_ads, type="HC1")
baseline_ads_robust_se <- sqrt(diag(baseline_ads_cov))



baseline_ads_book <- lm(cumulative_downloads ~ past_five_ave_performance+ total_twitter_followers + total_linkedin_followers + book_promo_binary,
                         data=regress_t14_ads)
baseline_ads_book_cov <- vcovHC(baseline_ads_book, type="HC2")
baseline_ads_book_robust_se <- sqrt(diag(baseline_ads_book_cov))




baseline_ads_book_tenure <- lm(cumulative_downloads ~ past_five_ave_performance + total_twitter_followers + total_linkedin_followers + general_ad + specific_ad + tenured_guest_binary,
                               data=regress_t14_ads)
baseline_ads_book_tenure_cov <- vcovHC(baseline_ads_book_tenure, type="HC1")
baseline_ads_book_tenure_robust_se <- sqrt(diag(baseline_ads_book_tenure_cov))

## NAIVE REGRESSIONS -- OUTPUT
stargazer(baseline, baseline_trimmed, baseline_ads, baseline_ads_book, baseline_ads_book_tenure,
          se=list(
            baseline_robust_se,
            baseline_trimmed_robust_se,
            baseline_ads_robust_se,
            baseline_ads_book_robust_se,
            baseline_ads_book_tenure_robust_se),
          column.labels = c("Baseline",
                            "Trimmed",
                            "Ads",
                            "Book Promo",
                            "Tenure"),
          covariate.labels = c("Trailing Avg.",
                               "Twitter Followers",
                               "LinkedIn Followers",
                               "General Ad$^{\\dag}$ ",
                               "Specific Ad$^{\\dag}$",
                               "Book Promo$^{\\dag}$",
                               "Tenure(d) Guest$^{\\dag}$"),
          dep.var.labels = "Cumulative downloads ($t=14$)",
          title = "Naive OLS estimates",
          omit.stat=c("ser", "f"),
          table.placement = 'h',
          font.size = "small",
          no.space = TRUE, 
          align=FALSE,
          # style="aer",
          notes='$^{\\dag}$ inidcates variable is dichotomous',
          out="naive_ols_reg_results.tex"
          )



##### NAIVE REGRESSIONS (cumulative-downloads) #######
cumulative_baseline <- lm(cumulative_downloads ~ log(days_since_release_log_adjust),
                     data=regress_daily_down_fe)
cumulative_baseline_cov <- vcovHC(cumulative_baseline, type="HC2")
cumulative_baseline_robust_se <- sqrt(diag(cumulative_baseline_cov))


# Only The Breaking Point Of Democracy With Morton Shapiro and Gary Morson was released on a Friday (and all day-of-the-week effects are not significant)
cumulative_baseline_days_of_week <- lm(cumulative_downloads ~ log(days_since_release_log_adjust) + release_day_of_week + day_of_week,
                                       data=regress_daily_down_fe)
cumulative_baseline_days_of_week_cov <- vcovHC(cumulative_baseline_days_of_week, type="HC2")
cumulative_baseline_days_of_week_robust_se <- sqrt(diag(cumulative_baseline_days_of_week_cov))


cumulative_ad_periods <- lm(cumulative_downloads ~ log(days_since_release_log_adjust) + general_ad + specific_ad,
                           data=regress_daily_down_fe)
cumulative_ad_periods_cov <- vcovHC(cumulative_ad_periods, type="HC2")
cumulative_ad_periods_robust_se <- sqrt(diag(cumulative_ad_periods_cov))




cumulative_ad_general_during_post <- lm(cumulative_downloads ~ log(days_since_release_log_adjust) + general_ad + inter_general_ads + post_general_campaign,
                                        data=regress_daily_down_fe)
cumulative_ad_general_during_post_cov <- vcovHC(cumulative_ad_general_during_post, type="HC2")
cumulative_ad_general_during_post_robust_se <- sqrt(diag(cumulative_ad_general_during_post_cov))




cumulative_post_ad_interaction <- lm(cumulative_downloads ~ log(days_since_release_log_adjust) + 
                                       log(days_since_release_log_adjust):post_general_campaign,
                                     data=regress_daily_down_fe %>%
                                       filter(release_date <= ymd("2021-06-14")))
cumulative_post_ad_interaction_cov <- vcovHC(cumulative_post_ad_interaction, type="HC2")
cumulative_post_ad_interaction_robust_se <- sqrt(diag(cumulative_post_ad_interaction_cov))
cumulative_post_ad_interaction_coef <- cumulative_post_ad_interaction$coefficients


ggplot(regress_daily_down_fe %>%
         mutate(post_general_campaign = as.factor(post_general_campaign)) %>%
         filter(release_date <= ymd("2021-06-14")))+
  geom_point(aes(x=log(days_since_release_log_adjust), y=cumulative_downloads, color=post_general_campaign), alpha=0.1)+
  scale_color_manual(values = c('green','red'),
                     limits = c(1,0),
                     labels = c('pre', 'post'))+
  geom_abline(slope=cumulative_post_ad_interaction_coef[2], intercept=cumulative_post_ad_interaction_coef[1], size=1.25, color='red4')+
  geom_abline(slope=cumulative_post_ad_interaction_coef[3]+cumulative_post_ad_interaction_coef[2], intercept=cumulative_post_ad_interaction_coef[1], size=1.25, color='green4')+
  theme_minimal()




cumulative_post_ad_interaction_partial_treat <- lm(cumulative_downloads ~ log(days_since_release_log_adjust)+
                                                     log(days_since_release_log_adjust):partially_treated_general_ad,
                                                   data=regress_daily_down_fe %>%
                                                     filter(release_date<= ymd("2021-06-14")))
cumulative_post_ad_interaction_partial_treat_cov <- vcovHC(cumulative_post_ad_interaction_partial_treat, type="HC2")
cumulative_post_ad_interaction_partial_treat_robust_se <- sqrt(diag(cumulative_post_ad_interaction_partial_treat_cov))

# ggplot(regress_daily_down_fe %>% 
#          mutate(date_period = factor(case_when((post_general_campaign == 1 & partially_treated_general_ad == 1) ~ 2,
#                                                partially_treated_general_ad == 1 ~ 1,
#                                                TRUE ~ 3))))+
#   geom_point(aes(x=log(days_since_release_log_adjust), y=cumulative_downloads, color=date_period))+
#   theme_minimal()
# 



cumulative_post_ad_interaction_monthyear <- lm(cumulative_downloads ~ log(days_since_release_log_adjust):month_year:post_general_campaign,
                                               data=regress_daily_down_fe)

ggplot(regress_daily_down_fe)+
  geom_point(aes(x=log(days_since_release_log_adjust), y=cumulative_downloads, color=month_year, shape=factor(post_general_campaign)))+
  theme_minimal()+
  facet_wrap(~month_year)


cumulative_post_ad_interaction_quarteryear <- lm(cumulative_downloads ~ log(days_since_release_log_adjust)+
                                                   log(days_since_release_log_adjust):post_general_campaign+
                                                   log(days_since_release_log_adjust):quarter_year,
                                                 data=regress_daily_down_fe)
quarter_year_coefs <- cumulative_post_ad_interaction_quarteryear$coefficients

est_labels <- cumulative_post_ad_interaction_quarteryear$xlevels
est_intercepts <- quarter_year_coefs[1]
est_slopes <- c(quarter_year_coefs[2],
                quarter_year_coefs[2]+quarter_year_coefs[4],
                quarter_year_coefs[2]+quarter_year_coefs[5],
                quarter_year_coefs[2]+quarter_year_coefs[6],
                quarter_year_coefs[2]+quarter_year_coefs[7],
                quarter_year_coefs[2]+quarter_year_coefs[8],
                quarter_year_coefs[2]+quarter_year_coefs[9])

est_lines <- data.frame(
  quarter_year = est_labels,
  intercepts = est_intercepts,
  slopes = est_slopes
)
  
quarter_year_lines <- data.frame(
  intercepts = intercepts
)
  

ggplot(regress_daily_down_fe)+
  geom_point(aes(x=log(days_since_release_log_adjust), y=cumulative_downloads, color=quarter_year, shape=factor(post_general_campaign)),alpha=0.1)+
  geom_abline(data=est_lines,
              aes(slope=slopes,
                  intercept=intercepts,
                  color=quarter_year),
              size=1.25)+
  scale_color_viridis_d()+
  theme_minimal()



cumulative_economist_vox <- lm(cumulative_downloads ~ log(days_since_release_log_adjust) + 
                                economist_ad +
                                inter_general_ads + 
                                vox_ad + 
                                post_general_campaign + 
                                log(days_since_release_log_adjust):post_general_campaign,
                              data=regress_daily_down_fe %>%
                                filter(release_date <= ymd("2021-06-14")))
cumulative_economist_vox_cov <- vcovHC(cumulative_economist_vox, type="HC2")
cumulative_economist_vox_robust_se <- sqrt(diag(cumulative_economist_vox_cov))





stargazer(cumulative_baseline,
          cumulative_ad_periods,
          cumulative_ad_general_during_post,
          cumulative_post_ad_interaction,
          cumulative_post_ad_interaction_partial_treat,
          se=list(cumulative_baseline_robust_se,
            cumulative_ad_periods_robust_se,
            cumulative_ad_general_during_post_robust_se,
            cumulative_post_ad_interaction_robust_se,
            cumulative_post_ad_interaction_partial_treat_robust_se),
          column.labels = c("Baseline",
                            "Ad Campaigns",
                            "General Ads",
                            "Interaction",
                            "Interaction (fuzzy)"),
          covariate.labels = c("$\\log{(\\text{Days since release})}$",
                               "General Ad $^{\\dag}$",
                               "Specific Ad $^{\\dag}$",
                               "Btwn. General Ads $^{\\dag}$",
                               "Post-General Ads $^{\\dag}$",
                               "$\\log{(\\text{Days since release})} \\times$ Post-General",
                               "$\\log{(\\text{Days since release})} \\times$ Any General"
                               ),
          dep.var.labels = "Cumulative downloads",
          title = "Cumulative Downloads vs Log(days since release)",
          omit.stat=c("ser", "f"),
          table.placement='h',
          font.size='small',
          align=FALSE,
          # style="aer",
          out="cumulative_v_log_days_since_release.tex",
          notes='$^{\\dag}$ inidcates variable is dichotomous')






## Breakdown of general ad; inter-general; post-general.
ggplot(regress_daily_down_fe %>% 
         mutate(date_period = factor(case_when(general_ad ==1 ~ 1,
                                               inter_general_ads == 1 ~ 2,
                                               post_general_campaign == 1 ~3,
                                               TRUE ~ 4))))+
  geom_point(aes(x=log(days_since_release_log_adjust), y=cumulative_downloads, color=date_period), alpha=0.5)+
  scale_color_viridis_d(limits = c(1,2,3,4),
                        labels = c('General Ad',
                                   'Btwn. General Ads',
                                   'Post-General Ads',
                                   'Baseline'),
                        name = "Classification")+
  labs(title="General Ad Classifcations")+
  xlab(label = "Log(Days since release +1)")+
  ylab(label = "Cumulative Downloads")+
  theme_minimal() +
  facet_grid(cols=vars(date_period))+
  theme(legend.position="bottom",
        strip.text = element_blank())
ggsave('general_ad_class_plot.png', plot=last_plot(), width=8 , height=3, units='in', dpi=300)








#### FIGURE 2
reg_viridis_lab_dates <- pretty(regress_t14$release_date)
ggplot(regress_t14)+
  geom_point(aes(x=past_five_ave_performance, y=cumulative_downloads, color=release_date))+
  geom_smooth(aes(x=past_five_ave_performance, y=cumulative_downloads),
            method = lm,
            formula = y ~ x )+
  scale_color_viridis_c(breaks=as.numeric(reg_viridis_lab_dates),
                        labels=reg_viridis_lab_dates,
                        name="Release date")+
  scale_x_continuous(labels=scales::comma)+
  scale_y_continuous(labels=scales::comma)+
  labs(title="Cumulative downloads vs. past-episode performance",
       subtitle="Performance assessed at 14-days after release")+
  xlab("Past-episode performance (five-episode trailing average)")+
  ylab("Cumulative downloads")+
  theme_minimal()




feb_jul <-chars_downloads_data %>%
  filter(release_date %within% interval(ymd("2021-02-01"), ymd("2021-12-31")))


ggplot(feb_jul)+
  annotate(geom = "rect", xmin = as.Date("2021-04-01"), xmax=as.Date("2021-04-30"), ymin=0, ymax=Inf, fill="red", alpha=0.2)+
  annotate(geom = "rect", xmin = as.Date("2021-06-01"), xmax=as.Date("2021-06-15"), ymin=0, ymax=Inf, fill="yellow", alpha=0.2)+
  geom_line(aes(x=date, y=cumulative_downloads, group=episode_title, color=episode_title), size=1.5)+
  # geom_label_repel()+
  theme_minimal()+
  theme(legend.position="bottom")
ggsave('ad-campaign-episodes.png', plot=last_plot(), width=6, height=4.5, units="in")




