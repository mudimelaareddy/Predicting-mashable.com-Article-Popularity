
library(tidyverse)



pop <- read.csv('/Users/zhuoyinglin/OneDrive - Duke University/12 2019 Summer 2/Applied Probability and Statistics/FinalProject/OnlineNewsPopularity/OnlineNewsPopularity.csv')

drop_nonpredictive <- c('url','timedelta')
pop <- pop %>% select(-drop_nonpredictive)

pop <- pop %>%
  mutate(channel = if_else(pop$data_channel_is_lifestyle == 1, "Lifestyle",
                           if_else(pop$data_channel_is_entertainment == 1, "Entertainment",
                                   if_else(pop$data_channel_is_bus == 1, "Business",
                                           if_else(pop$data_channel_is_socmed == 1, "Social Media",
                                                   if_else(pop$data_channel_is_tech == 1, "Tech",
                                                           if_else(pop$data_channel_is_world == 1, "World","Unknown")))))))

drop_channels <- c('data_channel_is_lifestyle', 'data_channel_is_entertainment', 'data_channel_is_bus', 'data_channel_is_socmed', 'data_channel_is_tech', 'data_channel_is_world')
pop <- pop %>% select(-drop_channels)

pop$dow <- ifelse(pop$weekday_is_monday == 1, "Monday",
                  ifelse(pop$weekday_is_tuesday == 1, "Tuesday",
                         ifelse(pop$weekday_is_wednesday == 1, "Wednesday",
                                ifelse(pop$weekday_is_thursday == 1, "Thursday",
                                       ifelse(pop$weekday_is_friday == 1, "Friday",
                                              ifelse(pop$weekday_is_saturday == 1, "Saturday",
                                                     ifelse(pop$weekday_is_sunday == 1, "Sunday","Unknown")))))))
drop_dow <- c('weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday', 'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday', 'weekday_is_sunday')
pop <- pop %>% select(-drop_dow)

pop1 <- pop %>% filter(shares < 10000)

ggplot(pop, aes(shares))+
  geom_histogram(aes(fill = as.factor(is_weekend)))+
  labs(title = "Histogram of Shares (With Outliers)")

ggplot(pop1, aes(shares))+
  geom_histogram(binwidth = 150, aes(fill = as.factor(is_weekend)))+
  geom_vline(xintercept=mean(pop1$shares), color="black")+
  labs(title = "Histogram of Shares (Without Outliers)")


poplog <- pop %>% mutate(lgshares = log(shares))
ggplot(poplog, aes(lgshares))+
  geom_histogram(aes(fill = as.factor(is_weekend)))+
  labs(title = "Histogram of log(shares)")

ggplot(pop1, aes(as.factor(dow), shares))+
  geom_boxplot(aes(color = (as.factor(dow))))
#### Interaction Exploration

#channel,self_reference_avg_sharess(when the self_reference_avg_sharess goes up, the number of shares goes up)
ggplot(data = pop, aes(log(self_reference_avg_sharess), log(shares))) +
  geom_point()+
  geom_smooth(aes(color = channel), method = "lm", se = FALSE)

#channel,kw_avg_avg(positive relationship between kw_avg_avg and shares)
ggplot(data = pop, aes(log(kw_avg_avg),log(shares))) +
  geom_point()+
  geom_smooth(aes(color = channel), method = "lm", se = FALSE)

ggplot(data = pop, aes(num_videos,log(shares))) +
  geom_point()+
  geom_smooth(aes(color = dow), method = "lm", se = FALSE)


## Data Preparation

pop1 <- pop %>% filter(average_token_length != 0)
pop2 <- subset(pop1, n_unique_tokens <= 1 & n_non_stop_words <= 1 & n_non_stop_unique_tokens <= 1 )

cor_pop_prelim <- cor(pop2 %>% select (-channel,-dow))
diag(cor_pop_prelim) <- 0
cor_pop_prelim <- round(cor_pop_prelim,4)

drop_token <- c("n_non_stop_words", "n_non_stop_unique_tokens")
pop2 <- pop2 %>% select(-drop_token) # correlation = 1, keep unique tokens

drop_kw <- c('kw_min_min','kw_max_min','kw_min_max','kw_max_max','kw_min_avg','kw_max_avg')
pop2 <- pop2 %>% select(-drop_kw)

drop_polarity <- c('min_positive_polarity','max_positive_polarity',
                   'min_negative_polarity','max_negative_polarity',
                   "avg_positive_polarity", "avg_negative_polarity")
pop2 <- pop2 %>% select(-drop_polarity)


drop_self <- c("num_self_hrefs", "self_reference_min_shares", "self_reference_max_shares")
pop2 <- pop2 %>% select(-drop_self) 

drop_lda <- c("LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04")
pop2 <- pop2 %>% select(-drop_lda)

drop_pos_neg <- c("global_rate_positive_words", "global_rate_negative_words", "rate_positive_words", "rate_negative_words")
pop2 <- pop2 %>% select(-drop_pos_neg) 

drop_abs <- c("abs_title_subjectivity", "abs_title_sentiment_polarity")
pop2 <- pop2 %>% select(-drop_abs) 


## Modeling

#### Model 1: basic model

### BACKWARD SELECTION
FitAll = lm(shares ~ . , data = pop2)
step(FitAll,direction = 'backward')
# Step:  AIC=719129.1
# shares ~ n_tokens_title + n_tokens_content + n_unique_tokens + 
#     num_hrefs + num_imgs + average_token_length + num_keywords + 
#     kw_avg_avg + self_reference_avg_sharess + global_subjectivity + 
#     global_sentiment_polarity + channel + dow

### FORWARD SELECTION
fit.nothing = lm(shares ~ 1, data = pop2)
step(fit.nothing,direction = 'forward', scope=formula('FitAll'))
# Step:  AIC=719128.9
# shares ~ kw_avg_avg + channel + self_reference_avg_sharess + 
#     num_hrefs + global_subjectivity + n_tokens_title + num_keywords + 
#     average_token_length + global_sentiment_polarity + dow + 
#     num_imgs

### STEPWISE SELECTION
step(fit.nothing,direction = 'both', scope = formula('FitAll'))
# Step:  AIC=719128.9
# shares ~ kw_avg_avg + channel + self_reference_avg_sharess + 
#     num_hrefs + global_subjectivity + n_tokens_title + num_keywords + 
#     average_token_length + global_sentiment_polarity + dow + 
#     num_imgs


mod_basic <- lm(shares ~ kw_avg_avg + channel + self_reference_avg_sharess + num_hrefs + global_subjectivity + n_tokens_title + num_keywords + average_token_length + global_sentiment_polarity + dow + num_imgs, data = pop2)

summary(mod_basic)



#### Model 2: log(share) model

### BACKWARD SELECTION
FitAll = lm(log(shares) ~ . , data = pop2)
step(FitAll,direction = 'backward')
# Step: AIC = -10298.72 (with log)
# log(shares) ~ n_unique_tokens + num_hrefs + num_imgs + num_videos + 
#     average_token_length + num_keywords + kw_avg_min + kw_avg_max + 
#     kw_avg_avg + self_reference_avg_sharess + global_subjectivity + 
#     global_sentiment_polarity + title_subjectivity + title_sentiment_polarity + 
#     channel + dow


### FORWARD SELECTION
fit.nothing = lm(log(shares) ~ 1, data = pop2)
step(fit.nothing,direction = 'forward', scope=formula('FitAll'))
# Step: AIC = -10298.72 (with log)
# log(shares) ~ channel + kw_avg_avg + dow + num_hrefs + global_subjectivity + 
#     self_reference_avg_sharess + kw_avg_min + kw_avg_max + n_unique_tokens + 
#     title_sentiment_polarity + average_token_length + num_keywords + 
#     title_subjectivity + num_imgs + global_sentiment_polarity + 
#     num_videos

### STEPWISE SELECTION
step(fit.nothing,direction = 'both', scope = formula('FitAll'))
# Step: AIC = -10298.72 (with log)
# log(shares) ~ channel + kw_avg_avg + dow + num_hrefs + global_subjectivity + 
#     self_reference_avg_sharess + kw_avg_min + kw_avg_max + n_unique_tokens + 
#     title_sentiment_polarity + average_token_length + num_keywords + 
#     title_subjectivity + num_imgs + global_sentiment_polarity + 
#     num_videos

mod_log <- lm(log(shares) ~ channel + kw_avg_avg + dow + num_hrefs + global_subjectivity + self_reference_avg_sharess + kw_avg_min + kw_avg_max + n_unique_tokens + title_sentiment_polarity + average_token_length + num_keywords + title_subjectivity + num_imgs + global_sentiment_polarity + num_videos, data = pop2)

summary(mod_log)




### Model 3: log(share) with interaction between channel and kw_avg_avg

mod_log_int <- lm(log(shares) ~ channel*kw_avg_avg + kw_avg_avg + dow + num_hrefs + global_subjectivity + self_reference_avg_sharess + kw_avg_min + kw_avg_max + n_unique_tokens + title_sentiment_polarity + average_token_length + num_keywords + title_subjectivity + num_imgs + global_sentiment_polarity + num_videos, data = pop2)

summary(mod_log_int)


