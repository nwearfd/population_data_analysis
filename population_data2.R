# 필요한 라이브러리 로드
library(tidyverse)
library(lubridate)

# 데이터 불러오기
demographic_data <- read.csv("PopulationByYear.csv")

# 데이터 구조 확인
str(demographic_data)

# 데이터 확인
head(demographic_data)

# 수치형으로 변환
demographic_data <- demographic_data %>%
  mutate(year = as.numeric(year),
         population = as.numeric(gsub(",", "", population)),
         birth_rate = as.numeric(gsub(",", "", birth_rate)),
         death_rate = as.numeric(gsub(",", "", death_rate)),
         natural_increase_rate = as.numeric(gsub(",", "", natural_increase_rate)))

# 변환된 데이터 확인
str(demographic_data)
head(demographic_data)

# 연도별 인구 변화 시각화
ggplot(demographic_data, aes(x = year, y = population)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("연도별 인구 변화 추이") +
  xlab("연도") +
  ylab("인구수") +
  theme_minimal()

# 출생률, 사망률, 자연증감률 시각화
demographic_data_long <- demographic_data %>%
  pivot_longer(cols = c(birth_rate, death_rate, natural_increase_rate), 
               names_to = "rate_type", values_to = "rate")

ggplot(demographic_data_long, aes(x = year, y = rate, color = rate_type)) +
  geom_line() +
  geom_point() +
  ggtitle("연도별 출생률, 사망률, 자연증감률 변화") +
  xlab("연도") +
  ylab("비율 (명)") +
  theme_minimal() +
  scale_color_manual(values = c("birth_rate" = "green", 
                                "death_rate" = "red", 
                                "natural_increase_rate" = "blue"))

# 누적 출생, 사망, 자연증가 수 계산
demographic_data <- demographic_data %>%
  mutate(cumulative_births = cumsum(birth_rate),
         cumulative_deaths = cumsum(death_rate),
         cumulative_natural_increase = cumsum(natural_increase_rate))

# 누적 출생, 사망, 자연증가 수 시각화
demographic_data_long_cumulative <- demographic_data %>%
  pivot_longer(cols = c(cumulative_births, cumulative_deaths, cumulative_natural_increase), 
               names_to = "cumulative_type", values_to = "cumulative_value")

ggplot(demographic_data_long_cumulative, aes(x = year, y = cumulative_value, fill = cumulative_type)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("연도별 누적 출생, 사망, 자연증가 수 변화") +
  xlab("연도") +
  ylab("누적 수 (명)") +
  theme_minimal() +
  scale_fill_manual(values = c("cumulative_births" = "green", 
                               "cumulative_deaths" = "red", 
                               "cumulative_natural_increase" = "blue"))


# 장래데이터 불러오기
future_data <- read.csv("PopulationByFuture.csv")

# 데이터 구조 확인
str(future_data)

# 수치형으로 변환
future_data <- future_data %>%
  mutate(year = as.numeric(year),
         population = as.numeric(gsub(",", "", population)),
         birth_rate = as.numeric(gsub(",", "", birth_rate)),
         death_rate = as.numeric(gsub(",", "", death_rate)),
         natural_increase_rate = as.numeric(gsub(",", "", natural_increase_rate)))

# 변환된 데이터 확인
str(future_data)
head(future_data)

# 연도별 인구 변화 시각화
ggplot(future_data, aes(x = year, y = population)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("미래 연도별 인구 변화 추이") +
  xlab("연도") +
  ylab("인구수") +
  theme_minimal()

# 출생률, 사망률, 자연증감률 시각화
future_data_long <- future_data %>%
  pivot_longer(cols = c(birth_rate, death_rate, natural_increase_rate), 
               names_to = "rate_type", values_to = "rate")

ggplot(future_data_long, aes(x = year, y = rate, color = rate_type)) +
  geom_line() +
  geom_point() +
  ggtitle("장래 연도별 출생률, 사망률, 자연증감률 변화") +
  xlab("연도") +
  ylab("비율 (명)") +
  theme_minimal() +
  scale_color_manual(values = c("birth_rate" = "green", 
                                "death_rate" = "red", 
                                "natural_increase_rate" = "blue"))

