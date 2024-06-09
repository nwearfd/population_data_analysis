# 필요한 라이브러리 로드
library(tidyverse)
library(lubridate)

# 가상의 인구 통계 데이터 생성
set.seed(123)
years <- 2000:2023
population <- round(runif(length(years), 48000000, 52000000))
birth_rate <- round(runif(length(years), 8, 13), 2)
death_rate <- round(runif(length(years), 4, 8), 2)
migration_rate <- round(runif(length(years), -1, 1), 2)

# 데이터 프레임 생성
demographic_data <- data.frame(
  year = years,
  population = population,
  birth_rate = birth_rate,
  death_rate = death_rate,
  migration_rate = migration_rate
)

# 데이터 확인
head(demographic_data)

# 연도별 인구 변화 시각화
ggplot(demographic_data, aes(x = year, y = population)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("연도별 인구 변화 추이") +
  xlab("연도") +
  ylab("인구수") +
  theme_minimal()

# 출생률, 사망률, 이민율 시각화
demographic_data_long <- demographic_data %>%
  pivot_longer(cols = c(birth_rate, death_rate, migration_rate), 
               names_to = "rate_type", values_to = "rate")

ggplot(demographic_data_long, aes(x = year, y = rate, color = rate_type)) +
  geom_line() +
  geom_point() +
  ggtitle("연도별 출생률, 사망률, 이민율 변화") +
  xlab("연도") +
  ylab("비율 (‰)") +
  theme_minimal() +
  scale_color_manual(values = c("birth_rate" = "green", 
                                "death_rate" = "red", 
                                "migration_rate" = "purple"))

# 상관관계 분석
correlation_matrix <- demographic_data %>%
  select(-year) %>%
  cor()

# 상관관계 시각화
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45)

# 간단한 정책 시나리오: 출생률 증가, 사망률 감소
future_years <- 2024:2030
future_birth_rate <- 14
future_death_rate <- 3.5
future_migration_rate <- 0.5

# 현재 인구수에서 시작하여 시뮬레이션
initial_population <- demographic_data$population[length(demographic_data$population)]
future_population <- numeric(length(future_years))
future_population[1] <- initial_population + 
  (initial_population * future_birth_rate / 1000) - 
  (initial_population * future_death_rate / 1000) + 
  (initial_population * future_migration_rate / 1000)

for (i in 2:length(future_years)) {
  future_population[i] <- future_population[i-1] + 
    (future_population[i-1] * future_birth_rate / 1000) - 
    (future_population[i-1] * future_death_rate / 1000) + 
    (future_population[i-1] * future_migration_rate / 1000)
}

# 데이터 프레임 생성
future_data <- data.frame(
  year = future_years,
  population = future_population
)

# 과거 데이터와 미래 데이터 합치기
combined_data <- rbind(
  demographic_data %>% select(year, population),
  future_data
)

# 시각화
ggplot(combined_data, aes(x = year, y = population)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("인구 변화 시뮬레이션 결과") +
  xlab("연도") +
  ylab("인구수") +
  theme_minimal()

