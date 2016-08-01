#http://jakeruss.com/cheatsheets/stargazer.html

library("dplyr")
library("nycflights13")
library("AER") # Applied Econometrics with R
library("stargazer")
library(htmltools)

daily <- flights %>%
  filter(origin == "EWR") %>%
  group_by(year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))

daily_weather <- weather %>%
  filter(origin == "EWR") %>%
  group_by(year, month, day) %>%
  summarise(temp   = mean(temp, na.rm = TRUE),
            wind   = mean(wind_speed, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE))

# Merge flights with weather data frames
both <- inner_join(daily, daily_weather) %>%
  data.frame()  # Temporary fix

# Create an indicator for quarter
both$quarter <- cut(both$month, breaks = c(0, 3, 6, 9, 12),
                    labels = c("1", "2", "3", "4"))

# Create a vector of class logical
both$hot <- as.logical(both$temp > 85)

head(both)

output  <- lm(delay ~ temp + wind + precip, data = both)
output2 <- lm(delay ~ temp + wind + precip + quarter, data = both)

# Instrumental variables model
output3 <- ivreg(delay ~ temp + wind + precip | . - temp + hot, data = both)

summary(output)


stargazer(output, output2, type = "html",
          title            = "These are awesome results!",
          covariate.labels = c("Temperature", "Wind speed", "Rain (inches)",
                               "2nd quarter", "3rd quarter", "Fourth quarter"),
          dep.var.caption  = "A better caption",
          dep.var.labels   = "Flight delay (in minutes)") %>%
  HTML %>%
  tagList(
    tags$script('$("table").addClass("table table-condensed table-hover")')
  ) %>%
  attachDependencies(
    list(
      #rmarkdown::html_dependency_jqueryui(),
      shiny::bootstrapLib(),
      rmarkdown::html_dependency_jquery()
    )
  ) %>% browsable
