library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

# downloaded from https://www.investing.com/crypto/bitcoin/historical-data
bitcoin_data <- fread('data/bitcoin_data.csv') %>%
  mutate(
    Date = as.Date(Date, "%m/%d/%Y"),
    Price = as.numeric(gsub(",", "", Price)),
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low))
  ) %>% select(Date, Price, Open, High, Low)
  
p <- ggplot(bitcoin_data, aes(x=Date, y=Price)) +
  geom_line() +
  scale_y_log10()

cycle1_start_date <- bitcoin_data %>% filter(Date >= as.Date('2014-08-01') & Date <= as.Date('2018-08-01')) %>% filter(min(Price) == Price) %>% pull(Date)
cycle1_end_date <- bitcoin_data %>% filter(Date >= as.Date('2014-08-01') & Date <= as.Date('2018-08-01')) %>% filter(max(Price) == Price) %>% pull(Date)
cycle2_start_date <- bitcoin_data %>% filter(Date >= as.Date('2018-08-01') & Date <= as.Date('2022-01-01')) %>% filter(min(Price) == Price) %>% pull(Date)
cycle2_end_date <- bitcoin_data %>% filter(Date >= as.Date('2018-08-01') & Date <= as.Date('2022-01-01')) %>% filter(max(Price) == Price) %>% pull(Date)

cycle1 <- bitcoin_data %>% filter(Date>=cycle1_start_date & Date<=cycle1_end_date)
cycle2 <- bitcoin_data %>% filter(Date>=cycle2_start_date & Date<=cycle2_end_date)

get_drawdowns <- function(data, lookahead_days = 30, drawdown_threshold = 0.2) {
  data <- data %>% arrange(Date)
  drawdowns <- data.frame()
  
  for (i in 1:nrow(data)) {
    largest_drawdown <- 0
    index_price <- data$Price[i]
    end_index <- i  # track the index where max drawdown occurs
    
    for (j in i:nrow(data)) {
      if (j - i > lookahead_days) break
      
      check_price <- data$Price[j]
      drawdown <- 1 - check_price / index_price
      
      if (drawdown > largest_drawdown) {
        largest_drawdown <- drawdown
        end_index <- j
      }
    }
    
    if (largest_drawdown > drawdown_threshold) {
      dd <- data.frame(
        drawdown_start_date = data$Date[i],
        drawdown_end_date = data$Date[end_index],
        drawdown_start_price = data$Price[i],
        drawdown_end_price = data$Price[end_index],
        drawdown_percent = largest_drawdown
      )
      drawdowns <- rbind(drawdowns, dd)
    }
  }
  
  drawdowns <- drawdowns %>%
    group_by(drawdown_end_date) %>%
    slice_max(drawdown_percent, with_ties = FALSE) %>%
    ungroup()
  
  return(drawdowns)
}

plot_cycle <- function(cycle_data, cycle_drawdowns){
  p <- ggplot(cycle_data, aes(x = Date, y = Price)) +
    geom_line() +   
    geom_rect(data = cycle_drawdowns, 
              aes(xmin = drawdown_start_date, xmax = drawdown_end_date, ymin = drawdown_end_price, ymax = drawdown_start_price),
                  fill = "red", alpha = 0.05, inherit.aes = FALSE) +
    geom_segment(data = cycle_drawdowns,
                 aes(x = drawdown_start_date, y = drawdown_end_price, xend = drawdown_start_date, yend = drawdown_start_price),
                 color = "red", size = 1, alpha = 0.3) +
    geom_segment(data = cycle_drawdowns,
                 aes(x = drawdown_start_date, y = drawdown_end_price, xend = drawdown_end_date, yend = drawdown_end_price),
                 color = "red", size = 1, alpha = 0.3) +
    geom_segment(data = cycle_drawdowns,
                 aes(x = drawdown_end_date, y = drawdown_start_price, xend = drawdown_end_date, yend = drawdown_end_price),
                 color = "red", size = 1, alpha = 0.3) +
    geom_segment(data = cycle_drawdowns,
                 aes(x = drawdown_start_date, y = drawdown_start_price, xend = drawdown_end_date, yend = drawdown_start_price),
                 color = "red", size = 1, alpha = 0.3) +
    geom_text(data = cycle_drawdowns,
              aes(x = drawdown_start_date + (drawdown_end_date - drawdown_start_date) / 2,
                  y = drawdown_start_price,
                  label = paste0(round(drawdown_percent*100,1),'%'),
                  color = 'darkred'),
              vjust = -0.5,
              inherit.aes = FALSE) +
    scale_y_log10() +
    theme_bw() +
    theme(legend.position = 'none')
  
  return(p)
}

cycle1_drawdowns <- get_drawdowns(cycle1, lookahead_days = 60, drawdown_threshold = 0.25)
cycle2_drawdowns <- get_drawdowns(cycle2, lookahead_days = 175, drawdown_threshold = 0.17)

cycle1_drawdown_plot <- plot_cycle(cycle1, cycle1_drawdowns) + ggtitle('2016 Cycle Drawdowns')
cycle2_drawdown_plot <- plot_cycle(cycle2, cycle2_drawdowns %>% filter(drawdown_start_date != as.Date('2019-06-23') & drawdown_start_date != as.Date('2019-06-24')) ) + ggtitle('2020 Cycle Drawdowns')

cycle1_drawdown_plot
cycle2_drawdown_plot
