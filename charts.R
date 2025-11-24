library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)

# downloaded from https://www.investing.com/crypto/bitcoin/historical-data
bitcoin_data <- fread('data/bitcoin_data_11_23_2025.csv') %>%
  mutate(
    Date = as.Date(Date, "%m/%d/%Y"),
    Price = as.numeric(gsub(",", "", Price)),
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low))
  ) %>% select(Date, Price, Open, High, Low)
  
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

plot_cycle <- function(cycle_data, cycle_drawdowns, year_title){
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
    scale_y_log10(labels = comma) +
    theme_bw() +
    theme(legend.position = 'none') +
    labs(caption = 'Drawdowns measured from Daily Open to Daily Open')
  
  p <- p + ggtitle(paste0(year_title, ' Cycle Drawdowns'))
  
  return(p)
}

merge_overlapping_drawdowns <- function(drawdowns_df) {
  # Ensure the data is sorted by start date
  drawdowns_df <- drawdowns_df %>% arrange(drawdown_start_date)
  
  merged <- list()
  current <- drawdowns_df[1, ]
  
  for (i in 2:nrow(drawdowns_df)) {
    next_row <- drawdowns_df[i, ]
    
    if (next_row$drawdown_start_date <= current$drawdown_end_date) {
      # They overlap, merge them
      current$drawdown_end_date <- max(current$drawdown_end_date, next_row$drawdown_end_date)
      current$drawdown_start_price <- current$drawdown_start_price # keep original start price
      current$drawdown_end_price <- next_row$drawdown_end_price    # use last end price
      current$drawdown_percent <- (current$drawdown_start_price - current$drawdown_end_price) / current$drawdown_start_price
    } else {
      # No overlap, save current and move on
      merged <- append(merged, list(current))
      current <- next_row
    }
  }
  
  # Append the last interval
  merged <- append(merged, list(current))
  
  # Return as tibble
  bind_rows(merged)
}

# main
cycle1_start_date <- bitcoin_data %>% filter(Date >= as.Date('2014-08-01') & Date <= as.Date('2018-08-01')) %>% filter(min(Price) == Price) %>% pull(Date)
cycle1_end_date <- bitcoin_data %>% filter(Date >= as.Date('2014-08-01') & Date <= as.Date('2018-08-01')) %>% filter(max(Price) == Price) %>% pull(Date)
cycle2_start_date <- bitcoin_data %>% filter(Date >= as.Date('2018-08-01') & Date <= as.Date('2022-01-01')) %>% filter(min(Price) == Price) %>% pull(Date)
cycle2_end_date <- bitcoin_data %>% filter(Date >= as.Date('2018-08-01') & Date <= as.Date('2022-01-01')) %>% filter(max(Price) == Price) %>% pull(Date)
cycle3_start_date <- bitcoin_data %>% filter(Date >= as.Date('2022-08-01') & Date <= Sys.Date()) %>% filter(min(Price) == Price) %>% pull(Date)
cycle3_end_date <- Sys.Date()

cycle1 <- bitcoin_data %>% filter(Date>=cycle1_start_date & Date<=cycle1_end_date)
cycle2 <- bitcoin_data %>% filter(Date>=cycle2_start_date & Date<=cycle2_end_date)
cycle3 <- bitcoin_data %>% filter(Date>=cycle3_start_date & Date<=cycle3_end_date)

cycle1_drawdowns <- get_drawdowns(cycle1, lookahead_days = 60, drawdown_threshold = 0.25)
cycle2_drawdowns <- get_drawdowns(cycle2, lookahead_days = 175, drawdown_threshold = 0.20)
cycle3_drawdowns <- get_drawdowns(cycle3, lookahead_days = 60, drawdown_threshold = 0.20)
cycle3_drawdowns <- merge_overlapping_drawdowns(cycle3_drawdowns)

cycle1_drawdown_plot <- plot_cycle(cycle1, cycle1_drawdowns, '2016')
cycle2_drawdown_plot <- plot_cycle(cycle2, cycle2_drawdowns %>% filter(drawdown_start_date != as.Date('2019-06-23') & drawdown_start_date != as.Date('2019-06-24')), '2020')
cycle3_drawdown_plot <- plot_cycle(cycle3, cycle3_drawdowns %>% filter(drawdown_start_date != as.Date('2024-06-06')), '2024')

ggsave(plot = cycle1_drawdown_plot, filename = './plots/cycle1_drawdown_plot.png', width=10, height=6)
ggsave(plot = cycle2_drawdown_plot, filename = './plots/cycle2_drawdown_plot.png', width=10, height=6)
ggsave(plot = cycle3_drawdown_plot, filename = './plots/cycle3_drawdown_plot.png', width=10, height=6)
