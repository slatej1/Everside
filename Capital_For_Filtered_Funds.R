capital_for_multiple_funds <- function(input_fund = c("Fund I Founders"), x_axis = "date") {
  
  ingested_capital_data <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    )
  
  everside_capital_data <- ingested_capital_data %>% 
    arrange(fund,
            date) %>% 
    mutate(
      facility_max = case_when(
        fund == "Direct I" ~ 86080000,
        fund == "Direct II" ~ 135000000,
        fund == "Fund I Founders" ~ 28900000,
        fund == "Fund I Suncap" ~ 44000000,
        fund == "Fund II" ~ 240000000,
        fund == "Fund III" ~ 510000000,
        fund == "Fund IV" ~ 601000000,
        TRUE ~ NA_real_
      ),
      cumulative_called_percent = (-1 * cumulative_called)/facility_max,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    tibble()
  
  all_everside_capital_data_called <- ingested_capital_data %>% 
    filter(call_dist == "Call",
           fund %in% input_fund) %>% 
    arrange(date) %>% 
    mutate(
      cumulative_called = cumsum(amount)
    )
  
  all_everside_capital_data_distribution <- ingested_capital_data %>% 
    filter(call_dist == "Distribution",
           fund %in% input_fund) %>% 
    arrange(date) %>% 
    mutate(
      cumulative_distribution = cumsum(amount)
    )
  
  all_everside_capital <- merge(
    merge(
      merge(
        ingested_capital_data,
        all_everside_capital_data_called,
        by = c(
          "fund",
          "entity",
          "call_dist",
          "date",
          "amount",
          "days",
          "years",
          "fund_max_age",
          "fund_max_days",
          "cumulative_distribution"
        ),
        all.x = TRUE,
        suffixes = c("", "_agg")
      ),
      all_everside_capital_data_distribution,
      by = c(
        "fund",
        "entity",
        "call_dist",
        "date",
        "amount",
        "days",
        "years",
        "fund_max_age",
        "fund_max_days",
        "cumulative_called"
      ),
      all.x = TRUE,
      suffixes = c("", "_agg")
    ),
    everside_capital_data,
    by = c(
      "fund",
      "entity",
      "call_dist",
      "date",
      "amount",
      "days",
      "years",
      "fund_max_age",
      "fund_max_days",
      "cumulative_called"
    ),
    all.x = TRUE,
    suffixes = c("", "_facility_max")
  )
  
  all_everside_capital_filled <- all_everside_capital %>% 
    arrange(date) %>% 
    fill(cumulative_called_agg) %>% 
    fill(cumulative_distribution_agg) %>% 
    mutate(
      cumulative_called = cumulative_called_agg,
      cumulative_distribution = cumulative_distribution_agg,
      cumulative_called_percent = (-1 * cumulative_called_agg)/facility_max,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    select(
      -cumulative_called_agg,
      -cumulative_distribution_agg,
      -cumulative_distribution_facility_max,
      
    )

  if (x_axis == "years") {
    
    one_everside_agg_distribution <- all_everside_capital_filled %>% 
      select(
        years,
        one_everside_agg_distribution = cumulative_distribution_percent
      )
    
    gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = years)) +
      geom_line(aes(y = one_everside_agg_distribution), color = "red")
    
  } else {
    
    one_everside_agg_distribution <- all_everside_capital_filled %>% 
      select(
        date,
        one_everside_agg_distribution = cumulative_distribution_percent
      )
    
    gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = date)) +
      geom_line(aes(y = one_everside_agg_distribution), color = "red")
    
  }
  
  gfg_plot_one_everside_agg
  
}

capital_for_multiple_funds_with_commitments <- function(input_fund = c("Fund I Founders", "Fund II", "Fund III"), commitments = c(1000000, 2000000, 50000), x_axis = "date") {
  
  ingested_capital_data <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    )
  
  input_fund_number <- input_fund %>% 
    data.table() %>% 
    mutate(
      row_num = row_number()
    )
  
  fund_i_founders_number <- input_fund_number %>%
    filter(. == "Fund I Founders") %>% 
    select(row_num) %>% 
    pull()
  
  fund_i_suncap_number <- input_fund_number %>%
    filter(. == "Fund I Suncap") %>% 
    select(row_num) %>% 
    pull()
  
  fund_ii_number <- input_fund_number %>%
    filter(. == "Fund II") %>% 
    select(row_num) %>% 
    pull()
  
  fund_iii_number <- input_fund_number %>%
    filter(. == "Fund III") %>% 
    select(row_num) %>% 
    pull()
  
  fund_iv_number <- input_fund_number %>%
    filter(. == "Fund IV") %>% 
    select(row_num) %>% 
    pull()
  
  direct_i_number <- input_fund_number %>%
    filter(. == "Direct I") %>% 
    select(row_num) %>% 
    pull()
  
  direct_ii_number <- input_fund_number %>%
    filter(. == "Direct II") %>% 
    select(row_num) %>% 
    pull()
  
  direct_i_min_date <- as.Date("2021-07-22")
  
  direct_ii_min_date <- as.Date("2023-08-09")
  
  fund_i_founders_min_date <- as.Date("2017-08-03")
  
  fund_i_suncap_min_date <- as.Date("2017-12-29")
  
  fund_ii_min_date <- as.Date("2019-06-28")
  
  fund_iii_min_date <- as.Date("2021-11-02")
  
  fund_iv_min_date <- as.Date("2023-07-13")
  
  everside_capital_data <- ingested_capital_data %>% 
    arrange(fund,
            date) %>% 
    filter(fund %in% input_fund) %>% 
    mutate(
      facility_max = case_when(
        fund == "Direct I" & length(direct_i_number) > 0 ~ 86080000,
        fund == "Direct II" & length(direct_ii_number) > 0 ~ 135000000,
        fund == "Fund I Founders" & length(fund_i_founders_number) > 0 ~ 28900000,
        fund == "Fund I Suncap" & length(fund_i_suncap_number) > 0 ~ 44000000,
        fund == "Fund II" & length(fund_ii_number) > 0 ~ 240000000,
        fund == "Fund III" & length(fund_iii_number) > 0 ~ 510000000,
        fund == "Fund IV" & length(fund_iv_number) > 0 ~ 601000000,
        TRUE ~ 0
      ),
      direct_i_facility_max = 86080000,
      direct_ii_facility_max = 135000000,
      fund_i_founders_facility_max = 28900000,
      fund_i_suncap_facility_max = 44000000,
      fund_ii_facility_max = 240000000,
      fund_iii_facility_max = 510000000,
      fund_iv_facility_max = 601000000,
      cumulative_called_percent = ifelse(facility_max == 0, 0, (-1 * cumulative_called)/facility_max),
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called),
      fund_i_founders_commitment = ifelse(length(fund_i_founders_number) > 0,
                                          commitments[[fund_i_founders_number]],
                                          0),
      fund_i_suncap_commitment = ifelse(length(fund_i_suncap_number) > 0,
                                          commitments[[fund_i_suncap_number]],
                                          0),
      fund_ii_commitment = ifelse(length(fund_ii_number) > 0,
                                        commitments[[fund_ii_number]],
                                        0),
      fund_iii_commitment = ifelse(length(fund_iii_number) > 0,
                                  commitments[[fund_iii_number]],
                                  0),
      fund_iv_commitment = ifelse(length(fund_iv_number) > 0,
                                  commitments[[fund_iv_number]],
                                  0),
      direct_i_commitment = ifelse(length(direct_i_number) > 0,
                                  commitments[[direct_i_number]],
                                  0),
      direct_ii_commitment = ifelse(length(direct_ii_number) > 0,
                                   commitments[[direct_ii_number]],
                                   0)
    ) %>% 
    arrange(date) %>% 
    mutate(
      amount = case_when(
        fund == "Fund I Founders" & fund_i_founders_commitment == 0 ~ 0,
        fund == "Fund I Suncap" & fund_i_suncap_commitment == 0 ~ 0,
        fund == "Fund II" & fund_ii_commitment == 0 ~ 0,
        fund == "Fund III" & fund_iii_commitment == 0 ~ 0,
        fund == "Fund IV" & fund_iv_commitment == 0 ~ 0,
        fund == "Direct I" & direct_i_commitment == 0 ~ 0,
        fund == "Direct II" & direct_ii_commitment == 0 ~ 0,
        TRUE ~ amount
      ),
      facility_max_aggreagte = ifelse(
        (date >= fund_i_founders_min_date & fund_i_founders_commitment > 0),
        fund_i_founders_facility_max,
        0
      ) + ifelse(
        (date >= fund_i_suncap_min_date & fund_i_suncap_commitment > 0),
        fund_i_suncap_facility_max,
        0
      ) + ifelse(
        (date >= fund_ii_min_date & fund_ii_commitment > 0),
        fund_ii_facility_max,
        0
      ) + ifelse(
        (date >= fund_iii_min_date & fund_iii_commitment > 0),
        fund_iii_facility_max,
        0
      ) + ifelse(
        (date >= fund_iv_min_date & fund_iv_commitment > 0),
        fund_iv_facility_max,
        0
      ) + ifelse(
        (date >= direct_i_min_date & direct_i_commitment > 0),
        direct_i_facility_max,
        0
      ) + ifelse(
        (date >= direct_ii_min_date & direct_ii_commitment > 0),
        direct_ii_facility_max,
        0
      )
    ) %>% 
    tibble()
  
  everside_capital_calls <- everside_capital_data %>% 
    filter(call_dist == "Call") %>% 
    mutate(
      cumulative_called = cumsum(amount)
    )
  
  everside_capital_distributions <- everside_capital_data %>% 
    filter(call_dist == "Distribution") %>% 
    mutate(
      cumulative_distribution = cumsum(amount)
    )
  
  all_everside_capital <- merge(
      merge(
        everside_capital_data,
        everside_capital_calls,
        by = c(
          "fund",
          "entity",
          "call_dist",
          "date",
          "amount",
          "days",
          "years",
          "fund_max_age",
          "fund_max_days",
          "cumulative_distribution"
        ),
        all.x = TRUE,
        suffixes = c("", "_called")
      ),
      everside_capital_distributions,
      by = c(
        "fund",
        "entity",
        "call_dist",
        "date",
        "amount",
        "days",
        "years",
        "fund_max_age",
        "fund_max_days",
        "cumulative_called"
      ),
      all.x = TRUE,
      suffixes = c("", "_distributed")
    ) %>% 
    arrange(date)
    
  
  all_everside_capital_filled <- all_everside_capital %>% 
    arrange(date) %>% 
    fill(cumulative_called_called) %>% 
    fill(cumulative_distribution_distributed) %>% 
    mutate(
      cumulative_called = cumulative_called_called,
      cumulative_distribution = cumulative_distribution_distributed,
      cumulative_called_percent = ifelse(is.na(facility_max_aggreagte), 0, (-1 * cumulative_called)/facility_max_aggreagte),
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    select(
      -cumulative_called_called,
      -cumulative_distribution_distributed
    )
  
  total_called_percent <- all_everside_capital_filled %>% summarize(max(cumulative_called_percent, na.rm = TRUE)) %>% pull()
  
  total_called <- sum(commitments) * total_called_percent
  
  total_distributed <- 0
  
  i <- 1
  
  for (i in 1:length(input_fund)){
    
    everside_capital_calls <- everside_capital_data %>% 
      filter(call_dist == "Call",
             fund == input_fund[[i]]) %>% 
      mutate(
        cumulative_called = cumsum(amount)
      )
    
    everside_capital_distributions <- everside_capital_data %>% 
      filter(call_dist == "Distribution",
             fund == input_fund[[i]]) %>% 
      mutate(
        cumulative_distribution = cumsum(amount)
      )
    
    everside_capital_data_filtered <- everside_capital_data %>% 
      filter(fund == input_fund[[i]])
    
    all_everside_capital_filtered <- merge(
      merge(
        everside_capital_data_filtered,
        everside_capital_calls,
        by = c(
          "fund",
          "entity",
          "call_dist",
          "date",
          "amount",
          "days",
          "years",
          "fund_max_age",
          "fund_max_days",
          "cumulative_distribution"
        ),
        all.x = TRUE,
        suffixes = c("", "_called")
      ),
      everside_capital_distributions,
      by = c(
        "fund",
        "entity",
        "call_dist",
        "date",
        "amount",
        "days",
        "years",
        "fund_max_age",
        "fund_max_days",
        "cumulative_called"
      ),
      all.x = TRUE,
      suffixes = c("", "_distributed")
    ) %>% 
      arrange(date) %>% 
      mutate(
        facility_max_aggreagte = case_when(
          input_fund[[i]] == "Direct I" ~ 86080000,
          input_fund[[i]] == "Direct II" ~ 135000000,
          input_fund[[i]] == "Fund I Founders" ~ 28900000,
          input_fund[[i]] == "Fund I Suncap" ~ 44000000,
          input_fund[[i]] == "Fund II" ~ 240000000,
          input_fund[[i]] == "Fund III" ~ 510000000,
          input_fund[[i]] == "Fund IV" ~ 601000000,
          TRUE ~ NA_real_
        )
      )
    
    
    all_everside_capital_filled_filtered <- all_everside_capital_filtered %>% 
      arrange(date) %>% 
      fill(cumulative_called_called) %>% 
      fill(cumulative_distribution_distributed) %>% 
      mutate(
        cumulative_called = cumulative_called_called,
        cumulative_distribution = cumulative_distribution_distributed,
        cumulative_called_percent = ifelse(is.na(facility_max_aggreagte), 0, (-1 * cumulative_called)/facility_max_aggreagte),
        cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
      ) %>% 
      select(
        -cumulative_called_called,
        -cumulative_distribution_distributed
      )
    
    total_called_percent_filtered <- all_everside_capital_filled_filtered %>% summarize(max(cumulative_called_percent, na.rm = TRUE)) %>% pull()
    
    total_called_filtered <- commitments[[i]] * total_called_percent_filtered
    
    total_distributed_percent_filtered <- all_everside_capital_filled_filtered %>% summarize(max(cumulative_distribution_percent, na.rm = TRUE)) %>% pull()
    
    total_distributed_filtered <- sum(total_called_filtered) * total_distributed_percent_filtered
    
    total_distributed <- total_distributed_filtered + total_distributed
    
    i <- i + 1
    
  }
  
  total_distributed_percent <- total_distributed / total_called
  
  print(paste0("Total amount called: ", total_called))
  print(paste0("With a total distribution % of: ", total_distributed_percent))
  print(paste0("Yields a total amount distributed of: ", total_distributed))
  
}
