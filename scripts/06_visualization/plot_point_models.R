## David Klinges
## This script plots outputs from point-based models

## Prep data for plotting #########

prep_for_plots <- function(hourlydata_microclimc = hourlydata_microclimc, 
                           nichemapr_out = FALSE,
                           empirical_air_temp = empirical_air_temp,
                           data_out = microclimc_out[[2]],
                           plot_reference_climate = FALSE,
                           site) {
  
  print("Prepping data for plotting...")
  ## Plot results ####################
  
  ## A. Prep for plotting ####################
  
  # Extract empirical temperatures at required height
  empirical_air_temp <- empirical_air_temp %>%
    mutate(obs_time = ymd_hms(obs_time)) %>%
    # Filter to timeseries that was modeled
    filter(obs_time %in% c(data_out$obs_time))
  
  est_emp_plotdata_wide <- data_out %>%
    dplyr::select(obs_time, tout) %>%
    full_join(empirical_air_temp) %>%
    filter(complete.cases(empirical_temp) & complete.cases(tout)) %>%
    # Calculate error
    mutate(rel_error = (empirical_temp - tout) / tout)
  
  est_emp_plotdata_tall <- est_emp_plotdata_wide %>%
    pivot_longer(cols = c(tout, empirical_temp), names_to = "type", values_to = "temp") %>%
    # Recode for plotting
    mutate(type = recode(type, "empirical_temp" = "Empirical", "tout" = "Estimated"))
  
  # If user desires, join in reference air temperature
  if (plot_reference_climate) {
    hourlydata_microclimc <- hourlydata_microclimc %>%
      mutate(obs_time = as.POSIXct(obs_time)) %>%
      mutate(ref_air_temp = hourlydata_microclimc$temp)
    
    empirical_air_temp <- empirical_air_temp %>%
      left_join(dplyr::select(hourlydata_microclimc, obs_time, ref_air_temp))
    
    est_emp_ref_plotdata_wide <- data_out %>%
      dplyr::select(obs_time, tout) %>%
      full_join(empirical_air_temp) %>%
      filter(complete.cases(empirical_temp) & complete.cases(tout)) %>%
      # Calculate error
      mutate(rel_error = (tout - empirical_temp) / empirical_temp)
    
    est_emp_ref_plotdata_tall <- est_emp_ref_plotdata_wide %>%
      pivot_longer(cols = c(tout, empirical_temp, ref_air_temp), names_to = "type",
                   values_to = "temp") %>%
      # Recode for plotting
      mutate(type = recode(type, "empirical_temp" = "Empirical", "tout" = "Estimated",
                           "ref_air_temp" = "Reference"))
  } else {  
    est_emp_ref_plotdata_wide <- data_out %>%
      dplyr::select(obs_time, tout) %>%
      full_join(empirical_air_temp) %>%
      filter(complete.cases(empirical_temp) & complete.cases(tout)) %>%
      # Calculate error
      mutate(rel_error = (tout - empirical_temp) / empirical_temp)
    
    est_emp_ref_plotdata_tall <- est_emp_ref_plotdata_wide %>%
      pivot_longer(cols = c(tout, empirical_temp), names_to = "type",
                   values_to = "temp") %>%
      # Recode for plotting
      mutate(type = recode(type, "empirical_temp" = "Empirical", "tout" = "Estimated"))
  }
  
  # Only if NicheMapR ran correctly and is desired
  if (class(nichemapr_out) != "logical") {
    taloc <- unique(as.data.frame(nichemapr_out$shadmet)$TALOC)
    if (all(!is.na(taloc) & length(taloc) > 1)) {
      ## Combine model outputs: shadmet
      shadmet <- as.data.frame(nichemapr_out$shadmet)
      soil <- as.data.frame(nichemapr_out$soil)
      shadsoil <- as.data.frame(nichemapr_out$shadsoil)
      
      # going to have to assume that it's just one year....for now
      est_emp_plotdata_wide <- shadmet %>% 
        mutate(date = as.Date(DOY, origin = paste0(unique(year(data_out$obs_time)), 
                                                   "-01-01"))) %>% 
        mutate(time = paste0(TIME/60, ":00:00")) %>%  
        mutate(obs_time = ymd_hms(paste0(date, time))) %>%
        inner_join(est_emp_ref_plotdata_wide) %>% 
        dplyr::select(obs_time, TALOC, ref_air_temp, tout, empirical_temp) %>% 
        # Calculate error
        mutate(microclimc_rel_error = (tout - empirical_temp) / empirical_temp) %>% 
        mutate(nichemapr_rel_error = (TALOC - empirical_temp) / empirical_temp)
      
      est_emp_ref_plotdata_tall <- est_emp_plotdata_wide %>% 
        pivot_longer(cols = c(TALOC, ref_air_temp, tout, empirical_temp), names_to = "type",
                     values_to = "temp") %>%
        # Recode for plotting
        mutate(type = recode(type, "empirical_temp" = "Empirical", "tout" = "Microclimc Estimated",
                             "TALOC" = "NicheMapR Estimated", "ref_air_temp" = "Reference"))
    } else {
      nichemapr_out <- FALSE
    }
  }
  
  # Find first complete (or near-complete) year
  est_emp_ref_plotdata_tall <- est_emp_ref_plotdata_tall %>%
    mutate(yday = yday(obs_time)) %>% 
    mutate(year = year(obs_time)) %>% 
    mutate(month = month(obs_time)) %>% 
    mutate(day = day(obs_time))
  
  # Count days of year
  count_yday <- est_emp_ref_plotdata_tall %>% 
    dplyr::select(year, yday) %>% 
    distinct() %>% 
    group_by(year) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(-n) %>% 
    slice(1) # Choose only most complete year
  
  week_tall <- est_emp_ref_plotdata_tall %>% 
    right_join(count_yday)
  
  months <- unique(week_tall$month)
  
  # Randomly sample 3 months if there's more than 2
  if (length(months) > 2) {
    months <- sample(months, size = 3)
  }
  
  week_tall <- week_tall %>%
    filter(month %in% months)
  
  days <- unique(week_tall$day)
  
  # Randomly sample 10 conscutive days if there's more than 10
  if (length(days) > 10) {
    indices <- seq(length(days) - 10 + 1)
    first_index <- sample(indices, 1)
    days <- days[first_index:(first_index + 10 -1)]
  }
  
  week_tall <- week_tall %>%
    filter(day %in% days)

  day_tall <- week_tall %>% 
    filter(day %in% unique(week_tall$day)[1:3])
  
  ## Flag output datasets with site
  est_emp_plotdata_tall$site <- site
  est_emp_plotdata_wide$site <- site
  week_tall$site <- site
  day_tall$site <- site
  
  ## Return prepped data 
  return(list(est_emp_plotdata_tall = est_emp_plotdata_tall,
              est_emp_plotdata_wide = est_emp_plotdata_wide,
              est_emp_ref_plotdata_tall = est_emp_ref_plotdata_tall,
              week_tall = week_tall,
              day_tall = day_tall))
}


## Plot #########################
plot_model_outs <- function(site = site, 
                            plot_ready_data = plot_ready_data,
                            nichemapr_out = FALSE,
                            model) {
  
  
  # For plotting
  library(wesanderson, quietly = TRUE)
  library(RColorBrewer, quietly = TRUE)
  require(grid)
  require(gridExtra)
  
  est_emp_plotdata_tall <- plot_ready_data$est_emp_plotdata_tall
  est_emp_plotdata_wide <- plot_ready_data$est_emp_plotdata_wide
  est_emp_ref_plotdata_tall <- plot_ready_data$est_emp_ref_plotdata_tall
  week_tall <- plot_ready_data$week_tall
  day_tall <- plot_ready_data$day_tall

  ## B. Generate plots #################
  
  ## Color palettes
  print("Generating plots...")
  
  # trying, in case user doesn't have packages installed
  pal_1 <- try(wes_palette("Chevalier1", length(unique(est_emp_plotdata_tall$type)), 
                           type = "discrete"), silent = TRUE)
  if (class(pal_1) == "try-error") {
    pal_1 <- palette()
  }
  
  pal_2 <- try(suppressWarnings(brewer.pal(n = length(unique(est_emp_ref_plotdata_tall$type)), 
                            name = "Dark2")), silent = TRUE)
    if (class(pal_2) == "try-error") {
      pal_2 <- palette()
    }
    
  ## .... Full (modeled) timeseries ##################
  
  est_emp_time <- ggplot(est_emp_plotdata_tall, aes(obs_time, temp, group = type)) +
    geom_point(aes(color = type), alpha = 0.35) +
    geom_line(data = filter(est_emp_plotdata_tall, type == "Empirical"),
              aes(obs_time, temp), color = pal_1[1], alpha = 0.5) +
    geom_line(data = filter(est_emp_plotdata_tall, type == "Estimated"),
              aes(obs_time, temp), color = pal_1[2], alpha = 0.5) +
    scale_color_manual(values = pal_1) +
    labs(color = "",
         x = "Time",
         y = "Temperature",
         title = "Empirical Below-Canopy and Estimated Below-Canopy, 1 year") +
    theme(axis.text = element_text(size = 20),
          legend.text = element_text(size = 20)) +
    theme_classic()
  
  ## .... One year #####################
  
  est_emp_ref_year <- ggplot(est_emp_ref_plotdata_tall, aes(obs_time, temp, group = type)) +
    # geom_point(aes(color = type), alpha = 0.35) +
    geom_line(aes(color = type)) +
    # geom_line(data = filter(est_emp_plotdata_tall, type == "Estimated"),
    #           aes(obs_time, temp), color = "yellow", alpha = 0.5) +
    scale_color_manual(values = pal_2) +
    labs(color = "",
         x = "Time",
         y = "Temperature") +
    theme(axis.text = element_text(size = 20),
          legend.text = element_text(size = 20)) +
    theme_classic()
  
  ## .... Weekish #################
  
  est_emp_ref_week <- ggplot(week_tall, aes(obs_time, temp, group = type)) +
    # geom_point(aes(color = type), alpha = 0.35) +
    geom_line(aes(color = type), size = 1.3) +
    # geom_line(data = filter(est_emp_plotdata_tall, type == "Estimated"),
    #           aes(obs_time, temp), color = "yellow", alpha = 0.5) +
    scale_color_manual(values = pal_2) +
    labs(color = "",
         x = "Time",
         y = "Temperature",
         title = "Empirical Below-Canopy and Estimated Below-Canopy, 10 days") +
    theme(axis.text = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.text.x = element_text(angle = 75)) +
    theme_classic() +
    facet_wrap(~month, scales = "free")
  
  ## .... A few days ##################
  
  est_emp_ref_day <- ggplot(day_tall, aes(obs_time, temp, group = type)) +
    # geom_point(aes(color = type), alpha = 0.35) +
    geom_line(aes(color = type), size = 1.3) +
    # geom_line(data = filter(est_emp_plotdata_tall, type == "Estimated"),
    #           aes(obs_time, temp), color = "yellow", alpha = 0.5) +
    scale_color_manual(values = pal_2) +
    labs(color = "",
         x = "Time",
         y = "Temperature",
         title = "Empirical Below-Canopy and Estimated Below-Canopy, a few days") +
    theme(axis.text = element_text(size = 20),
          legend.text = element_text(size = 20)) +
    theme_classic() +
    facet_wrap(~month, scales = "free")
  
  ## .... 1-for-1 plot ####################
  
  if (class(nichemapr_out) != "logical") {
    microclimc_emp_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, tout)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1) +
      lims(y = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T)),
           x = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T))) +
      labs(color = "",
           x = "Empirical",
           y = "Estimated",
           title = "Empirical x microclimc Estimate, Below-Canopy temperatures") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
    
    nichemapr_emp_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, TALOC)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1) +
      lims(y = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T)),
           x = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T))) +
      labs(color = "",
           x = "Empirical",
           y = "Estimated",
           title = "Empirical x NicheMapR Estimate, Below-Canopy temperatures") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
    
    est_emp_error <- arrangeGrob(microclimc_emp_error, nichemapr_emp_error)
    
  } else {
    est_emp_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, tout)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1) +
      lims(y = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T)),
           x = c(min(est_emp_plotdata_wide$tout, na.rm = T), max(est_emp_plotdata_wide$tout, na.rm = T))) +
      labs(color = "",
           x = "Empirical",
           y = "Estimated",
           title = "Empirical x Estimated Below-Canopy temperatures") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
  }
  
  ## .... Relative Error #########################
  
  if (class(nichemapr_out) != "logical") {
    microclimc_abs_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, microclimc_rel_error)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0) +
      labs(color = "",
           x = "Empirical",
           y = "Relative Error",
           title = "Relative Error in Below-Canopy microclimc Estimates") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
    
    nichemapr_abs_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, nichemapr_rel_error)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0) +
      labs(color = "",
           x = "Empirical",
           y = "Relative Error",
           title = "Relative Error in Below-Canopy NicheMapR Estimates") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
    
    emp_abs_error <- arrangeGrob(microclimc_abs_error, nichemapr_abs_error)
    
  } else {
    emp_abs_error <- ggplot(est_emp_plotdata_wide, aes(empirical_temp, rel_error)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0) +
      labs(color = "",
           x = "Empirical",
           y = "Relative Error",
           title = "Relative Error in Below-Canopy Model Estimates") +
      theme(axis.text = element_text(size = 20),
            legend.text = element_text(size = 20)) +
      theme_classic()
  }
  ## ......** Save plots ##################
  
  print("Saving plots")
  # Check if directories exist, if not make them
  main_dir <- paste0(getwd(), "/figures/model_testing/microclimc")
  dir.create(file.path(main_dir, site), showWarnings = FALSE)
  dir.create(file.path(main_dir, site, model), showWarnings = FALSE)
  
  
  suppressMessages(ggsave(plot = est_emp_time, filename = paste0(main_dir, "/", 
                          site, "/", model, "/est_emp_time.png"),
                          height = 7.2, width = 14))
  suppressMessages(ggsave(plot = est_emp_ref_year, filename = paste0(main_dir, "/", 
                          site, "/", model, "/est_emp_ref_year.png"),
                          height = 7.2, width = 14))
  suppressMessages(ggsave(plot = est_emp_ref_week, filename = paste0(main_dir, "/", 
                          site, "/", model, "/est_emp_ref_week.png"),
                          height = 7.2, width = 14))
  suppressMessages(ggsave(plot = est_emp_ref_day, filename = paste0(main_dir, "/", 
                          site, "/", model, "/est_emp_ref_day.png"),
                          height = 7.2, width = 14))
  suppressMessages(ggsave(plot = est_emp_error, filename = paste0(main_dir, "/", 
                          site, "/", model, "/est_emp_error.png"),
                          height = 7.2, width = 14))
  suppressMessages(ggsave(plot = emp_abs_error, filename = paste0(main_dir, "/", 
                          site, "/", model, "/emp_abs_error.png"),
                          height = 7.2, width = 14))
  
  ## Return plots ################
  
  return(list(est_emp_time = est_emp_time, est_emp_ref_year = est_emp_ref_year, 
              est_emp_ref_week = est_emp_ref_week, est_emp_ref_day = est_emp_ref_day,
              est_emp_error = est_emp_error, emp_abs_error = emp_abs_error))
}



