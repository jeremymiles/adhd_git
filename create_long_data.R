
CreateLongData <- function(
    data = data, true_pos, true_neg, false_pos, false_neg)
  {
  dat_sens <- metafor::escalc(
    measure = "PLO", 
    xi = data[[true_pos]],                       # Access the column via name
    ni = (data[[true_pos]] + data[[false_neg]]), # Calculate the denominator
    data = data, 
    add = 0.5
  )
  dat_sens$outcome <- "sens"
  dat_sens$calculated = dat_sens[[true_pos]] / 
    (dat_sens[[true_pos]] + dat_sens[[false_neg]])
  
  # Calculate Specificity
  dat_spec <- metafor::escalc(
    measure = "PLO", 
    xi = data[[true_neg]], 
    ni = (data[[true_neg]] + data[[false_pos]]), 
    data = data, 
    add = 0.5
  )
  dat_spec$outcome <- "spec"
  dat_spec$calculated = dat_spec[[true_neg]] / 
    (dat_spec[[true_neg]] + dat_sens[[false_pos]])
  
  # Combine and filter for studies that actually have data for this type
  dat_long <- rbind(dat_sens, dat_spec)
  dat_long <- dat_long %>% 
    filter(!is.na(yi)) %>% ##############################
    arrange(desc(study))
  
  if(nrow(dat_long) == 0) {
    return(message(paste("No data for", diag_type_name, "<p>")))
  } else {
    cat(glue::glue("N Studies = {nrow(dat_long) / 2} <p>"))
  }
  
  dat_sens_spec <- dat_long %>%
    dplyr::select(
      ID, outcome, calculated) %>%
    tidyr::pivot_wider(
      id_cols = "ID",
      names_from = outcome,
      values_from = calculated
    ) %>%
    dplyr::left_join(
      data %>% select(ID, sens_reported, spec_reported)
    ) %>%
    dplyr::mutate(
      sens_calculated = round(sens * 100),
      spec_calculated = round(spec * 100)
    ) %>%
    dplyr::select(
      ID, 
      starts_with("sensitivity"), 
      sens_calculated, 
      starts_with("specificity"), 
      spec_calculated
    ) %>%
    # Get TP, TN, FP, FN from data
    dplyr::inner_join(
      data %>% dplyr::select(
        ID,
        !!sym(true_pos),
        !!sym(false_pos),
        !!sym(true_neg),
        !!sym(false_neg),
      )
    ) %>%
    dplyr::mutate(
      total_positive = !!sym(true_pos) + !!sym(false_pos),
      total_positive = !!sym(true_neg) + !!sym(false_neg),
    )
  return(list(dat_long = dat_long, dat_sens_spec = dat_sens_spec))
}