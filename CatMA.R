
CatMA <- function(
    data = data,
    label = "ID",
    outcome,
    comparator_analysis = TRUE,
    comparator_vs_int = comparator_vs_int
) {
  
  # Can't do both!
  if (comparator_vs_int) {
    comparator_analysis <- FALSE
  }
  
  data$ai <- data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
  data$bi <- 
    data[[glue::glue("{categorical_outcome}_n_ctrl_categ")]] - 
    data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
  data$ci <- data[[glue::glue("{categorical_outcome}_counts_int_categ")]]
  data$di <- data[[glue::glue("{categorical_outcome}_n_int_categ")]] - 
    data[[glue::glue("{categorical_outcome}_counts_int_categ")]] 
  
  if (comparator_vs_int) {
    data$ai <- data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
    data$bi <- 
      data[[glue::glue("{categorical_outcome}_n_ctrl_categ")]] - 
      data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
    
  }

  orig_data <- data
  
  categorical_outcome <- outcome

  data$outcome <- data[[outcome]]
  data$measure <- data[[paste0(outcome, "_measure_categ")]]
  data <- data %>% 
    dplyr::filter(ai > 0 & ci > 0)  # Example: Ensure cells for RR are not zero
  
  
  cat("Analyzing ", nrow(data), "studies.<p>")
  if (nrow(data) == 0) {
    cat("<p>No data.<p>")
    return(NULL)
  }
  
  
  d_es <- metafor::escalc(
    ai = ai,
    bi = bi,
    ci = ci,
    di = di,
    measure = "RR", slab = ID,
    data = data,
    append = TRUE
  )
  
  d_es$comparator <- FALSE
  
  d_es <- d_es %>%
    dplyr::filter(!is.na(yi))
  
  # Order by int_label (which is blank if no label) and then by ID
  d_es <- d_es %>%
    dplyr::arrange(int_label, ID)
  
  
  # doing comparator analysis, calculate effect sizes
  # and then join to d_es
  if (comparator_analysis) {
    data <- orig_data
    
    data$ai <- data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
    data$bi <- 
      data[[glue::glue("{categorical_outcome}_n_ctrl_categ")]] - 
      data[[glue::glue("{categorical_outcome}_counts_ctrl_categ")]]
    data$ci <-  data[[glue::glue("{categorical_outcome}_counts_comp_categ")]]
    data$di <- data[[glue::glue("{categorical_outcome}_n_comp_categ")]] - 
      data[[glue::glue("{categorical_outcome}_counts_comp_categ")]]
    
    #data$value <- data[[value]]
    data$outcome <- data[[outcome]]
    data$measure <- data[[paste0(outcome, "_measure_categ")]]
    data <- 
      data %>%
      dplyr::filter(ai > 0 & ci > 0)  # Example: Ensure cells for RR are not zero
    
    if (nrow(data) > 0) {
      d_comp_es <- metafor::escalc(
        ai = ai,
        bi = bi,
        ci = ci,
        di = di,
        measure = "RR", slab = ID, data = data,
        append = TRUE
      ) %>%
        dplyr::filter(!is.na(yi))
      
      d_comp_es <- d_comp_es %>%
        dplyr::filter(
          !(ID %in% d_es$ID)  # remove those that are already in ES
        )
      if (nrow(d_comp_es) > 0) {
        d_comp_es$comparator <- TRUE # identify these as comparator
        d_es <- dplyr::bind_rows(d_es, d_comp_es)
        
        d_es <- d_es %>% 
          dplyr::mutate(comparator = ifelse(!is.na(comparator), TRUE, FALSE)
          )
        
        d_es$ID <- ifelse(
          d_es$comparator, 
          glue::glue("{d_es$comparator} (C)"), 
          d_es$comparator
        )
      }
    }
  }
  
  
  # Reverse RR for substance use
  if (outcome %in% 
      c(
        "substance_use", "relationship", "BP", "sleep_disturbance",
        "AEs"
      )
  ) {
    d_es <- d_es %>%
      dplyr::mutate(yi = yi * -1, yi)
    
  }
  
  
  if (nrow(d_es) > 0) {
    d_es$ID <- paste0(d_es$ID, " ", d_es$int_label)
  }
  
  cat("<p>Control Types <p>")
  d_es %>% dplyr::group_by(control) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    knitr::kable() %>%
    print()
  
  if (nrow(d_es) > 0) {
    fit_ma <- metafor::rma(
      data = d_es, yi = yi, vi = vi,
      slab = ID,test="knha"
    )
    d_es %>%
      dplyr::mutate(se = sqrt(vi)) %>%
      dplyr::select(ID, measure, yi, se) %>%
      knitr::kable() %>%
      print()
    
    forest(
      fit_ma, atransf = exp, xlab = "Relative Risk", cex = 0.75, mlab = NULL,
      fonts = "Arial"
    )
    
    # Put thsi back if needed, and fix var names
    #cat(
    #  glue::glue(
    #    "Number of studies = {nrow(d_es)}<p>
    #  Intervention sample =  {sum(d_es[[n_int_categ]])} <p>
    #  Control sample = {sum(d_es[[n_ctrl_categ]])}  <p>
    #  Total sample = {sum(d_es[[n_int_categ]]) +
    #  sum(d_es[[n_ctrl_categ]])}. <p>
    #  "
    #  )
    #)
    
    cat("<p>relative Risk Estimate<p>")
    coef(summary(fit_ma)) %>%
      as.data.frame() %>%
      #dplyr::select(estimate, ci.lb, ci.ub) %>%
      round(2)%>%
      exp() %>%
      knitr::kable()
    
    cat("<p>I-squared = ", summary(fit_ma)$I2 %>% round(), "%<p>")
    
    if (nrow(d_es) > 2) {
      cat("<h5>Publication Bias Tests</h5>")
      cat("<h6>Begg (non-parametric)</h6>")
      ranktest(fit_ma) %>% capture.output() %>% cat()
      cat("<h6>Egger (parametric)</h6>")
      cat(regtest(fit_ma) %>% capture.output(), sep = "\n" )
      
      
      cat("<h6>Trim and Fill</h6>")
      
      cat("Estimate and Cis<p>")
      coef(summary(trimfill(fit_ma))) %>%
        as.data.frame() %>%
        dplyr::select(estimate, ci.lb, ci.ub) %>%
        dplyr::mutate(
          estimate = exp(estimate),
          ci.lb = exp(ci.lb),
          ci.ub = exp(ci.ub)
        ) %>%
        knitr::kable() %>% print()
      cat("<p>TrimFill Relative Risk Estimate: ",
          round(exp(coef(trimfill(fit_ma, stepadj=0.5, maxiter=10000)))), "<p>")
      
      cat("<h6>int_stimulant vs int_nonstimulant meta-regression</h6>")
      
      StimMetaReg(d_es)
      
    }
    
    
    return(fit_ma)
    
    
    
  } else {
    return("No data for meta-analysis.<p>")
  }
}
