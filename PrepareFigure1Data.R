PrepareFigure1Data <- function(x) {
  purrr::map(
    x,
    \(x) dplyr::slice(x, -(1:29))
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      outcome = rep(c("Language", "Mathematics", "Intelligence"), each = 23),
      `Antimicrobial prescription` = rep(
        c(
          "  Any number of prescriptions", "    One", "    Two", "    Three", 
          "    Four or more", "  Any \u03b2-lactam", 
          "    Any \u03b2-lactamase sensitive penicillin",
          "    Any \u03b2-lactamase resistent penicillin", 
          "    Extended-spectrum penicillin", "    Cephalosporin*",
          "  Any macrolide", "    Azithromycin", "    Erythromycin",
          "    Roxithromycin", "    Other macrolide*", "  Trimethoprim*", 
          "  Sulfonamide", "  Quinolone*", "  Triazole antifungal", 
          "  Nucleotide/nucleoside-analogue", "  Any number of hospitalizations", "    One ", "    Two or more"
        ), 
        3
      )
    ) |>
    dplyr::select(`Antimicrobial prescription`, outcome, estimate, conf.low, conf.high, p.value) |>
    dplyr::mutate(
      estimate_tbl = ifelse(
        outcome == "Intelligence", 
        sprintf("%.3f", 15 * estimate), 
        sprintf("%.3f", estimate)
      ),
      conf_low_tbl = ifelse(
        outcome == "Intelligence", 
        sprintf("%.3f", 15 * conf.low), 
        sprintf("%.3f", conf.low)
      ),
      conf_high_tbl = ifelse(
        outcome == "Intelligence", 
        sprintf("%.3f", 15 * conf.high), 
        sprintf("%.3f", conf.high)
      ),
      conf_int = ifelse(
        outcome == "Intelligence", 
        sprintf("(%.3f to %.3f)", 15 * conf.low, 15 * conf.high), 
        sprintf("(%.3f to %.3f)", conf.low, conf.high)
      ),
      p_value =  sprintf("%.3f", p.value)
    )
}