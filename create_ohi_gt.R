#' Create OHI "gt" Table. This is used in the Onboarding Book using the schedule
#' Great gt resources: 
#' - https://evamaerey.github.io/tables/about
#' - http://www.danieldsjoberg.com/gt-and-gtsummary-presentation/
#' 
#' @param wrangled_dt a wrangled data frame to make as a gt table
#'
#' @return sched_gt a formatted table
#' @export
#'
#' @examples
#' 
#' Troubleshooting: wrangled_dt <- sched

create_ohi_gt <- function(wrangled_dt) {
  
  sched_gt <- wrangled_dt %>%
    gt() %>%
    
    ## understand as markdown
    fmt_markdown(columns = vars(Action)) %>%
    
    ## title/header
    tab_header(title = "OHI Onboarding Roadmap") %>%
    opt_align_table_header(align = "left") %>%
    tab_style(cell_text(weight = "bold"), locations = cells_title()) %>%
    ### ^^ figure out later: Warning message:
    # In if ((loc$groups %>% rlang::eval_tidy()) == "title") { :
    #     the condition has length > 1 and only the first element will be used
    
    ## cells_body: make table body white (not striped), and indent
    tab_style(cell_fill("white"), locations = cells_body()) %>%
    tab_style(cell_text(indent = pct(1.5)), locations = cells_body()) %>%
    
    ## cells_column_labels: grey background and bold
    # troubleshooted for hours: jihongzhang.org/post/study-notes-gt-package-and-format-table/
    tab_style(cell_fill("grey97"), 
              locations = cells_column_labels(columns = gt::everything())) %>% 
    tab_style(cell_text(weight = "bold"), 
              locations = cells_column_labels(columns = gt::everything())) %>%
    
    ## Chapter cell_row_groups: make them bold and grey
    tab_style(cell_fill("grey97"), locations = cells_row_groups()) %>%
    tab_style(cell_text(weight = "bold"), locations = cells_row_groups())
  
  return(sched_gt)
  
}


