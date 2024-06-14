# Helper functions for extracting SLATS data

library(exactextractr)
library(dplyr)


fcn_slats_extract_raster <-  function(slats_raster, shape, col = "RES_NO") {
  slats_np <- exactextractr::exact_extract(slats_raster, shape, 
                                           fun = function(df) {
                                             df %>%
                                               filter(value == 4) %>%
                                               group_by(.data[[col]], value) %>%
                                               summarize(area = sum(coverage_area), .groups = "drop")
                                           },
                                           summarize_df = TRUE,
                                           coverage_area = TRUE,
                                           include_cols = col,
                                           max_cells_in_memory = 1e+07)
  if (length(slats_np) > 0) {
    out <- shape %>%
      dplyr::left_join(slats_np, by = col) %>%
      dplyr::mutate(area = ifelse(is.na(area), 0, area))
    out <- out$area
  } else {
    # Deal with case where no deforestation in any of the protected areas
    out <- rep(0, length(shape))
  }
  return(out)
}
