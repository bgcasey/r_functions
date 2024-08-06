#' Calculate Focal Statistics on a Raster
#'
#' This function calculates focal statistics on a given raster using a 
#' specified window size and function.
#'
#' @param raster_input RasterLayer. The input raster on which to perform 
#' focal statistics.
#' @param window_size_meters Numeric. The desired window size in meters.
#' @param fun Function or character. The function to apply (e.g., 
#' "mean", "modal", "sd").
#' @param n_cores Integer. The number of cores to use for parallel 
#' processing.
#' @return RasterLayer. A raster with the focal statistics calculated 
#' and the band names updated accordingly.
#' 
#' @example 
#' # Example usage of the function
#' 
#' ibrary(raster)
#' library(parallel)
#' 
#' # Create a sample raster
#' r <- raster(nrows=10, ncols=10)
#' r[] <- runif(ncell(r), 0, 100)
#' 
#' # Apply the function with mean
#' result_mean <- calculate_focal_stat(r, 500, mean, n_cores = 4)
#' print(names(result_mean))
calculate_focal_stat <- function(raster_input, window_size_meters, 
                                 fun, n_cores = 1) {
  # Step 1: Get the cell size (assuming x and y resolution are the same)
  cell_size <- res(raster_input)[1]  
  
  # Step 2: Calculate window size
  window_size <- ceiling(window_size_meters / cell_size)
  
  # Step 3: Store the original raster band names
  original_names <- names(raster_input)
  
  # Function to process a chunk of the raster
  process_chunk <- function(chunk) {
    focal(chunk, w = matrix(1, nrow = window_size, 
                            ncol = window_size), fun = fun, 
          na.rm = TRUE)
  }
  
  # Split the raster into chunks
  split_raster <- function(raster, n_chunks) {
    chunks <- list()
    n_rows <- nrow(raster)
    chunk_size <- ceiling(n_rows / n_chunks)
    
    for (i in 1:n_chunks) {
      start_row <- (i - 1) * chunk_size + 1
      end_row <- min(i * chunk_size, n_rows)
      chunks[[i]] <- crop(raster, extent(raster, start_row, 
                                         end_row, 1, ncol(raster)))
    }
    
    return(chunks)
  }
  
  # Number of chunks
  n_chunks <- n_cores
  
  # Split the raster into chunks
  raster_chunks <- split_raster(raster_input, n_chunks)
  
  # Run the focal stat calculation in parallel
  results <- mclapply(raster_chunks, process_chunk, 
                      mc.cores = n_cores)
  
  # Merge the results back together
  focal_result <- do.call(merge, results)
  
  # Step 5: Determine the function name for band naming
  fun_name <- ifelse(fun == "modal", "mode", fun)
  
  # Step 6: Create new names for the resulting raster bands
  new_names <- paste0(original_names, "_", fun_name, "_", 
                      window_size_meters)
  names(focal_result) <- new_names
  
  return(focal_result)
}