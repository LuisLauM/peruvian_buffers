require(sf)
require(qgisprocess)
require(dplyr)

land <- st_read(dsn = "data/world_wo_islands.gpkg", layer = "land_noislands")
oceans <- st_read(dsn = "data/oceans_10m.gpkg", layer = "ocean_10m")
atlantic_ocean <- st_read(dsn = "data/atlantic_ocean.gpkg", layer = 'atlantic_ocean')

subset_limits <- list(
  x = c(-150, -70),
  y = c(-30, 5)
)

land_subset <- qgis_run_algorithm(
  algorithm = "native:polygonstolines",
  INPUT = land |> select(featurecla)
) |> 
  
  qgis_extract_output() |> 
  
  st_read()

land_subset <- qgis_run_algorithm(
  algorithm = "native:extractbyextent",
  INPUT = land_subset,
  EXTENT = unlist(subset_limits) |> paste(collapse = ","),
  CLIP = TRUE
) |> 
  
  qgis_extract_output() |> 
  
  st_read()


land_subset <- qgis_run_algorithm(
  algorithm = "native:clip",
  INPUT = land_subset,
  OVERLAY = land
) |>

  qgis_extract_output() |>

  st_read()



bufferDistances_nm <- seq(from = 1, to = 300)
# bufferDistances_nm <- c(50, 100, 150, 300)
# allBuffers <- setNames(
#   object = bufferDistances_nm/60,
#   nm = sprintf(fmt = "%03d_mn", bufferDistances_nm)
# ) |> 
#   
#   lapply(
#     FUN = \(input, dist, overlay, ...){
#       sprintf(fmt = "Buffer %.0f mn", dist*60) |> cli::cli_h1()
#       
#       out <- qgis_run_algorithm(
#         algorithm = "native:buffer", 
#         INPUT = input,
#         DISTANCE = dist,
#         ...
#       ) |> 
#         
#         qgis_extract_output() |> 
#         
#         st_read()
#       
#       out <- qgis_run_algorithm(
#         algorithm = "native:polygonstolines", 
#         INPUT = out
#       ) |> 
#         
#         qgis_extract_output() |> 
#         
#         st_read()
#       
#       out <- qgis_run_algorithm(
#         algorithm = "native:difference", 
#         INPUT = out,
#         OVERLAY = atlantic_ocean
#       ) |> 
#         
#         qgis_extract_output() |> 
#         
#         st_read()
#       
#       qgis_run_algorithm(
#         algorithm = "native:clip", 
#         INPUT = out,
#         OVERLAY = overlay
#       ) |> 
#         
#         qgis_extract_output() |> 
#         
#         st_read()
#     },
#     input = land_subset,
#     overlay = oceans |> select(featurecla),
#     DISSOLVE = TRUE,
#     END_CAP_STYLE = "Round",
#     JOIN_STYLE = "Round"
#   ) 

require(foreach)
require(doParallel)

# Registering cluster
cl <- makeCluster(spec = 16)
registerDoParallel(cl = cl)
requiredPkgs <- c('sf', 'qgisprocess', "dplyr")

# Run multithread process
allBuffers <- foreach(
  i = seq_along(bufferDistances_nm), 
  .inorder = FALSE, 
  .packages = requiredPkgs
) %dopar% {
  
  out <- qgis_run_algorithm(
    algorithm = "native:buffer", 
    INPUT = land_subset,
    DISTANCE = bufferDistances_nm[i]/60,
    DISSOLVE = TRUE,
    END_CAP_STYLE = "Round",
    JOIN_STYLE = "Round"
  ) |> 
    
    qgis_extract_output() |> 
    
    st_read()
  
  out <- qgis_run_algorithm(
    algorithm = "native:polygonstolines", 
    INPUT = out
  ) |> 
    
    qgis_extract_output() |> 
    
    st_read()
  
  out <- qgis_run_algorithm(
    algorithm = "native:difference", 
    INPUT = out,
    OVERLAY = atlantic_ocean
  ) |> 
    
    qgis_extract_output() |> 
    
    st_read()
  
  list(
    distance = sprintf(fmt = "%03d_mn", bufferDistances_nm[i]),
    shp = qgis_run_algorithm(
      algorithm = "native:clip", 
      INPUT = out,
      OVERLAY = oceans |> select(featurecla)
    ) |> 
      
      qgis_extract_output() |> 
      
      st_read()
  )
}

# Finish cluster
stopCluster(cl)

allBuffers <- setNames(
  object = lapply(X = allBuffers, FUN = \(x) x$shp),
  nm = sapply(X = allBuffers, FUN = \(x) x$distance)
)

allBuffers <- allBuffers[order(names(allBuffers))]



# land_subset |> plot(reset = FALSE, col = "red", ylim = c(-20, -2))
# allBuffers[[1]] |> plot(reset = FALSE, add = TRUE, col = "blue")
# allBuffers[[4]] |> plot(reset = FALSE, add = TRUE, col = "forestgreen")

saveRDS(object = allBuffers, file = "allBuffers.rds", compress = "xz")
