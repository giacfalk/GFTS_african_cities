setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility public transit/GTFS/Harare")

merge_gtfs_feeds <- function(gtfs_list){
  # read all fees separately
  all_feeds <- lapply(gtfs_list, read_gtfs)
  
  create_new_ids <- function(i, id, files){
    values <- function(i, mfile, id)
      all_feeds[[i]][[mfile]][[id]]
    
    ids <- as.vector(unlist(sapply(files, function(mfile) values(i, mfile, id))))
    new_ids <- paste0(i, "_", seq_along(ids))
    
    for(mfile in files){  
      if(!is.null(all_feeds[[i]][[mfile]])){
        positions <- match(values(i, mfile, id), ids)
        all_feeds[[i]][[mfile]][[id]] <- new_ids[positions]
      }
    }
  }
  
  for(i in seq_along(all_feeds)){
    create_new_ids(i, "shape_id",   c("shapes", "trips"))
    create_new_ids(i, "agency_id",  c("agency", "routes"))
    create_new_ids(i, "route_id",   c("routes", "trips"))
    create_new_ids(i, "trip_id",    c("trips", "stop_times"))
    create_new_ids(i, "stop_id",    c("stop_times", "stops"))
    create_new_ids(i, "service_id", c("trips", "calendar"))
    create_new_ids(i, "trip_id",    c("trips", "frequencies", "stop_times"))
  }
  
  # separate 1st feed
  new_gtfs <- list()
  
  # function to extract elements in a series of lists
  extract_list_element <- function(i, element){ all_feeds[[i]][[element]] }
  
  ## piling up
  
  # 1/8 agency
  new_gtfs$agency <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'agency') %>% data.table::rbindlist(fill = TRUE)
  
  # 2/8 routes
  new_gtfs$routes <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'routes') %>% data.table::rbindlist(fill = TRUE)
  
  # 3/8 stops
  new_gtfs$stops <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'stops') %>% data.table::rbindlist(fill = TRUE)
  
  # 4/8 stop_times
  new_gtfs$stop_times <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'stop_times') %>% data.table::rbindlist(fill = TRUE)
  
  # 5/8 shapes
  new_gtfs$shapes <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'shapes') %>% data.table::rbindlist(fill = TRUE)
  
  # 6/8 trips
  new_gtfs$trips <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'trips') %>% data.table::rbindlist(fill = TRUE)
  
  # 7/8 calendar
  new_gtfs$calendar <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'calendar') %>% data.table::rbindlist(fill = TRUE)
  
  # 8/8 frequencies
  new_gtfs$frequencies <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'frequencies') %>% data.table::rbindlist(fill =TRUE)
  
  return(new_gtfs)
}

library(tidyverse)

a <- merge_gtfs_feeds(list("uno.zip", "due.zip"))

gtfs2gps::write_gtfs(a, "Harare.zip")
