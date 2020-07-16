folder_setup <- function(workspace){
  dir.create(paste0(workspace,"/floodR_output"))
  dir.create(paste0(workspace,"/floodR_output", "/DEMs"))
  dir.create(paste0(workspace,"/floodR_output",  "/flooding_rasters"))
  dir.create(paste0(workspace,"/floodR_output",  "/figures"))
  dir.create(paste0(workspace,"/floodR_output",  "/input_shps"))
  dir.create(paste0(workspace, "/floodR_output", "/percent_filled"))
  dir.create(paste0(workspace,"/floodR_output",  "/structures"))
  dir.create(paste0(workspace,"/floodR_output",  "/no_pipes"))
  dir.create(paste0(workspace,"/floodR_output", "/no_pipes/structures"))
  dir.create(paste0(workspace,"/floodR_output", "/no_pipes/flooding_rasters"))
}

