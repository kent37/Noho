# Helpers for Drury Lane project

#' Read and process data for one municipality
#' @param path Path to geodatabase
#' @param lots_layer Name of layer for lot polygons
#' @param assess_layer Name of layer for assessor info
#' @param lut_layer Name of layer for use code lookup
#' @param url_format {glue} format for property card lookup
read_city_data = 
  function(path, lots_layer, assess_layer, lut_layer, url_format) 
{
  lots = read_sf(path, lots_layer) |> 
    st_transform(4326)
  
  assess = read_sf(path, assess_layer)
  
  # Aggregate the properties on a single lot, e.g. condos
  assess_by_locid = assess|> 
    group_by(LOC_ID) |> 
    summarize(TOTAL_VAL=as.numeric(sum(TOTAL_VAL)),
              PROP_ID=list(PROP_ID),
              ADDR_NUM=first(ADDR_NUM),
              FULL_STR=first(FULL_STR),
              USE_CODE=list(USE_CODE),
              Owners = list(OWNER1),
              .groups='drop')
  
  if ('SHAPE' %in% names(lots))
    lots = lots |> 
      rename(Shape=SHAPE, Shape_Area=SHAPE_Area)
  
  # if (any(st_geometry_type(lots)=='MULTISURFACE'))
  #   lots = st_segmentize(lots, dfMaxLength = 1)
  
  sq_meter_per_acre = 4046.86
  lots_with_assess = assess_by_locid |> 
    left_join(lots) |> 
    mutate(Shape = Shape |> st_cast('MULTIPOLYGON'), # mapview needs this...
           Acres = Shape_Area / sq_meter_per_acre,
           Full_addr = str_to_title(if_else(is.na(ADDR_NUM), 
                                            FULL_STR, 
                                            paste(ADDR_NUM, FULL_STR)))) |>
    st_as_sf()
  
  # Popup labels include all PROP_IDs
  format_prop_id = function(ids) {
    str_glue(url_format)
  }

  # Table from the data
  uc_lut = read_sf(path, lut_layer)
  use_code_lookup = uc_lut |> 
    select(USE_CODE, USE_DESC) |> 
    mutate(USE_DESC = str_remove(USE_DESC, '^\\(formerly '),
           USE_DESC = str_remove(USE_DESC, '\\. *Removed June 2009\\. ?\\)')) |> 
    deframe()
  
  # Check for missing codes
  # setdiff(unique(assess$USE_CODE), names(use_code_lookup)) |> sort()
  
  format_prop_id_and_use_code = function(ids, codes, owners) {
    if (length(ids)==1 && length(codes)==1)
      paste(owners, use_code_lookup[[codes]], format_prop_id(ids),
            sep='<br>')
    else {
      paste(owners, 
            lapply(codes, \(code) use_code_lookup[[code]]), 
            format_prop_id(ids),
            sep='<br>', collapse='<br><br>')
    }
  }
  
  lots_with_assess$popups = unclass(str_glue_data(lots_with_assess,
    '{Full_addr}<br>',
    '{round(Acres, 2)} acres<br>',
    '{pmap(list(PROP_ID, USE_CODE, Owners), format_prop_id_and_use_code)}'
  )) |>
    lapply(htmltools::HTML)
  
  format_owners = function(owners) {
    if(length(owners)==1) paste0("<br>", owners) else ""
  }
  
  lots_with_assess$labels = unclass(str_glue_data(lots_with_assess,
    '{Full_addr}{map_chr(Owners, format_owners)}'))|>
    lapply(htmltools::HTML)
  
  return(lots_with_assess)
}