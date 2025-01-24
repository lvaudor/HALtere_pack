#' Description
#' @param data a tibble produced with the tidy_authors() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' cross_by_group(data_ref_authors %>% filter(id_ref==1),var_to_cross="affiliation")
cross_by_group=function(data,var_to_cross="name"){
  var_to_cross=rlang::sym(var_to_cross)
  values=data %>%
    dplyr::pull(!!var_to_cross)
  values_crossed=tidyr::expand_grid(values,values)
  #%>% dplyr::filter(values...2!=values...1)
  return(values_crossed)
}
#' Description
#' @param data_by_ref data with one line=one ref
#' @param data_by_structure data with one line=one structure or person
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' data_authors=tidy_authors(data)
#' HALtere::cross(data_ref_authors,data_authors,var_to_cross="affiliation")
#' HALtere::cross(data_ref_authors,data_authors,var_to_cross="name")
cross=function(data_by_ref, data_by_structure,var_to_cross){
  string_var_to_cross=var_to_cross
  var_to_cross=rlang::sym(var_to_cross)
  crossed_data=data_by_ref %>%
    dplyr::group_by(id_ref,producedDateY_i,docType_s) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data,
                                  .f=~cross_by_group(.x,var_to_cross=string_var_to_cross))) %>%
    tidyr::unnest(cols=c("data")) %>%
    dplyr::mutate(val1=values...1,val2=values...2) %>%
    dplyr::group_by(val1,val2,docType_s,producedDateY_i) %>%
    dplyr::summarise(nlinks=dplyr::n(),.groups="drop") %>%
    dplyr::ungroup()
    if(string_var_to_cross=="name"){
      data_by_structure=data_by_structure %>%
        dplyr::group_by(docType_s,producedDateY_i,affiliation,name) %>%
        dplyr::summarise(nrefs=sum(nrefs),.groups="drop")
    }else{
      data_by_structure=data_by_structure %>%
        dplyr::group_by(docType_s,producedDateY_i,affiliation) %>%
        dplyr::summarise(nrefs=sum(nrefs),.groups="drop") %>%
        dplyr::mutate(name=affiliation)
    }
    crossed_data=crossed_data %>%
      dplyr::left_join(data_by_structure,
                       by=c("val1"="name",
                            "producedDateY_i"="producedDateY_i",
                            "docType_s"="docType_s"))
  return(crossed_data)
}
