#' Description
#' @param data a tibble produced with the tidy_ref_authors() function and containing lines only for one reference
#' @return a tibble
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=+Inf)
#' data_ref_authors=tidy_ref_authors(data)
#' data_groups=tidy_groups(data_ref_authors)
#' cross_by_group(data_ref_authors %>% dplyr::filter(id_ref==1))
cross_by_group=function(data_ref_authors_oneref, var_groups="name"){
  values=unique(data_ref_authors_oneref[[var_groups]])
  values_crossed=tidyr::expand_grid(values,values)  %>%
    dplyr::mutate(val1=values...1,val2=values...2)
  return(values_crossed)
}
#' Description
#' @param data_groups data with one line=one group (defined as people or labs)
#' @param data_ref_authors data with one line=one ref & one author
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' data_groups=tidy_groups(data_ref_authors, type="people")
#' HALtere::cross(data_groups, data_ref_authors)
#' data_groups=tidy_groups(data_ref_authors,type="labs")
#' HALtere::cross(data_groups,data_ref_authors)
cross=function(data_groups,data_ref_authors){
  if(all(data_groups$name==data_groups$affiliation)){var_groups="affiliation"}else{var_groups="name"}
  crossed_data=data_ref_authors%>%
    dplyr::group_by(id_ref,producedDateY_i,docType_s) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data,
                                  .f=~HALtere:::cross_by_group(.x,var_groups))) %>%
    tidyr::unnest(cols=c("data")) %>%
    dplyr::group_by(val1,val2,docType_s,producedDateY_i) %>%
    dplyr::summarise(nlinks=dplyr::n(),.groups="drop") %>%
    dplyr::ungroup()
    data_groups=data_groups %>%
        dplyr::group_by(docType_s,producedDateY_i,affiliation,name,name_simplified) %>%
        dplyr::summarise(nrefs=sum(nrefs),.groups="drop")
    crossed_data=crossed_data %>%
      dplyr::left_join(data_groups,
                       by=c("val1"="name",
                            "producedDateY_i"="producedDateY_i",
                            "docType_s"="docType_s"))
  return(crossed_data)
}
