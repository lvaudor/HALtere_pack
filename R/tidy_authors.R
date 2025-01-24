#' Description
#' @param data a tibble produced with the extract_collection() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' tidy_ref_authors(data)
tidy_ref_authors=function(data,method="longest"){
  dat=data %>%
    tidyr::unnest(cols=c("authIdHasPrimaryStructure_fs")) %>%
    dplyr::mutate(auth_and_aff=authIdHasPrimaryStructure_fs) %>%
    tidyr::separate(auth_and_aff,sep="_JoinSep_", into=c("auth","affiliation")) %>%
    tidyr::separate(auth, sep="_FacetSep_",into=c("id_auth","auth")) %>%
    tidyr::separate(id_auth, sep="-",into=c("id_HAL","id_internal")) %>%
    tidyr::separate(affiliation,sep="_FacetSep_",into=c("id_affiation","affiliation"))
  if(method=="longest"){foptimum=max}
  if(method=="shortest"){foptimum=min}
  tib_authors_names=tibble::tibble(auth=data %>%
                                     dplyr::pull(authAlphaLastNameFirstNameId_fs) %>%
                                     unique() %>%
                                     unlist()) %>%
    tidyr::separate(auth, sep="AlphaSep_",into=c("dump","name_var")) %>%
    tidyr::separate(name_var,sep="_FacetSep_", into=c("name_var","id_internal"))
  tib_authors_names_with_internal_id=tib_authors_names %>%
    dplyr::filter(id_internal!=0) %>%
    dplyr::arrange(id_internal) %>%
    unique() %>%
    dplyr::mutate(name_length=stringr::str_length(name_var)) %>%
    dplyr::group_by(id_internal) %>%
    dplyr::mutate(n=dplyr::n(),
                  name_length_optimum=foptimum(name_length)) %>%
    dplyr::mutate(name=name_var[name_length==name_length_optimum][1]) %>%
    dplyr::mutate(name=dplyr::case_when(is.na(name)~name_var,
                          TRUE~name)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_internal,name,name_var) %>%
    dplyr::group_by(id_internal) %>%
    dplyr::top_n(n=1)

  dat=dat %>%
    dplyr::left_join(tib_authors_names_with_internal_id %>%
                dplyr::select(id_internal,name) %>%
                unique(),by=c("id_internal")) %>%
    dplyr::mutate(name=dplyr::case_when(is.na(name)~auth,
                          TRUE~name)) %>%
    dplyr::select(title_s,journalTitle_s,docType_s,producedDateY_i,id_ref,affiliation,name)
  # For each year and name keep main affiliation
  dat_aff=dat  %>%
    dplyr::select(producedDateY_i,affiliation,name) %>%
    unique() %>%
    dplyr::group_by(producedDateY_i,name) %>%
    dplyr::mutate(n=dplyr::n()) %>%
    dplyr::arrange(n=desc(n)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(-n)%>%
    dplyr::ungroup()
  # In case there is a year with no affiliation, keep major affiliation through all times
  dat_aff_maj=dat_aff %>%
    dplyr::select(affiliation,name) %>%
    unique() %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(n=dplyr::n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(-n) %>%
    dplyr::rename(affmaj=affiliation) %>%
    dplyr::ungroup()
  dat=dat %>%
    dplyr::select(-affiliation) %>%
    unique() %>%
    dplyr::left_join(dat_aff,by=c("producedDateY_i","name")) %>%
    dplyr::left_join(dat_aff_maj,by=c("name")) %>%
    dplyr::mutate(affiliation=dplyr::case_when(is.na(affiliation)~affmaj,
                                               TRUE~affiliation)) %>%
    dplyr::select(-affmaj)
  # dat_nrefs=dat %>%
  #   dplyr::group_by(name,docType_s,producedDateY_i) %>%
  #   dplyr::summarise(nrefs=dplyr::n(),.groups="drop")
  # dat=dat %>%
  #   dplyr::left_join(dat_nrefs,by=c("name","docType_s","producedDateY_i"))
  return(dat)
}

#' Description
#' @param data a tibble produced with the extract_collection() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' tidy_authors(data)
tidy_authors=function(data,method="longest"){
  dat=tidy_ref_authors(data,method=method)
  dat=dat %>%
    dplyr::group_by(name,affiliation, producedDateY_i, docType_s) %>%
    dplyr::summarise(nrefs=dplyr::n(),
                     .groups="drop")
  return(dat)
}
