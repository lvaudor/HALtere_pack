#' Description
#' @param data a tibble produced with the extract_collection() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=+Inf)
#' data_ref_authors=tidy_ref_authors(data)
tidy_ref_authors=function(data,method="shortest"){
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
                                     dplyr::pull(authIdFullName_fs) %>%
                                     unique() %>%
                                     unlist()) %>%
    tidyr::separate(auth, sep="_FacetSep_",into=c("id_internal","name_var"))
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
    dplyr::select(-starts_with("auth"))
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
  return(dat)
}

#' Description
#' @param data_ref_authors a tibble produced with the extract_collection() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("EVS_UMR5600", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' data_labs=tidy_groups(data,type="labs")
tidy_groups=function(data_ref_authors,method="shortest", type="people"){
  if(type=="labs"){data_ref_authors=data_ref_authors %>%
    dplyr::mutate(name=affiliation)}
  dat_groups=data_ref_authors %>%
    dplyr::group_by(name,affiliation, producedDateY_i, docType_s) %>%
    dplyr::summarise(nrefs=dplyr::n_distinct(id_ref),
                     .groups="drop")

   res=data_ref_authors %>%
    tidytext::unnest_tokens(output=word,input=text,token="words")%>%
    dplyr::left_join(lexicon_en,by="word") %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    dplyr::mutate(lemma_completed=dplyr::case_when(is.na(lemma)~word,
                                                   TRUE~lemma))
  spec_unique=mixr::tidy_specificities(res %>% dplyr::filter(!is.na(lemma)),
                                       cat1=name,
                                       cat2=lemma) %>%
    dplyr::arrange(name,desc(spec)) %>%
    dplyr::group_by(name) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data,~.x[1,])) %>%
    tidyr::unnest(cols=c(data)) %>%
    dplyr::ungroup()
  dat_groups=dat_groups %>%
    dplyr::left_join(spec_unique %>%
                       dplyr::select(name,lemma,spec),
                     by="name") %>%
    dplyr::mutate(lemma=dplyr::case_when(is.na(lemma)~name,
                                         TRUE~lemma)) %>%
  return(dat_groups)
}
