#' Description
    #' @param collection name of a HAL collection
    #' @return a tibble
    #' @export
    #' @examples
    #' publis=extract_collection("BIOEENVIS", nmax=20)
    extract_collection=function(collection, nmax=+Inf){
      #URL de l'API HAL
      url <- "https://api.archives-ouvertes.fr/search/"
      data=tibble::tibble()
      nr=1000
      i=0
      while(nr>=100 & nrow(data)<nmax){
        nrows=min(c(nmax,100),na.rm=TRUE)
        # Paramètres de la requête
        params <- list(
          q = glue::glue("collCode_s:({collection})"),  # Code de la collection
          rows =nrows ,                   # Nombre de résultats à récupérer
          start = i*100,                     # Début de la pagination
          wt = "json",                   # Format de réponse en JSON
          fl = "authIdHasPrimaryStructure_fs,
        authIdFullName_fs,
        producedDateY_i,
        title_s,
        journalTitle_s,
        docType_s,
        keyword_s,
        en_abstract_s,
        language_s")

        # Effectuer la requête GET
        data_tmp<- httr::GET(url, query = params) %>%
          httr::content(as = "text") %>%
          jsonlite::fromJSON() %>%
          .$response %>%
          .$docs
        nr=nrow(data_tmp)
        message(glue::glue("Extracted results {i*100} to {i*100+nr}"))
        data=dplyr::bind_rows(data,data_tmp)
        i=i+1
      }
      data= data %>%
        dplyr::mutate(id_ref=1:dplyr::n())
      dat_title=data %>%
        dplyr::select(id_ref,title_s,language_s) %>%
        dplyr::group_by(id_ref) %>%
        tidyr::nest() %>%
        dplyr::mutate(data=purrr::map(data,
                                      ~dplyr::filter(.x,language_s=="en"))) %>%
        dplyr::mutate(ntitle_en=purrr::map(data,nrow)) %>%
        dplyr::filter(ntitle_en>0) %>%
        dplyr::mutate(title_en=purrr::map_chr(data,~.x$title_s[[1]][1])) %>%
        dplyr::select(-data,-ntitle_en)
      dat=data %>%
        dplyr::mutate(title_s=purrr::map_chr(title_s,~.x[1])) %>%
        dplyr::left_join(dat_title,by=c("id_ref")) %>%
        dplyr::mutate(en_abstract_s=purrr::map_chr(en_abstract_s,
                                                   function(x){if(is.null(x)){return(NA)}else{return(x)}})) %>%
        dplyr::mutate(keywords_s=purrr::map_chr(keyword_s,
                                                ~stringr::str_c(.x,collapse="; "))) %>%
        tidyr::unite("text",keywords_s,en_abstract_s,title_en,remove=FALSE)
      return(dat)
}
