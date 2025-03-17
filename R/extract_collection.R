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
        journalTitle_s,
        docType_s,
        fr_keyword_s,
        en_keyword_s,
        en_title_s,
        fr_title_s,
        en_abstract_s,
        fr_abstract_s,
        language_s,
        halId_s,
        modifiedDate_tdate")

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
      #remove repeated publications
      data=unique(data)
      replace_null_with_na=function(list_with_nulls){
        result=list_with_nulls %>%
          purrr::map(~ifelse(is.null(.x[[1]]),NA,.x[[1]])) %>%
          unlist()
        return(result)
      }
      complete_with_translated_texts=function(text_to_complete,text_to_translate,whether_to_translate){
        resulting_text=text_to_complete
        ind=which(whether_to_translate==TRUE & !is.na(text_to_translate))
        for (i in 1:length(ind)){
        if(floor(i/100)==i/100){print(i)}
          text=text_to_translate[ind[i]]
          split_text <- function(text,
                                 maxpart=5000) {
            length_text=stringr::str_length(text)
            nparts=ceiling(length_text/maxpart)
            sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
            n_sentences <- length(sentences)
            group_size <- ceiling(n_sentences / nparts)
            splits <- split(sentences, ceiling(seq_along(sentences) / group_size))
            parts <- sapply(splits, paste, collapse = " ")
            return(parts)
          }
          text=split_text(text)
          resulting_text[ind[i]]=purrr::map(text,
                                       purrr::safely(polyglotr::google_translate),
                                       source_language="fr",
                                       target_language="en") %>%
                                 purrr::map("result") %>%
                                 paste0(collapse=" ") %>%
                                 replace_null_with_na() %>%
                                 unlist()

        }
        return(resulting_text)
      }

      dat= data %>%
        dplyr::mutate(id_ref=1:dplyr::n()) %>%
        dplyr::mutate(dplyr::across(c(en_title_s,fr_title_s,
                                      en_abstract_s,fr_abstract_s,
                                      en_keyword_s,fr_keyword_s),replace_null_with_na)
                      ) %>%
        dplyr::mutate(translate_title_s=dplyr::case_when(is.na(en_title_s)~TRUE,
                                                         TRUE~FALSE),
                      translate_keyword_s=dplyr::case_when(is.na(en_keyword_s)~TRUE,
                                                          TRUE~FALSE),
                      translate_abstract_s=dplyr::case_when(is.na(en_abstract_s)~TRUE,
                                                            TRUE~FALSE)) %>%
        dplyr::mutate(title_s=en_title_s,
                      keyword_s=en_keyword_s,
                      abstract_s=en_abstract_s) %>%
        dplyr::mutate(title_s=complete_with_translated_texts(en_title_s,fr_title_s,translate_title_s)) %>%
        dplyr::mutate(keyword_s=complete_with_translated_texts(en_keyword_s,fr_keyword_s,translate_keyword_s)) %>%
        dplyr::mutate(abstract_s=complete_with_translated_texts(en_abstract_s,fr_abstract_s,translate_abstract_s)) %>%
        tidyr::unite("text",keyword_s,abstract_s,title_s,remove=FALSE)
      return(dat)
}
