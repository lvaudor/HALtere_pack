#' Description
#' @param data a tibble produced with the extract_collection() function
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=20)
#' data_text=tidy_text(data,"title_en")
tidy_text=function(data,column){
  string_column=column
  column=rlang::sym(column)
  data=data %>%
    dplyr::filter(!is.na(title_en)) %>%
    tidytext::unnest_tokens(input=!!column,output=word, token="words")
  data(lexicon_en)
  data=data %>%
    dplyr::left_join(lexicon_en,by="word") %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    dplyr::select(id_ref,producedDateY_i,word,lemma,type) %>%
    dplyr::group_by(id_ref) %>%
    dplyr::mutate(n=purrr::map_int(lemma,~length(is.na)))

  return(data)
}
