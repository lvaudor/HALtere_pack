#' Description
#' @param data a tibble produced with the extract_collection() function
#' @param column the name of the column to be tokenized
#' @return a tibble
#' @export
#' @examples
#' data_BIOEENVIS=extract_collection("BIOEENVIS", nmax=+Inf)
#' data_text=tidy_text(data_BIOEENVIS,"text")
tidy_text=function(data,column){
  string_column=column
  column=rlang::sym(column)
  res=data %>%
    dplyr::filter(!is.na(title_en)) %>%
    tidytext::unnest_tokens(input=!!column,output=word, token="words")
  data(lexicon_en)
  res=res %>%
    dplyr::left_join(lexicon_en,by="word") %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    dplyr::select(id_ref,producedDateY_i,word,lemma,type) %>%
    dplyr::group_by(id_ref) %>%
    dplyr::mutate(n=purrr::map_int(lemma,~length(is.na))) %>%
    ungroup()
  return(res)
}
