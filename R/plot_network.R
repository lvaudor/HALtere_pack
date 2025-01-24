#' Description
#' @param data_crossed data_crossed (by author or affiliation)
#' @param data_by_structure data_authors
#' @param number_of_nodes the number of nodes
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' data_authors=tidy_authors(data)
#' data_crossed=cross(data_ref_authors,data_authors,var_to_cross="name")
#' graph=build_network(data_crossed,data_by_structure=data_authors,number_of_nodes=100)
build_network=function(data_crossed,
                       data_by_structure,
                       number_of_nodes=60
                       ){
  data_summary=data_by_structure %>%
    dplyr::group_by(name,affiliation) %>%
    dplyr::summarise(nrefs=sum(nrefs),.groups="drop")
  data=data_crossed %>%
    dplyr::group_by(val1,val2) %>%
    dplyr::summarise(nlinks=sum(nlinks),
            .groups="drop")
  # nrefs min to be included as node
  keep_nodes=data_summary %>%
    dplyr::arrange(desc(nrefs)) %>%
    unique() %>%
    dplyr::pull(name) %>%
    .[1:number_of_nodes]
  # filter data to keep only nodes with enough references
  data_filtered=data %>%
    dplyr::filter(val1 %in% keep_nodes,
                  val2 %in% keep_nodes)
  g=tidygraph::as_tbl_graph(data_filtered)
  layout <- ggraph::create_layout(g, layout = "fr")
  nodes <- tibble::as_tibble(layout)
  edges <- tibble::as_tibble(g, "edges")
  edges <- tibble::tibble(
    from=edges$from,
    to=edges$to,
    xfrom=nodes$x[edges$from],
    xto=nodes$x[edges$to],
    yfrom=nodes$y[edges$from],
    yto=nodes$y[edges$to],
    namefrom=nodes$name[edges$from],
    nameto=nodes$name[edges$to],
    nlinks=edges$nlinks) %>%
    dplyr::left_join(data_summary,by=c("namefrom"="name")) %>%
    dplyr::mutate(hover=glue::glue("{namefrom}-\n{nameto}:\n{nlinks} links")) %>%
    dplyr::mutate(width=round(dplyr::percent_rank(nlinks)*10+0.5)) %>%
    dplyr::mutate(line_color= "rgb(200,200,200)")

  nodes=nodes %>%
    dplyr::mutate(betweenness=igraph::betweenness(g))
  nodes= nodes %>%
    dplyr::left_join(data_summary,
                     by=c("name"))
  return(list(nodes=nodes,edges=edges))
}


#' Description
#' @param data a tibble produced with the cross() function
#' @param number_of_nodes the number of nodes to display
#' @param number_of_names the number of names to display,
#' @param margin the number of standard deviations to keep nodes
#' @param colorvar the variable to take into account to define node color
#' @param sizevar the variable to take into account to define node size
#' @param colors the colors to interpolate to define palette
#' @return a tibble
#' @export
#' @examples
#' data=extract_collection("BIOEENVIS", nmax=200)
#' data_authors=tidy_authors(data)
#' data_crossed=HALtere::cross(data_authors,var_grouping="id_ref",var_to_cross="name")
#' graph=build_network(data_crossed,data_authors,number_of_nodes=50)
#' plot_network(graph, colorvar="name")
plot_network=function(graph,
                      number_of_names=30,
                      margin=5,
                      colorvar="name",
                      sizevar="nrefs",
                      colors=c("red", "yellow", "blue")){
  nodes=graph$nodes
  edges=graph$edges
  colorvar=rlang::sym(colorvar)
  sizevar=rlang::sym(sizevar)
  palROB <- grDevices::colorRampPalette(colors)
  keep_names=nodes %>%
    dplyr::arrange(desc(betweenness)) %>%
    dplyr::pull(name) %>%
    unique() %>%
    .[1:number_of_names]
  fmatch=function(vec){match(vec, sort(unique(vec)))}
  nodes=nodes %>%
    dplyr::mutate(name_to_display=dplyr::case_when(name %in% keep_names ~name,
                                                   TRUE~"")) %>%
    dplyr::mutate(marker_color=fmatch(!!colorvar),
                  size=dplyr::percent_rank(!!sizevar)*10)
  if("affiliation" %in% colnames(nodes)){
    nodes=nodes %>%
      dplyr::mutate(hover=glue::glue("Name: {name} <br>
                                     Affiliation: {affiliation}
                                     <br> Number of references: {nrefs}"))
  }else{
    nodes=nodes %>%
      dplyr::mutate(hover=glue::glue("Name: {name} <br> Number of references: {nrefs}"))
  }
  ncols=max(nodes$marker_color)
  cols=palROB(ncols)
  # remove nodes that are too marginal
  nodes_to_display=nodes %>%
    dplyr::mutate(marker_color=cols[nodes$marker_color])
  #%>%
    # dplyr::mutate(xsup=mean(x)+margin*sd(x),
    #        xinf=mean(x)-margin*sd(x),
    #        ysup=mean(y)+margin*sd(y),
    #        yinf=mean(y)-margin*sd(y)) %>%
    # dplyr::filter(x>xinf & x<xsup & y>yinf & y<ysup) %>%
    # dplyr::select(-xsup,-xinf,-ysup,-yinf)


  #nmin=quantile(edges$n,1-prop_edges)
  #edges=filter(edges,n>=nmin)
  # Créer le graphique interactif avec plotly
  fig <- plotly::plot_ly() %>%
    # Ajouter les arêtes
    plotly::add_segments(
      data=edges,
      x = ~xfrom,
      y = ~yfrom,
      xend = ~xto,
      yend = ~yto,
      line = list(
        color = ~line_color,  # Utiliser la couleur dynamique
        width = ~width        # Largeur dynamique
      )
    ) %>%
    # Ajouter les nœuds avec info en hover
    plotly::add_trace(
      x = ~nodes_to_display$x,
      y = ~nodes_to_display$y,
      type = 'scatter',
      mode = 'markers+text',
      text= ~nodes_to_display$name_to_display,
      hovertext= ~nodes_to_display$hover,
      marker = list(
        size = ~nodes_to_display$size,
        color = ~nodes_to_display$marker_color,
        line = list(width = 0)  # Pas de bordure
      ),
      textfont = list(
        color = 'black',
        size = 15
      ),
      textposition = 'center',
      showlegend=FALSE
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "",         # Pas de titre pour l'axe x
        showticklabels = FALSE,  # Masquer les étiquettes de l'axe x
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",         # Pas de titre pour l'axe x
        showticklabels = FALSE,  # Masquer les étiquettes de l'axe x
        showgrid = FALSE,
        zeroline = FALSE
      )
    )
  return(fig)
}
