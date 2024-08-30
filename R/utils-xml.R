xpath_node_at <- local({
    # NOTE: defined locally to minimize re-creation of the same pattern string
    sprintf_xpath <- paste0(
        # of all nodes
        "(//*[",
            # greater than desired line, or on desired line and past col
            "(number(@line1) < %1$s or (",
                "number(@line1) = %1$s and ", 
                "number(@col1) <= %2$s",
            ")) and ",
            # and less than desired line, or on desired line before col
            "(number(@line2) > %1$s or (",
                "number(@line2) = %1$s and ", 
                "number(@col2) >= %2$s",
            "))",
        "])",
        # take the last
        "[last()]"
    )

    function(line, col) {
        sprintf(sprintf_xpath, line, col)
    }
})

xpath_roxygen_transparent <- paste(
    "/expr[", 
        "child::LEFT_ASSIGN or ",
        "child::RIGHT_ASSIGN or ",
        "child::EQ_ASSIGN or ",
        "child::OP-LEFT-BRACE or ",
        "child::*[1][self::OP-LEFT-PAREN]",
    "]"
)

xml_find_parent_function <- function(node) {
    for (i in seq_along(xml2::xml_parents(node))) {
        res <- xml_find_function(node[[i]])
        if (!is.na(res)) {
            return(res)
        }
    }
}

#' Expands a function node to encompass related expressions
#' 
#' Used primarily for discovering the outermost expressions related to a
#' function definition, which should be used for the insertion of a roxygen
#' header.
#'
#' Traverses parents of `node` to encompase associated syntax - calls and 
#' operators that would not affect the association of roxygen headers. Namely
#' parenthesis, curly-braces and assignment operators.
#' 
#' Given an expression such as
#' 
#'    (x = { 
#'      function(a, b, c) {
#'        a + b + c  
#'      }
#'    }) -> 
#'    y ->
#'    z
#' 
#' The expression outermost assignment to 'z' will be returned, whose expression
#' node encompasses all lines shown. 
#' 
#' @noRd
xml_expand_roxygen_associated <- function(node) {
    
}

xml_find_parent_function <- function(node) {
    node <- xml2::xml_find_first(node, xpath_node_at(line, col))
    xml2::xml_parents(node)
}
