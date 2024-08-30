CodeActionKind <- list(
    Empty = "",
    QuickFix = "quickfix",
    Refactor = "refactor",
    RefactorExtract = "refactor.extract",
    RefactorInline = "refactor.inline",
    RefactorRewrite = "refactor.rewrite",
    Source = "source",
    SourceOrganizeImports = "source.organizeImports"
)

#' The response to a textDocument/codeAction Request
#'
#' @keywords internal
document_code_action_reply <- function(
    id,
    uri,
    workspace,
    document,
    range,
    context,
    capabilities
) {
    result <- list()
    listed_linters <- character()

    result <- c(
        result,
        code_action_codegrip_reshape(id, uri, workspace, document, range, context, capabilities),
        code_action_insert_roxygen_header(id, uri, workspace, document, range, context, capabilities)
    )

    # lintr diagnostics actions
    for (item in context$diagnostics) {
        item_range <- list(
            start = document$from_lsp_position(item$range$start),
            end = document$from_lsp_position(item$range$end)
        )

        if (item_range$start$row == item_range$end$row &&
            item_range$start$col < item_range$end$col) {
            line <- document$line(item_range$end$row + 1)

            position <- document$to_lsp_position(
                item_range$end$row,
                nchar(line)
            )

            logger$info("document_code_action_reply:", list(
                line = line,
                position = position
            ))

            if (!("*" %in% listed_linters)) {
                if (grepl("#\\s*nolint\\s*:", line)) {
                    # modify existing nolint directives
                    nolint_start <- regexec("#\\s*nolint\\s*:", line)[[1]] - 1
                    edit <- text_edit(range = range(
                        start = position(
                            item_range$end$row,
                            nolint_start
                        ),
                        end = position(
                            item_range$end$row,
                            nchar(line)
                        )
                    ), "# nolint")
                } else {
                    position <- document$to_lsp_position(
                        item_range$end$row,
                        nchar(line)
                    )
                    edit <- text_edit(range = range(
                        start = position,
                        end = position
                    ), " # nolint")
                }
                changes <- list(
                    list(edit)
                )
                names(changes) <- uri
                action <- list(
                    title = "Disable all linters for this line",
                    kind = CodeActionKind$QuickFix,
                    edit = list(
                        changes = changes
                    )
                )

                result <- c(result, list(action))
                listed_linters <- c(listed_linters, "*")
            }

            if (!(item$code %in% listed_linters)) {
                if (grepl("#\\s*nolint\\s*:.+\\.", line)) {
                    # modify existing nolint directives
                    nolint_start <- regexec("#\\s*nolint\\s*:.+\\.", line)[[1]] - 1
                    nolint_end <- nolint_start + attr(nolint_start, "match.length") - 1
                    edit <- text_edit(range = range(
                        start = position(
                            item_range$end$row,
                            nolint_end
                        ),
                        end = position(
                            item_range$end$row,
                            nolint_end
                        )
                    ), sprintf(", %s", item$code))
                } else {
                    position <- document$to_lsp_position(
                        item_range$end$row,
                        nchar(line)
                    )
                    edit <- text_edit(range = range(
                        start = position,
                        end = position
                    ), sprintf(" # nolint: %s.", item$code))
                }
                changes <- list(
                    list(edit)
                )
                names(changes) <- uri
                action <- list(
                    title = sprintf("Disable %s for this line", item$code),
                    kind = CodeActionKind$QuickFix,
                    edit = list(
                        changes = changes
                    )
                )

                result <- c(result, list(action))
                listed_linters <- c(listed_linters, item$code)
            }
        }
    }



    logger$info("document_code_action_reply: ", list(
        uri = uri,
        range = range,
        context = context,
        result = result
    ))

    Response$new(id, result = result)
}

code_action_codegrip_reshape <- function(
    id,
    uri,
    workspace,
    document,
    range,
    context,
    capabilities
) {
    if (!requireNamespace("codegrip", quietly = TRUE)) {
        return(NULL)
    }

    reshape <- tryCatch(
        codegrip:::reshape_info(
            line = range$start$row + 1,
            col = range$start$col + 1,
            info = list(
                file = uri,
                lines = document$content,
                xml = document$parse_data$xml_doc
            )
        ),
        error = function(e) {
            logger$info(
                "R Error encountered while preparing {codegrip} reshape code action: ",
                conditioNMessage(e)
            )
            NULL
        }
    )

    # null if parse error or no replacement is suggested
    if (is.null(reshape)) {
        return()
    }

    start <- document$to_lsp_position(
        row = reshape$start[["line"]] - 1,
        col = reshape$start[["col"]] - 1
    )

    end <- document$to_lsp_position(
        row = reshape$end[["line"]] - 1,
        col = reshape$end[["col"]] - 1
    )

    edit_range <- range(start = start, end = end)
    edit <- if (isTRUE(capabilities$workspace$workspaceEdit$snippetEditSupport)) {
        snippet <- reshape$reshaped
        head_cursor <- snippet_escape(substring(snippet, 1, cursor_pos))
        tail_cursor <- snippet_escape(substring(snippet, cursor_pos))
        snippet <- paste0(head_cursor, "$0", tail_cursor)
        snippet_edit(range = edit_range, snippet)
    } else {
        text_edit(range = edit_range, reshape$reshaped)
    }

    changes <- list(list(edit))
    names(changes) <- uri

    list(list(
        title = "{codegrip} reshape",
        kind = CodeActionKind$Refactor,
        edit = list(changes = changes)
    ))
}

code_action_insert_roxygen_header <- function(
    id,
    uri,
    workspace,
    document,
    range,
    context,
    capabilities
) {
    doc <- document$parse_data$xml_doc

    # find the node that our cursor is on
    line <- range$start$line
    col  <- range$start$character
    node <- xml2::xml_find_first(doc, xpath_node_at(line, col))

    # if we're in an R6 object declaration, check if we're in a location where
    # we should insert a @field


    # if we're in a function definition, proceed with function header
    node_fn <- xml_find_parent_function(node)
    if (!is.na(node_fn)) {
        
    }
    
    # sym_node <- xml_find_associated_symbol(doc, range$start)
    # com_node <- xml_find_associated_comment(sym_node)

    edit <- text_edit(
        range = range(
            document$to_lsp_position(0, 0),
            document$to_lsp_position(0, 0)
        ),
        "test"
    )

    changes <- list(list(edit))
    names(changes) <- uri

    list(list(
        title = "insert roxygen header",
        kind = CodeActionKind$QuickFix,
        edit = list(changes = changes)
    ))
}


