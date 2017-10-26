
#

ModifyPanel <- function(doc, panels_to_delete, panel_label = 'Panel',
                        panel_end = '(midrule|bottomrule)',
                        alphabet_order = TRUE){

  panel_list <- list()
  panel_lines_start <- grep(panel_label, doc)
  panel_lines_end <- grep(panel_end, doc)
  for(i in 1:length(panel_lines_start)){
    p_start <- panel_lines_start[[i]]
    panel_list[[LETTERS[[i]]]] <-
      p_start: min(panel_lines_end[which(panel_lines_end > p_start)])
  }
  all_panels <- names(panel_list)
  delete_panels <- sapply(panel_list, function(x) grepl(panels_to_delete, doc[[min(x)]]))
  if(sum(delete_panels) == 0) stop('Not Found')
  remaining_panel_list <- panel_list[!delete_panels]
  i <- 0
  for(panel_name in names(remaining_panel_list)){
    i <- i + 1
    panel_line <- min(panel_list[[panel_name]])
    if(alphabet_order){
      doc[[panel_line]] <- sub('Panel [A-Z]', paste0('Panel ', LETTERS[[i]]), doc[[panel_line]])
    }
  }
  updated_doc <- doc[-unlist(panel_list[delete_panels])]

  if(!any(grepl('bottomrule', updated_doc))){
    last_line <- max(grep('\\\\midrule', updated_doc))
    if(last_line > 0){
      updated_doc[[last_line]] <- sub('\\midrule', '\\bottomrule', updated_doc[[last_line]],
                                      fixed = TRUE)
      if(grepl('^[ &\\]*$', updated_doc[[last_line - 1]])){
        updated_doc <- updated_doc[-(last_line - 1)]
      }
    }
  }
  return(updated_doc)
}


DeleteNote <- function(doc){
  tablenotes_line <- grep('\\{tablenotes\\}', doc)
  doc <- doc[-(tablenotes_line[[1]]:tablenotes_line[[2]])]
}

DeleteColumns <- function(doc, cols_to_delete,
                          new_column_format = NULL
                          ){
  doc_split <- lapply(doc, function(x) strsplit(x, '&')[[1]])
  lines_length <- sapply(doc_split, length)
  title_line <- which.max(lines_length)
  ncol <- length(doc_split[[title_line]])
  if(!is.null(cols_to_delete)){
    table_format_line <- grep('\\\\begin\\{tabular\\}', doc)
    table_format_pat <- sprintf('[lc|r]{%d, }', ncol)
    table_format_begin <- gregexpr(table_format_pat, doc[[table_format_line]])
    table_format_line_latter <- substring(doc[[table_format_line]], table_format_begin)
    table_format_index <- gregexpr('[lcr]', table_format_line_latter)[[1]]
    exist_delete_cols <- sapply(cols_to_delete, function(x) grep(x, doc_split[[title_line]]))
    found <- sapply(exist_delete_cols, length) > 0
    if(mean(found) < 1){
      print(exist_delete_cols[!found])
      stop('Columns cannot be found')
    } else {
      exist_delete_cols <- unlist(exist_delete_cols)
    }
    # update table format
    updated_table_format_line_latter <- strsplit(table_format_line_latter, '')[[1]][-table_format_index[exist_delete_cols]]
    updated_table_format_line_latter <- paste(updated_table_format_line_latter, collapse = '')
    updated_table_format_line <- paste0(
      substring(doc[[table_format_line]], 1, as.numeric(table_format_begin) - 1),
      updated_table_format_line_latter
    )
    updated_table_format_line <- gsub('\\|{2,}', '|', updated_table_format_line)
    doc[[table_format_line]] <- updated_table_format_line
    for(cur_line in which(lines_length == ncol)){
      revised_line <- paste(doc_split[[cur_line]][-exist_delete_cols], collapse = '&')
      if(max(exist_delete_cols) == length(doc_split[[cur_line]])){
        if(grepl('\\\\ *$', doc_split[[cur_line]][[max(exist_delete_cols)]])){
          revised_line <- paste0(revised_line, '\\\\')
        }
      }
      doc[[cur_line]] <- revised_line
    }
  }
  return(doc)
}

DeleteRows <- function(doc,
                       rows_to_delete,
                       n_multi_row = 1){
  if(!is.null(rows_to_delete)){
    exist_delete_rows <- sapply(
      rows_to_delete, function(x) grep(x, doc
      ))
    found <- sapply(exist_delete_rows, length) > 0
    if(mean(found) < 1){
      print(exist_delete_rows[!found])
      stop('Rows cannot be found')
    } else {
      exist_delete_rows <- unlist(exist_delete_rows)
    }
    doc <- doc[-exist_delete_rows]
  }
  return(doc)
}

ReplaceKeyWord <- function(doc, word_list){
  for(cur_word in word_list){
    doc <- sapply(doc, function(x) gsub(cur_word$pat, cur_word$replace, x))
  }
  return(doc)
}

tables_to_modify <- list(
  table_1 = list(
    name = '../Tables/Product-sample-descriptive-statistics.tex',
    func = list(
      function(x) ModifyPanel(x, 'Panel C'),
      function(x) DeleteNote(x)
    )
  ),
  table_2 = list(
    name = '../Tables/Chain-store-descriptive-statistics.tex',
    func = list(
      function(x) ModifyPanel(x, 'Panel A', panel_end = '(midrule|bottomrule|cline)')
    )
  ),
  table_5 = list(
    name = '../Tables/Price-Decomposition-Main.tex',
    func = list(
      function(x) ModifyPanel(x, 'Basic decomposition', 'Basic decomposition',
                              panel_end = '^ *& *&',
                              alphabet_order = FALSE)
    )
  ),
  table_6 = list(
    name = '../Tables/Dispersion-Regression-Revenue.tex',
    func = list(
      function(x) DeleteColumns(x, c('(5)', '(6)')),
      function(x) DeleteRows(
        x, c(
             'Product group dummy', 'Product module dummy',
             'Observations', '\\(0.00[0-9]*\\)')),
      function(x) ReplaceKeyWord(x, list(
        # list(pat = 'Dependent variable: ', replace = ''),
        list(pat = 'multicolumn\\{6\\}', replace = 'multicolumn{4}'),
        # list(pat = '\\^\\{\\*{1, }\\}', replace = ''),
        list(pat = 'cline\\{2-7\\}', replace = 'cline{2-5}')
      ))
    )
  ),
  table_7 = list(
    name = '../Tables/Dispersion-Regression-Penetration.tex',
    func = list(
      function(x) DeleteColumns(x, c('(5)', '(6)')),
      function(x) DeleteRows(
        x, c(
             'Product group dummy', 'Product module dummy',
             'Observations', '\\(0.00[0-9]*\\)')),
      function(x) ReplaceKeyWord(
        x, list(
        # list(pat = 'Dependent variable: ', replace = ''),
        list(pat = 'cline\\{2-7\\}', replace = 'cline{2-5}'),
        # list(pat = '\\^\\{\\*{1, }\\}', replace = ''),
        list(pat = 'multicolumn\\{6\\}', replace = 'multicolumn{4}')
      ))
    )
  ),
  table_9 = list(
    name = '../Tables/Promotion-Dependence-Regressions.tex',
    func = list(
      function(x) ModifyPanel(x, 'National', 'National', panel_end = 'Intercept & 0.077'),
      function(x) DeleteColumns(x, c('0.1', '0.25', '0.75', '0.9\\}')),
      function(x) ReplaceKeyWord(
        x, list(
          list(pat = 'multicolumn\\{8\\}', replace = 'multicolumn{4}'),
          list(pat = '16\\}', replace = '12}')
        )
      )
    )
  )
)

PrintStandAloneTable <- function(doc, output_file){
  doc_start <-
'\\documentclass[tightpage]{standalone}
\\usepackage{varwidth}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{threeparttable}
\\usepackage{array}
\\usepackage{preview}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{preview}
\\begin{varwidth}{\\linewidth}
%REPLACE
\\end{varwidth}
\\end{preview}
\\end{document}'

doc_end <- '\\end{varwidth}
\\end{preview}
\\end{document}'

  doc <- paste(doc, collapse = '\n')
  doc_all <- sub('%REPLACE', doc, doc_start, fixed = TRUE)
  tmp_file = 'tmp_latex.tex'
  writeLines(doc_all, tmp_file)
  compile_command <- sprintf('pdflatex -interaction nonstopmode -halt-on-error -file-line-error %s', tmp_file)
  system(compile_command)
  file.rename(sub('tex$', 'pdf', tmp_file), output_file)
  file.remove(grep('tmp_latex\\.', dir(), value = TRUE))
}

#
# library(CommonFunctions)
#
# setwd('~/Dropbox/RA/Price-Promotion-Analysis/Documents-Paper/Tables_For_Slides')
# # MakeDir('Tables_For_Slides')
#
#
# for(info_name in names(tables_to_modify)){
#   cur_info <- tables_to_modify[[info_name]]
#   doc <- readLines(cur_info$name)
#   for(cur_func in cur_info$func){
#     doc <- cur_func(doc)
#   }
#   tex_out_path <- sprintf('%s.tex', info_name)
#   pdf_out_path <- sub('tex$', 'pdf', tex_out_path)
#   writeLines(doc, tex_out_path)
#   PrintStandAloneTable(doc, pdf_out_path)
# }

