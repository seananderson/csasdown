#' Post process appendix subsections in LaTeX code
#'
#' @description
#' A post-processor to modify the appendix subsection LaTeX code
#' after the document has been knit. The modifications ensure correct
#' referencing of the appendices throughout the document
#'
#' @keywords internal
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
add_appendix_subsection_refs <- function(x){

  # Need a new counter for each appendix
  star_chap_inds <- grep("^\\\\starredchapter\\{", x)
  # If there are Appendices (Resdoc and SR only, the techreport has a totally different TEX structure)
  if(length(star_chap_inds)){
    counters <- paste0("app_counter_", seq_along(star_chap_inds))
    pre_starred_x <- x[1:(star_chap_inds[1] - 3)]
    appendix_chunks <- list()
    for(i in seq_along(star_chap_inds)){
      if(i == length(star_chap_inds)){
        appendix_chunks[[i]] <- x[(star_chap_inds[i] - 2):length(x)]
      }else{
        appendix_chunks[[i]] <- x[(star_chap_inds[i] - 2):(star_chap_inds[i + 1] - 3)]
      }
    }
    # At this point the TEX file is broken into several chunks, `pre_starred_x` which
    # is everything before the appendices, and N chunks in the list `appendix_chunks`,
    # one element for each appendix

    # Apply mods to the appendix chunks
    for(h in seq_along(appendix_chunks)){
      appsection_inds <- grep("^\\\\appsection\\{", appendix_chunks[[h]])
      if(length(appsection_inds)){
        # Strip appendix header away and call function on the rest
        app_chunk <- appendix_chunks[[h]]
        app_header <- app_chunk[1:(appsection_inds[1] - 2)]
        app_chunk <- app_chunk[(appsection_inds[1] - 1):length(app_chunk)]
        app_chunk_inds <- grep("^\\\\appsection\\{", app_chunk)
        # Now, break each into section chunks
        sec_chunks <- list()
        sec_header <- list()
        for(i in seq_along(app_chunk_inds)){
          if(i == length(app_chunk_inds)){
            sec_chunks[[i]] <- app_chunk[(app_chunk_inds[i] - 1):length(app_chunk)]
          }else{
            sec_chunks[[i]] <- app_chunk[(app_chunk_inds[i] - 1):(app_chunk_inds[i + 1] - 2)]
          }
          # Check for a label and allow missing label
          if(!length(grep("^\\\\hypertarget\\{", sec_chunks[[i]][1]))){
            # An auto-generated label was not added (using manually-added label) so switching the
            # label and appsection is necessary
            tmp_label <- sec_chunks[[i]][1]
            tmp_section <- sec_chunks[[i]][2]
            sec_chunks[[i]][1] <- tmp_section
            sec_chunks[[i]][2] <- tmp_label
          }

          # Iterate through each section chunk and create a list for the subsection chunks
          subsection_inds <- grep("^\\\\subsection\\{", sec_chunks[[i]])
          if(length(subsection_inds)){
            sec_chunk <- sec_chunks[[i]]
            sec_header[[i]] <- sec_chunk[1:(subsection_inds[1] - 2)]
            sec_chunk <- sec_chunk[(subsection_inds[1] - 1):length(sec_chunk)]
            sec_chunk_inds <- grep("^\\\\subsection\\{", sec_chunk)
            subsec_chunks <- list()
            subsec_header <- list()
            for(j in seq_along(sec_chunk_inds)){
              if(j == length(sec_chunk_inds)){
                subsec_chunks[[j]] <- sec_chunk[(sec_chunk_inds[j] - 1):length(sec_chunk)]
              }else{
                subsec_chunks[[j]] <- sec_chunk[(sec_chunk_inds[j] - 1):(sec_chunk_inds[j + 1] - 2)]
              }
              if(!length(grep("^\\\\hypertarget\\{", subsec_chunks[[j]][1]))){
                # An auto-generated label was not added (using manually-added label) so switching the
                # label and subsection is necessary
                tmp_label <- subsec_chunks[[j]][1]
                tmp_subsection <- subsec_chunks[[j]][2]
                subsec_chunks[[j]][1] <- tmp_subsection
                subsec_chunks[[j]][2] <- tmp_label
              }
              # Iterate through each section chunk and create a list for the subsection chunks
              subsubsection_inds <- grep("^\\\\subsubsection\\{", subsec_chunks[[j]])
              if(length(subsubsection_inds)){
                subsec_chunk <- subsec_chunks[[j]]
                subsec_header[[j]] <- subsec_chunk[1:(subsubsection_inds[1] - 2)]
                subsec_chunk <- subsec_chunk[(subsubsection_inds[1] - 1):length(subsec_chunk)]
                subsec_chunk_inds <- grep("^\\\\subsubsection\\{", subsec_chunk)
                subsubsec_chunks <- list()
                for(k in seq_along(subsec_chunk_inds)){
                  if(k == length(subsec_chunk_inds)){
                    subsubsec_chunks[[k]] <- subsec_chunk[(subsec_chunk_inds[k] - 1):length(subsec_chunk)]
                  }else{
                    subsubsec_chunks[[k]] <- subsec_chunk[(subsec_chunk_inds[k] - 1):(subsec_chunk_inds[k + 1] - 2)]
                  }
                  if(!length(grep("^\\\\hypertarget\\{", subsubsec_chunks[[k]][1]))){
                    # An auto-generated label was not added (using manually-added label) so switching the
                    # label and subsection is necessary
                    tmp_sublabel <- subsubsec_chunks[[k]][1]
                    tmp_subsubsection <- subsubsec_chunks[[k]][2]
                    subsubsec_chunks[[k]][1] <- tmp_subsubsection
                    subsubsec_chunks[[k]][2] <- tmp_sublabel
                  }
                }
                subsubsec_chunks <- unlist(subsubsec_chunks)
                counter_lines <- c(paste0("\\newcounter{appendix_",
                                          h,
                                          "_appsection_",
                                          i,
                                          "_subsection_",
                                          j,
                                          "_counter}"),
                                   paste0("\\refstepcounter{appendix_",
                                          h,
                                          "_appsection_",
                                          i,
                                          "_subsection_",
                                          j,
                                          "_counter}"))
                subsubsec_chunks <- c(counter_lines, subsubsec_chunks)
                names(subsubsec_chunks) <- NULL
                subsec_chunks[[j]] <- c(subsec_header[[j]], subsubsec_chunks)
              }
            }
            subsec_chunks <- unlist(subsec_chunks)
            counter_lines <- c(paste0("\\newcounter{appendix_",
                                      h,
                                      "_appsection_",
                                      i,
                                      "_counter}"),
                               paste0("\\refstepcounter{appendix_",
                                      h,
                                      "_appsection_",
                                      i,
                                      "_counter}"))
            subsec_chunks <- c(counter_lines, subsec_chunks)
            names(subsec_chunks) <- NULL
            sec_chunks[[i]] <- c(sec_header[[i]], subsec_chunks)
          }
        }
        sec_chunks <- unlist(sec_chunks)
        sec_header <- NULL
        counter_lines <- c(paste0("\\newcounter{appendix_",
                                  h,
                                  "_counter}"),
                           paste0("\\refstepcounter{appendix_",
                                  h,
                                  "_counter}"))
        sec_chunks <- c(counter_lines, sec_chunks)
        names(sec_chunks) <- NULL
        appendix_chunks[[h]] <- c(app_header, sec_chunks)
      }
    }
    appendix_chunks <- unlist(appendix_chunks)
    names(appendix_chunks) <- NULL
    x <- c(pre_starred_x, appendix_chunks)

  }
  x
}
