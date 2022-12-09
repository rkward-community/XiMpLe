# Copyright 2011-2022 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package XiMpLe.
#
# XiMpLe is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# XiMpLe is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with XiMpLe.  If not, see <http://www.gnu.org/licenses/>.


## internal functions, not exported

## wrapper for paste0() needed?
if(isTRUE(R_system_version(getRversion()) < 2.15)){
  # if this is an older R version, we need a wrapper function for paste0()
  # which was introduced with R 2.15 as a more efficient shortcut to paste(..., sep="")
  paste0 <- function(..., collapse=NULL){
    return(paste(..., sep="", collapse=collapse))
  }
} else {}


## function child.list()
# convenience function to let single children be provided without list()
child.list <- function(children){
  if(is.XiMpLe.node(children)){
    children <- list(children)
  } else {
    # if already a list, check if it's a list in a list and get it out
    if(inherits(children, "list") & length(children) == 1){
      if(inherits(children[[1]], "list")){
        children <- children[[1]]
      } else {}
    } else {}
  }
  return(children)
} ## end function child.list()


## function split.chars()
# used to split a character string into parts at each occurrence of the start and end of a regex pattern
split.chars <- function(txt, pattern, perl=FALSE){
  found.pattern <- gregexpr(pattern, text=txt, perl=perl)
  found.pattern.start <- found.pattern[[1]]
  found.pattern.end <- found.pattern.start + attr(found.pattern[[1]], "match.length") - 1
  # returned -1 if pattern wasn't found
  if(found.pattern.start[1] == -1){
    return(txt)
  } else {
    txt.length <- nchar(txt)
    num.found.patterns <- length(found.pattern.start)
    result <- unlist(sapply(
      0:num.found.patterns,
      function(pat.idx){
        # 0: chars before first match
        if(pat.idx == 0){
          if(found.pattern.start[1] > 1){
            return(substr(txt, 1, found.pattern.start[1] - 1))
          } else {}
        } else {
          result.match <- substr(txt, found.pattern.start[pat.idx], found.pattern.end[pat.idx])
          # check if there's stuff between two matches
          aft.match <- found.pattern.end[pat.idx] + 1
            if(pat.idx < num.found.patterns){
              nxt.match <- found.pattern.start[pat.idx + 1]
            } else {
              nxt.match <- txt.length + 1
            }
          if(aft.match < nxt.match){
            result.aft.match <- trim(substr(txt, aft.match, nxt.match - 1))
            # remove empty space
            if(!identical("", result.aft.match)){
              result.match <- c(result.match, result.aft.match)
            } else {}
          } else {}
          return(result.match)
        }
      },
      USE.NAMES=FALSE
    ), use.names=FALSE)
    return(result)
  }
} ## end function split.chars()


## function XML.single.tags()
# Splits one character string or vector with an XML tree into a vector with its single tags.
# - tree: The XML tree, must be character.
# - drop: A character vector with the possible contens \code{c("comments","declarations","cdata","value")}
XML.single.tags <- function(tree, drop=NULL){
  if(!is.character(tree)){
    stop(simpleError("'tree' must be character!"))
  } else {}
  if(length(tree) > 1) {
    # force tree into one string
    tree <- paste(tree, collapse="")
  } else {}
  # remove space at beginning (and end)
  tree <- trim(tree)

  ## the main splitting process
  # CDATA or comments can contain stuff which might ruin the outcome. we'll deal with those parts first.
  tree <- split.chars(txt=tree, pattern="<!\\[CDATA\\[((?s).*?)\\]\\]>|/\\*[[:space:]]*<!\\[CDATA\\[[[:space:]]*\\*/((?s).*?)/\\*[[:space:]]*\\]\\]>[[:space:]]*\\*/|<!--((?s).*?)-->", perl=TRUE)
  # now do the splitting
  single.tags <- sapply(
    tree,
    function(this.tree){
      # exclude the already cut our comments an CDATA entries
      if(XML.comment(this.tree) | XML.cdata(this.tree) | XML.commcdata(this.tree)){
        return(this.tree)
      } else {
        these.tags <- unlist(split.chars(txt=this.tree, "<((?s).*?)>", perl=TRUE), use.names=FALSE)
        # remove probably troublesome content like newlines
        these.tags[!XML.value(these.tags)] <- gsub("[[:space:]]+", " ", these.tags[!XML.value(these.tags)])
        return(these.tags)
      }
    },
    USE.NAMES=FALSE
  )
  single.tags <- unlist(single.tags, use.names=FALSE)
  single.tags <- as.character(single.tags)

  if("comments" %in% drop){
    single.tags <- single.tags[!XML.comment(single.tags)]
  } else {}
  if("declarations" %in% drop){
    single.tags <- single.tags[!XML.declaration(single.tags)]
  } else {}
  if("doctype" %in% drop){
    single.tags <- single.tags[!XML.doctype(single.tags)]
  } else {}
  if("cdata" %in% drop){
    single.tags <- single.tags[!XML.cdata(single.tags)]
  } else {}
  # force garbage collection
  gc()
  return(single.tags)
} ## end function XML.single.tags()


## function setMinIndent()
# takes a string, determines the minimum number of grouped \t strings,
# and adjusts it globally to the given level
setMinIndent <- function(tag, level=1, indent.by="\t"){
  currentMinIndent <- nchar(unlist(strsplit(tag, "[^\t ]+"), use.names=FALSE))
  currentMinIndent <- ifelse(length(currentMinIndent) > 0L, min(currentMinIndent), 0)
  indentDiff <- currentMinIndent - level
  tagParts <- unlist(strsplit(tag, "\n"))
  # if currentMinIndent is greater than level, reduce indentation
  if(indentDiff > 0L){
    tagParts <- gsub(paste0("(^|\n)([\t ]){", indentDiff+1, "}"), "\\1", tagParts, perl=TRUE)
  } else if(indentDiff < 0L){
    tagParts <- paste0(indent(level=level, by=indent.by), tagParts)
  } else {}

  return(paste0(tagParts, collapse="\n"))
} ## end function setMinIndent()


## function indent()
# will create tabs to format the output
indent <- function(level, by="\t"){
  paste(rep(by, max(0, level-1)), collapse="")
} ## end function indent()


# TODO:
## function paste_shine()
# pastes nodes with a given indentation and separator between arguments
#
# output is this for shine=0:
# {start}{ attrs[1] attrs[2]}{ child}{end}
#
# output is this for shine=1:
# {start}{
#   attrs[1] attrs[2]}{
#   child}
# {end}
#
# output is this for shine=2:
# {start}{
#   attrs[1]
#   attrs[2]}{
#   child}
# {end}
#
# start: the initial tag start, e.g. "<a" or "<br"
# end: how the inital tag ends, e.g. ">" or "/>"
# attrs: optional character vector, arguments to paste
# child: optional character string, a fully indeted child node, used for recursion
# close: closong tag for non-empty tags, e.g. "</a>"
# level: level of indentation for the tag; indentation of arguments or child nodes depends on 'shine'
# indent.by: indentation character
# shine: shine level
# space_child: useful for e.g. comment tags to add a single space between start/closing tags and value
# space_attrs: similar to space_child, but adds an extra space only before the end tag
# as_script: logical, whether to separate by space (FALSE) or comma (TRUE)
paste_shine <- function(
  start,
  end,
  attrs,
  child,
  close,
  level,
  indent.by="\t",
  shine=1,
  space_child=FALSE,
  space_attrs=FALSE,
  as_script=FALSE
){
  if(isTRUE(as_script)){
    next_sep <- ","
  } else {
    next_sep <- ""
  }
  indent_node <- indent(level=level, by=indent.by)
  indent_attrs <- indent(level=level + 1, by=indent.by)
  indent_child <- indent(level=level + 1, by=indent.by)
  indent_end <- indent(level=level, by=indent.by)
  indent_close <- indent(level=level, by=indent.by)
  next_node <- "\n"
  extra_space_child <-""
  extra_space_attrs <-""
  if(isTRUE(as_script)){
    next_attr <- next_sep
  } else {
    next_attr <- paste0(next_sep, "\n")
  }
  next_close <- "\n"
  first_attr <- "\n"
  first_child <- "\n"

  if(shine < 1){
    # shine is 0
    indent_attrs <- ""
    indent_child <- ""
    indent_end <- ""
    indent_close <- ""
    next_node <- ""
    next_close <- ""
    if(isTRUE(as_script)){
      next_attr <- paste0(next_sep, " ")
      first_attr <- ""
    } else {
      next_attr <- next_sep
      first_attr <- " "
      if(isTRUE(space_child)){
        extra_space_child <-" "
      } else {}
      if(isTRUE(space_attrs)){
        extra_space_attrs <-" "
      } else {}
    }
    first_child <- ""
  } else if(shine < 2){
    # shine is 1
    if(isTRUE(as_script)){
      indent_attrs <- ""
    } else {
      indent_attrs <- " "
      if(isTRUE(space_attrs)){
        extra_space_attrs <-" "
      } else {}
    }
    indent_end <- ""
    next_attr <- next_sep
    first_attr <- ""
  } else {
    # shine is 2, keep defaults
  }

  no_attrs <- no_child <- no_close <- FALSE
  if(missing(attrs)){
    attrs <- ""
    no_attrs <- TRUE
  } else if(any(identical(trim(attrs), ""), identical(trim(attrs), character()))){
    no_attrs <- TRUE
  }
  if(missing(child)){
    child <- ""
    no_child <- TRUE
  } else if(any(identical(trim(child), ""), identical(trim(child), character()))){
    no_child <- TRUE
  }
  if(missing(close)){
    close <- ""
    no_close <- TRUE
  } else if(any(identical(trim(close), ""), identical(trim(close), character()))){
    no_close <- TRUE
  }

  if(isTRUE(no_attrs)){
    indent_attrs <- ""
    indent_end <- ""
    next_attr <- ""
    first_attr <- ""
    if(isTRUE(space_attrs)){
      extra_space_attrs <-" "
    } else {}
  } else {}

  if(isTRUE(no_close)){
    if(!isTRUE(no_child)){
      stop(simpleError("Invalid call to XiMpLe:::paste_shine(): Missing closing tag!"))
    } else {}
    next_close <- ""
    indent_close <- ""
  } else {}

  if(isTRUE(no_child)){
    indent_child <- ""
    child <- ""
    if(any(shine < 2, as_script)){
      next_attr <- ""
    } else {}
    first_child <- ""
    extra_space_child <-""
  } else {}

  if(
      all(
        isTRUE(as_script),
        any(
          identical(attrs, ""),
          identical(shine, 1)
        ),
        identical(child, "")
      )
  ){
    next_close <- ""
    indent_close <- ""
  } else {}

  ## debugging:
  # message(
  #   paste0(
  #     "level: ", level, "\n",
  #     "indent_node: ", deparse(indent_node), "\n",
  #     "start: ", deparse(start), "\n",
  #     "first_attr: ", deparse(first_attr), "\n",
  #     "indent_attrs: ", deparse(indent_attrs), "\n",
  #     "attrs: ", deparse(attrs), "\n",
  #     "next_attr: ", deparse(next_attr), "\n",
  #     "extra_space_attrs: ", deparse(extra_space_attrs), "\n",
  #     "indent_end: ", deparse(indent_end), "\n",
  #     "end: ", deparse(end), "\n",
  #     "first_child: ", deparse(first_child), "\n",
  #     "indent_child: ", deparse(indent_child), "\n",
  #     "extra_space_child: ", deparse(extra_space_child), "\n",
  #     "child: ", deparse(child), "\n",
  #     "next_close: ", deparse(next_close), "\n",
  #     "indent_close: ", deparse(indent_close), "\n",
  #     "close: ", deparse(close), "\n",
  #     "next_node: ", deparse(next_node), "\n"
  #   )
  # )

  return(
    paste0(
      indent_node, start, first_attr,
      indent_attrs, attrs, next_attr, extra_space_attrs,
      indent_end, end, first_child,
      indent_child, extra_space_child, trim(child), extra_space_child,
      next_close,
      indent_close, close,
      next_node
    )
  )
} ## end function paste_shine()


## function xml.tidy()
# replace special character < and > from attributes or text values
# with harmless entities
xml.tidy <- function(text){
  if(is.character(text)){
    tidy.text <- gsub("<", "&lt;", gsub(">", "&gt;", gsub("&([#[:alnum:]]{7}[^;]|[[:space:]]|[^;]*$)", "&amp;\\1", text, perl=TRUE)))
  } else {
    return(text)
  }
  return(tidy.text)
} ## function xml.tidy()


## function lookupAttrName()
# takes the original input element names and returns
# the according XML attribute name
lookupAttrName <- function(tag, attr, rename){
  if(is.null(tag)){
    attr.name <- attr
  } else {
    attr.name <- rename[[tag]][[attr]]
  }
  return(attr.name)
} ## end function lookupAttrName()


## function pasteXMLAttr()
# pastes all attributes in a nicely readable way
pasteXMLAttr <- function(
  attr=NULL,
  tag=NULL,
  level=1,
  rename=NULL,
  shine=2,
  indent.by="\t",
  tidy=FALSE,
  as_script=FALSE
){
  if(is.null(attr)){
    return("")
  } else {}

  if(isTRUE(tidy)){
    attr <- sapply(attr, xml.tidy, USE.NAMES=FALSE)
  } else {}

  new.indent <- ifelse(shine > 1, indent(level+1, by=indent.by), "")
  new.attr   <- ifelse(shine > 1, "\n", " ")
  paste_collapse <- ifelse(isTRUE(as_script), paste0(",", new.attr, new.indent), paste0(new.attr, new.indent))

  # only use formatting if more than one attribute
  if(length(attr) > 1){
    full.attr <- c()
    full.attr <- paste0(sapply(
      names(attr),
      function(this.attr){
        # skip empty elements
        if(is.null(attr[[this.attr]])){
          return()
        } else {
          if(!is.null(rename)){
            # look up attribute name to paste
            attr.name <- lookupAttrName(tag, this.attr, rename=rename)
          } else {
            attr.name <- this.attr
          }
          if(identical(attr[[this.attr]], character())){
            # empty argument
            if(isTRUE(as_script)){
              attr_value <- "=character()"
            } else {
              attr_value <- ""
            }
          } else {
            attr_value <- paste0("=\"", attr[[this.attr]], "\"")
          }
          return(
            trim(paste0(full.attr, new.attr, new.indent, attr.name, attr_value))
          )
        }
      }
    ), collapse=paste_collapse)
#     for (this.attr in seq_along(all_attrs)){
#       # skip empty elements
#       if(is.null(attr[[this.attr]])){next}
#       if(!is.null(rename)){
#         # look up attribute name to paste
#         attr.name <- lookupAttrName(tag, this.attr, rename=rename)
#       } else {
#         attr.name <- this.attr
#       }
#       full.attr <- trim(paste0(full.attr, new.attr, new.indent, attr.name, "=\"", attr[[this.attr]], "\""))
#     }
  } else {
    if(!is.null(rename)){
      # look up attribute name to paste
      attr.name <- lookupAttrName(tag, names(attr), rename=rename)
    } else {
      attr.name <- names(attr)
    }
    if(identical(attr[[1]], character())){
      # empty argument
      if(isTRUE(as_script)){
        attr_value <- "=character()"
      } else {
        attr_value <- ""
      }
    } else {
      attr_value <- paste0("=\"", attr[[1]], "\"")
    }
    full.attr <- paste0(attr.name, attr_value)
  }
  return(full.attr)
} ## end function pasteXMLAttr()


## function args2list()
# takes a string that was separated from a tag, containing only its attributes,
# and tries to turn it into a named list
# drop_empty_tags: if set to TRUE, empty tags will be removed, otherwise they will
#   get an empty character value assigned to them
# doctype_args: if TRUE, quoted empty attributes will temporarily be named for
#   counting the start and end points of arguments, to not confuse the parser
attr2list <- function(attr, drop_empty_tags=FALSE, doctype_args=FALSE){
  # regular expression to detect alphanumeric characters (we'll also accept some more,
  # this is mainly needed to safely detect spaces and quotes from argument values
  alnum_plus <- "[-_/'|*#@+~&%$§.,:;(){}?![:alnum:]]"
  doctype_restore <- FALSE
  qr_to_use <- ""
  if(isTRUE(doctype_args) & grepl("\"", attr)){
    # find a temporary name for quoted empty attributes,
    # testing a few that are unlikely all used here
    quote_replacer <- c(
      "\u0142", # "ł" (latin small letter "L" with stroke)
      "\u014B", # "ŋ" (latin small letter "Eng")
      "\u0167", # "ŧ" (latin small letter "T" with stroke)
      "\u00E6", # "æ" (latin small letter "Ae")
      "\u00F0", # "ð" (latin small letter "Eth")
      "\u00F8", # "ø" (latin small letter "O" with stroke)
      "\u00FE"  # "þ" (latin small letter "Thorn")
    )
    qr_in_attrs <- sapply(
      quote_replacer,
      function(this_qr){
        any(grepl(this_qr, x=attr))
      }
    )
    if(!all(qr_in_attrs)){
      qr_to_use <- names(which.min(!qr_in_attrs))
      attr <- gsub("[[:space:]]\"", paste0(" ", qr_to_use, "=\""), attr)
      doctype_restore <- TRUE
    } else {
      # go on without replacement, this will likely result in an error
    }
  } else {}
  # split into individual characters
  attr_chars <- unlist(strsplit(trim(attr), ""))
  if(length(attr_chars) > 0){
    # which one is alphanumeric (we'll also accept "-" and "_"?
    attr_alnum <- grepl(alnum_plus, attr_chars)
    # find continuous patterns in the boolean vector, i.e. detect words vs. nonwords
    attr_alnum_rle <- rle(attr_alnum)
    # add zero for the sapply loop
    attr_borders <- c(0, cumsum(attr_alnum_rle[["lengths"]]))
    # here we glue the single characters together again,
    # this way we'll get words in one string an nonwords in separated strings
    # the result should be a vector of strings that is either
    #   - just alphanumeric, which could be either an attribute name or part of an attribute value
    #   - non-alphanumeric, which could be
    #     - space
    #     - "=\"" (including spaces, start of an attribute value)
    #     - "\"" (including spaces, end of an attribute value)
    #     - anything else, part of an attribute value
    attr_tokens <- sapply(seq_along(attr_borders)[-1],
      function(n){
        paste0(attr_chars[(attr_borders[n-1] + 1):attr_borders[n]], collapse="")
      }
    )
    tokens_n <- seq_along(attr_tokens)
    # now we'll mark start and end of attributes
    attr_on <- grepl("^[[:space:]]*=[[:space:]]*\"[[:space:]]*$", attr_tokens)
    attr_off <- grepl("^[[:space:]]*\"[[:space:]]*$", attr_tokens)
    # numbers must match, otherwise there was an error in parsing and we should PANIC! :D
    if(!identical(sum(attr_on), sum(attr_off))){
      stop(
        simpleError(
          paste0(
            "Looks like I failed to parse attributes correctly. This one i couldn't digest:\n  \"",
            paste0(attr_chars, collapse=""),
            "\""
          )
        )
      )
    } else {}
    if(sum(attr_on, attr_off) > 0){
      # now we can assume the range of attribute values
      # make it a list to keep them separated
      # each list entry is a vector of one full attribute value
      attr_values <- lapply(
        1:sum(attr_on),
        function(n){
          which(attr_on)[n]:which(attr_off)[n]
        }
      )
      # right before each attribute value should be the attribute's name
      attr_names <- sapply(
        attr_values,
        function(val){
          if(val[1] < 2){
            stop(simpleError("I've detected an attribute value without an attribute name!"))
          } else {
            arg_name_n <- val[1] - 1
          }
          if(!isTRUE(grepl(alnum_plus, attr_tokens[arg_name_n]))){
            warning(paste0("This attribute name might be invalid, please check: \"", attr_tokens[arg_name_n], "\""), call.=FALSE)
          } else {}
          return(arg_name_n)
        }
      )
    } else {
      # only empty attributes?
      attr_values <- list()
      attr_names <- attr_tokens
    }

    # for safety reasons, consider putting arg names in quotes when non alphanumeric strings are used
    non_alnum_names <- grepl("[^[:alnum:]]", attr_tokens[attr_names])
    if(any(non_alnum_names)){
      attr_tokens[attr_names[non_alnum_names]] <- paste0("\"", attr_tokens[attr_names[non_alnum_names]], "\"")
    } else {}

    # if we've gotten this far, all that's neither an attribute name nor its value is probably an empty attribute
    attr_done <- sort(c(attr_names, unlist(attr_values)))
    attr_unknown <- tokens_n[!tokens_n %in% attr_done]
    if(length(attr_unknown) > 0){
      attr_unknown <- attr_unknown[!grepl("^[[:space:]]+$", attr_tokens[attr_unknown])]
      if(length(attr_unknown) > 0){
        # set value for empty attribute
        attr_tokens <- unlist(sapply(
          tokens_n,
          function(n){
            if(isTRUE(n %in% attr_unknown)){
              if(isTRUE(drop_empty_tags)){
                return("")
              } else {
                return(c(attr_tokens[n], "=character()"))
              }
            } else {
              return(attr_tokens[n])
            }
          }
        ))
        # recalculate attr_off
        attr_off <- grepl("^[[:space:]]*\"[[:space:]]*$|^[[:space:]]*=character\\(\\)[[:space:]]*$", attr_tokens)
      } else {}
    } else {}
    # all but the last value closing will need a comma separator
    if(length(which(attr_off)) > 1){
      add_comma <- which(attr_off)
      add_comma <- add_comma[1:(length(add_comma) - 1)]
      attr_tokens[add_comma] <- paste0(attr_tokens[add_comma], ",")
    } else {}
    result <- eval(parse(text=paste("list(", paste0(attr_tokens, collapse=""), ")")))
    if(isTRUE(doctype_restore)){
      # restore the original attributes
      to_restore <- which(names(result) %in% qr_to_use)
      if(length(to_restore) > 0){
        new_names <- paste0("\"", result[to_restore], "\"")
        for(this_attr in to_restore){
          result[[this_attr]] <- character()
        }
        names(result)[to_restore] <- new_names
      } else {}
    } else {}
    return(result)
  } else {
    return(list())
  }
} ## end function attr2list()


## function parseXMLAttr()
# takes a whole XML tag and returns a named list with its attributes
parseXMLAttr <- function(tag, drop_empty_tags=FALSE){
#   if(XML.doctype(tag)){
#     stripped.tag <- gsub("<!((?i)DOCTYPE)[[:space:]]+([^[:space:]]+)[[:space:]]*([^\"[:space:]]*)[[:space:]]*.*>",
#       "doctype=\"\\2\", decl=\"\\3\"", tag)
#     stripped.tag2 <- eval(parse(text=paste("c(",gsub("[^\"]*[\"]?([^\"]*)[\"]?[^\"]*", "\"\\1\",", tag),"NULL)")))
#     is.dtd <- grepl("\\.dtd", stripped.tag2)
#     doct.decl <- ifelse(sum(!is.dtd) > 0, paste0(stripped.tag2[!is.dtd][1]), paste0(""))
#     doct.ref <- ifelse(sum(is.dtd) > 0, paste0(stripped.tag2[is.dtd][1]), paste0(""))
#     parsed.list <- eval(parse(text=paste0("list(", stripped.tag, ", id=\"", doct.decl,"\"", ", refer=\"", doct.ref,"\")")))
#   } else if(XML.endTag(tag) | XML.comment(tag) | XML.cdata(tag)){
  if(XML.endTag(tag) | XML.comment(tag) | XML.cdata(tag)){
    # end tags, comments and CDATA don't have attributes
    parsed.list <- ""
  } else {
    # first strip of start and end characters
    stripped.tag <- gsub("<([?[:space:]]*)[^[:space:]]+[[:space:]]*(.*)", "\\2", tag, perl=TRUE)
    stripped.tag <- trim(gsub("[/?]*>$", "", stripped.tag, perl=TRUE))
    parsed.list <- attr2list(stripped.tag, drop_empty_tags=drop_empty_tags, doctype_args=XML.doctype(tag))
  }
  if(XML.declaration(tag)){
    # only enforce validation for <?xml ... ?>
    if(identical(XML.tagName(tag), tolower("?xml"))){
      valid.attr <- c("version", "encoding", "standalone")
      parsed.list <- parsed.list[tolower(names(parsed.list)) %in% valid.attr]
      for (miss.attr in valid.attr[!valid.attr %in% tolower(names(parsed.list))]){
        parsed.list[[miss.attr]] <- ""
      }
    } else {}
  } else {}

  return(parsed.list)
} ## end function parseXMLAttr()


## function trim()
# cuts off space at start and end of a character string
trim <- function(char){
  char <- gsub("^[[:space:]]*", "", char)
  char <- gsub("[[:space:]]*$", "", char)
  return(char)
} ## end function trim()


## function XML.emptyTag()
# checks if a tag is a pair of start/end tags or an empty tag;
# returns either TRUE/FALSE, or the tag name if it is an empty tag and get=TRUE
XML.emptyTag <- function(tag, get=FALSE){
  empty.tags <- sapply(
    tag,
    function(this.tag){
      empty <- grepl("/>$", this.tag)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(empty), XML.tagName(this.tag), "")
      } else {
        result <- empty
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(empty.tags)
} ## end function XML.emptyTag()


## function XML.endTag()
# checks if a tag an end tag;
# returns either TRUE/FALSE, or the tag name if it is an end tag and get=TRUE
XML.endTag <- function(tag, get=FALSE){
  end.tags <- sapply(
    tag,
    function(this.tag){
      end <- grepl("^</", this.tag)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(end), XML.tagName(this.tag), "")
      } else {
        result <- end
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(end.tags)
} ## end function XML.endTag()


## function XML.comment()
# checks if a tag is a comment, returns TRUE or FALSE, or the comment (TRUE & get=TRUE)
XML.comment <- function(tag, get=FALSE, trim=TRUE){
  comment.tags <- sapply(
    tag,
    function(this.tag){
      comment <- grepl("<!--((?s).*)-->", this.tag, perl=TRUE)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(comment), gsub("<!--((?s).*)-->", "\\1", this.tag, perl=TRUE), "")
        if(isTRUE(trim)){result <- trim(result)} else {}
      } else {
        result <- comment
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(comment.tags)
} ## end function XML.comment()


## function XML.cdata()
# checks if a tag is a CDATA declaration, returns TRUE or FALSE, or the data (TRUE & get=TRUE)
XML.cdata <- function(tag, get=FALSE, trim=TRUE){
  cdata.tags <- sapply(
    tag,
    function(this.tag){
      cdata <- grepl("<!\\[CDATA\\[((?s).*)\\]\\]>", this.tag, perl=TRUE)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(cdata), gsub("<!\\[CDATA\\[((?s).*)\\]\\]>", "\\1", this.tag, perl=TRUE), "")
        if(isTRUE(trim)){result <- trim(result)} else {}
      } else {
        result <- cdata
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(cdata.tags)
} ## end function XML.cdata()


## function XML.commcdata()
# checks if a tag is a /* CDATA */ declaration, returns TRUE or FALSE, or the data (TRUE & get=TRUE)
XML.commcdata <- function(tag, get=FALSE, trim=TRUE){
  commcdata.tags <- sapply(
    tag,
    function(this.tag){
      commcdata <- grepl("/\\*[[:space:]]*<!\\[CDATA\\[[[:space:]]*\\*/((?s).*?)/\\*[[:space:]]*\\]\\]>[[:space:]]*\\*/", this.tag, perl=TRUE)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(commcdata), gsub("/\\*[[:space:]]*<!\\[CDATA\\[[[:space:]]*\\*/((?s).*?)/\\*[[:space:]]*\\]\\]>[[:space:]]*\\*/", "\\1", this.tag, perl=TRUE), "")
        if(isTRUE(trim)){result <- trim(result)} else {}
      } else {
        result <- commcdata
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(commcdata.tags)
} ## end function XML.commcdata()


## function XML.value()
# checks if 'tag' is actually not a tag but value/content/data. returns TRUE or FALSE, or the value (TRUE & get=TRUE)
XML.value <- function(tag, get=FALSE, trim=TRUE){
  all.values <- sapply(
    tag,
    function(this.tag){
      value <- grepl("^[[:space:]]*[^<]", this.tag)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(value), this.tag, "")
        if(isTRUE(trim)){result <- trim(result)} else {}
      } else {
        result <- value
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(all.values)
} ## end function XML.value()


## function XML.declaration()
# checks for a declaration, like <?xml bar?>
XML.declaration <- function(tag, get=FALSE){
  decl.tags <- sapply(
    tag,
    function(this.tag){
      declaration <- grepl("<\\?((?i)xml).*\\?>", this.tag, perl=TRUE)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(declaration), XML.tagName(this.tag), "")
      } else {
        result <- declaration
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(decl.tags)
} ## end function XML.declaration()


## function XML.doctype()
# checks for a doctype declaration, like <!DOCTYPE foo>
XML.doctype <- function(tag, get=FALSE){
  decl.tags <- sapply(
    tag,
    function(this.tag){
      declaration <- grepl("<!((?i)DOCTYPE).*>", this.tag, perl=TRUE)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(declaration), XML.tagName(this.tag), "")
      } else {
        result <- declaration
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(decl.tags)
} ## end function XML.doctype()


## function XML.def()
XML.def <- function(tag, get=FALSE){
  decl.tags <- sapply(
    tag,
    function(this.tag){
      declaration <- grepl("<[!?]+[^-]*>", this.tag)
      if(isTRUE(get)){
        result <- ifelse(isTRUE(declaration), XML.tagName(this.tag), "")
      } else {
        result <- declaration
      }
      return(result)
    },
    USE.NAMES=FALSE
  )
  return(decl.tags)
} ## end function XML.def()


## function XML.tagName()
XML.tagName <- function(tag){
  tag.names <- sapply(
    tag,
    function(this.tag){
      tagName <- gsub("<([[:space:]!?/]*)([^[:space:]>/]+).*", "\\2", this.tag, perl=TRUE)
      return(tagName)
    },
    USE.NAMES=FALSE
  )
  return(tag.names)
} ## end function XML.tagName()


# unused?
# ## function parseXMLTag()
# parseXMLTag <- function(tag, drop_empty_tags=FALSE){
#   tag.name <- XML.tagName(tag)
#   tag.attr <- parseXMLAttr(tag, drop_empty_tags=drop_empty_tags)
#   if(!is.null(tag.attr)){
#     parsed.tag <- list()
#     parsed.tag[[tag.name]] <- list(attr=tag.attr)
#   } else {
#     parsed.tag <- list()
#     parsed.tag[[tag.name]] <- list()
#   }
#   return(parsed.tag)
# } ## end function parseXMLTag()


## function XML.nodes()
XML.nodes <- function(single.tags, end.here=NA, drop_empty_tags=FALSE, start=1){
  # to save memory, we'll put the single.tags object into an environment
  # and pass that on to all iterations
  if(is.environment(single.tags)){
    single.tags.env <- single.tags
    num.all.tags <- length(get("single.tags", envir=single.tags.env))
  } else {
    single.tags.env <- new.env()
    assign("single.tags", single.tags, envir=single.tags.env)
    num.all.tags <- length(single.tags)
  }
  # try to iterate through the single tags
  children <- list()
  tag.no <- start
  ## uncomment to debug:
  # cat(start,"\n")
  while (tag.no <= num.all.tags){
    ## uncomment to debug:
    # time.spent <- system.time({
    this.tag <- get("single.tags", envir=single.tags.env)[tag.no]
    nxt.child <- length(children) + 1
    child.name <- XML.tagName(this.tag)
    child.end.tag <- paste0("</[[:space:]]*", end.here,"[[:space:]>]+.*")
    if(isTRUE(grepl(child.end.tag, this.tag))){
    ## uncomment to debug:
    # cat(this.tag, ": break (",tag.no,")\n")
      break
    } else {}
    # we must test for commented CDATA first, because XML.value() would be TRUE, too
    if(XML.commcdata(this.tag)){
      children[[nxt.child]] <- XiMpLe_node(
        name="*![CDATA[",
        value=XML.commcdata(this.tag, get=TRUE)
      )
      names(children)[nxt.child] <- "*![CDATA["
      tag.no <- tag.no + 1
      next
    } else {}
    if(XML.value(this.tag)){
      children[[nxt.child]] <- XiMpLe_node(
        name="",
        value=XML.value(this.tag, get=TRUE)
      )
      names(children)[nxt.child] <- "!value!"
      tag.no <- tag.no + 1
      next
    } else {
      child.attr <- parseXMLAttr(this.tag, drop_empty_tags=drop_empty_tags)
    }
    if(XML.declaration(this.tag)){
      children[[nxt.child]] <- XiMpLe_node(
        name=child.name,
        attributes=child.attr
      )
      names(children)[nxt.child] <- child.name
      tag.no <- tag.no + 1
      next
    } else {}
    if(XML.comment(this.tag)){
      children[[nxt.child]] <- XiMpLe_node(
        name="!--",
        value=XML.comment(this.tag, get=TRUE)
      )
      names(children)[nxt.child] <- "!--"
      tag.no <- tag.no + 1
      next
    } else {}
    if(XML.cdata(this.tag)){
      children[[nxt.child]] <- XiMpLe_node(
        name="![CDATA[",
        value=XML.cdata(this.tag, get=TRUE)
      )
      names(children)[nxt.child] <- "![CDATA["
      tag.no <- tag.no + 1
      next
    } else {}
    if(XML.endTag(this.tag)){
      break
    } else {}
    if(!XML.emptyTag(this.tag)){
    ## uncomment to debug:
    # cat(child.name, ":", tag.no, "-", child.end.tag,"\n")
      rec.nodes <- XML.nodes(single.tags.env, end.here=child.name, drop_empty_tags=drop_empty_tags, start=tag.no + 1)
      children[[nxt.child]] <- XiMpLe_node(
        name=child.name,
        attributes=child.attr,
        children=rec.nodes$children,
        # this value will force the node to remain non-empty if it had no children,
        # it would be turned into an empty tag otherwise
        value=""
      )
      names(children)[nxt.child] <- child.name
      tag.no <- rec.nodes$tag.no + 1
      next
    } else {
      children[[nxt.child]] <- XiMpLe_node(
        name=child.name,
        attributes=child.attr
      )
      names(children)[nxt.child] <- child.name
      tag.no <- tag.no + 1
      next
    }
    ## uncomment to debug:
    # })
    # cat("system.time:", time.spent, "\n")
  }
  return(list(children=children, tag.no=tag.no))
} ## end function XML.nodes()


## function valid.child()
# - parent: character string, name of the parent node
# - children: (list of) XiMpLe.node objects, child nodes to check
# - validity: definitions of valid child nodes, class XiMpLe.validity
# - warn: warning or stop?
# - section: an optional name for the section for the warning/error
#   (if it shouldn't be the parent name)
# - node names: can alternatively be given instead of 'children', as character vector
# - graceful: allow everything inside "!--" comments?
valid.child <- function(parent, children, validity, warn=FALSE, section=parent, node.names=NULL,
  caseSens=TRUE, graceful=TRUE){
  if(isTRUE(graceful) & identical(parent, "!--")){
    # skip all checks and return TRUE
    return(TRUE)
  } else {}
  if(is.null(node.names)){
    # check the node names and allow only valid ones
    node.names <- unlist(sapply(
      child.list(children),
      function(this.child){
        if(is.XiMpLe.node(this.child)){
          this.child.name <- XMLName(this.child)
          if(identical(this.child.name, "")){
            # special case: empty node name; this is used to combine
            # comments with the node they belong to, so rather check
            # the children of this special node
            return(unlist(sapply(XMLChildren(this.child), XMLName, USE.NAMES=FALSE)))
          } else {
            return(this.child.name)
          }
        } else {
          stop(simpleError(paste0("Invalid object for <", section, "> node, must be of class XiMpLe.node, but got class ", class(this.child), "!")))
        }
      },
      USE.NAMES=FALSE
    ))
  } else {}

  validAllChildren <- slot(validity, "allChildren")
  validChildren <- slot(validity, "children")[[parent]]
  # check for recursion
  if(is.XiMpLe.validity(validChildren)){
    validChildren <- c(
      names(slot(validChildren, "children")), 
      slot(validChildren, "allChildren")
    )
  } else {}
  
  ignoreChildren <- slot(validity, "ignore")
  if(!isTRUE(caseSens)){
    node.names <- tolower(node.names)
    validAllChildren <- tolower(validAllChildren)
    validChildren <- tolower(validChildren)
    ignoreChildren <- tolower(ignoreChildren)
  } else {}

  invalid.sets <- !node.names %in% c(validAllChildren, validChildren, ignoreChildren)
  if(any(invalid.sets)){
    return.message <- paste0("Invalid XML nodes for <", section, "> section: ", paste(node.names[invalid.sets], collapse=", "))
    if(isTRUE(warn)){
      warning(return.message, call.=FALSE)
      return(FALSE)
    } else {
      stop(simpleError(return.message))
    }
  } else {
    return(TRUE)
  }
} ## end function valid.child()


## function valid.attribute()
# similar to valid.child(), but checks the validity of attributes of a given node
# it's a bit simpler
# - node: a character string, node name
# - attrs: a named list of attributes to check
# - validity: definitions of valid child nodes, class XiMpLe.validity
valid.attribute <- function(node, attrs, validity, warn=FALSE, caseSens=TRUE){
  if(length(attrs) > 0){
    attrsNames <- names(attrs)
    validAllAttrs <- slot(validity, "allAttrs")
    validAttrs <- slot(validity, "attrs")[[node]]
    ignoreNodes <- slot(validity, "ignore")
    if(!isTRUE(caseSens)){
      attrsNames <- tolower(attrsNames)
      validAllAttrs <- tolower(validAllAttrs)
      validAttrs <- tolower(validAttrs)
      ignoreNodes <- tolower(ignoreNodes)
    } else {}
    if(node %in% ignoreNodes){
      return(NULL)
    } else {
      invalid.sets <- !attrsNames %in% c(validAllAttrs, validAttrs)
      if(any(invalid.sets)){
        return.message <- paste0("Invalid XML attributes for <", node, "> node: ", paste(attrsNames[invalid.sets], collapse=", "))
        if(isTRUE(warn)){
          warning(return.message, call.=FALSE)
          return(FALSE)
        } else {
          stop(simpleError(return.message))
        }
      } else {
        return(TRUE)
      }
    }
  } else {
    return(NULL)
  }
} ## end function valid.attribute()


## function validParamName()
# called by XMLgenerators() to ensure valid parameter names in the generated function calls
validParamName <- function(name, replacement="_"){
  return(gsub(pattern="[^a-zA-Z0-9_.]", replacement=replacement, x=name))
} ## end function validParamName()
