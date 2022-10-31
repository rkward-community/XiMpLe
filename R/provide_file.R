# Copyright 2022 Meik Michalke <meik.michalke@hhu.de>
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


#' Manage static files in project directory
#'
#' Copies or overwrites files from a source directory to your project directory. Can be used
#' to make sure that files you are referencng in your generated XML code are present and up to date.
#'
#' The function returns the relative path that was given as its first argument, e.g. it can be used
#' inside \code{XMLNode} to add relative paths to arguments while also copying the referenced file
#' to the given output directory, keeping the relative path.
#' 
#' It can be useful to write a simple wrapper around theis function to set the relevant \code{from}
#' and \code{to} paths for a project (see examples).
#'
#' @param rel Relative path of file as to be used in HTML.
#' @param to Full path to the project directory where files should be copied to.
#' @param from Full path to the directory where the file can be found under its \code{rel_path}.
#' @param overwrite Logical, whether existing files should be re-written or kept in place.
#' @param mode Permissions for newly created directories below \code{to}.
#' @param quiet Logical, whether you would like to see a message whem files are copied or already exist.
#' @return When called, the file is copied from the \code{from} to the \code{to} directory,
#'    including the relative path given by \code{rel}. Missing target directories below \code{to}
#'    are created on-the-fly. If successful, the function finally returns an invisible character
#'    string identical to \code{rel}.
#' @export
#' @examples
#' \dontrun{
#' # a direct call that would copy the file ~/webpage/v1/static/css/bootstrap.min.css
#' # to the project directory as /tmp/static/css/bootstrap.min.css
#' # and include "static/css/bootstrap.min.css" in the <link> tag
#' my_HTML <- XMLNode(
#'   "link",
#'   rel="stylesheet",
#'   type="text/css",
#'   href=provide_file(
#'     rel="static/css/bootstrap.min.css",
#'     to="/tmp",
#'     from="~/webpage/v1"
#'   )
#' )
#'
#' # for larger projects, a wrapper function might become handy
#' prov <- function(
#'   rel,
#'   to="/tmp",
#'   from="~/webpage/v1",
#'   overwrite=TRUE,
#'   mode="0777"
#' ){
#'   provide_file(rel=rel, to=to, from=from, overwrite=overwrite, mode=mode)
#' }
#' # let's combine it with a shortcut function for <link>
#' gen_tag_functions("link")
#' # now this code produces the same result as the direct call above
#' my_HTML2 <- link_(
#'   rel="stylesheet",
#'   type="text/css",
#'   href=prov("static/css/bootstrap.min.css")
#' )
#' }

provide_file <- function(
    rel,
    to,
    from,
    overwrite=TRUE,
    mode="0777",
    quiet=FALSE
){
    # does the input file exist?
    source_path <- normalizePath(file.path(from, rel), mustWork=TRUE)
    # what about the project directory?
    proj_dir <- normalizePath(to, mustWork=TRUE)
    target_path <- file.path(proj_dir, rel)
    target_dir <- dirname(target_path)
    if(!dir.exists(target_dir)){
        if(!isTRUE(quiet)){
            message(paste0("Creating directory in project: ", dirname(rel)))
        } else {}
        dir.create(target_dir, recursive=TRUE, mode=mode)
    } else {}
    if(any(!file.exists(target_path), isTRUE(overwrite))){
        if(!isTRUE(quiet)){
            if(!file.exists(target_path)){
                message(paste0("Adding file to project: ", rel))
            } else {
                message(paste0("Renewing file in project: ", rel))
            }
        } else {}
        file.copy(from=source_path, to=target_path, recursive=FALSE, copy.date=TRUE)
    } else {
        if(!isTRUE(quiet)){
            message(paste0("File exists in project: ", rel))
        } else {}
    }
    return(invisible(rel))
}
