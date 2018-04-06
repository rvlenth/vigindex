# Code for creating an index vignette

#' Generate a vignette topical index.
#'
#' Read indexing tags from a set of R-Markdown vignettes and create
#' a new vignette with topic names and links to the tagged locations.
#'
#' In ordinary package development, the default areguments will
#' index all of the package's vignettes and create an index source file
#' named \code{"vignette-topics.Rmd"}.
#'
#' If the target file does not exist, one is created.
#' The user may edit the target file once created. The function looks
#' for an HTML comment line (\code{<!-- (comment) -->}) -- the comment
#' must start at the beginning of a line. Everything after this comment
#' line will be replaced by a new set of indexing information each time
#' \code{vigindex} is run. Everything before the comment line is retained.
#'
#' @param dir character name of directory where vignettes are stored
#' @param vignettes character vector of names of files to index.
#'   These files should contain tags for entries in the index; see
#'   the section below on index tags.
#' @param target name to store the result (in the same directory).
#'        If \code{target} already exists, it will be replaced,
#'        except that the initial part of it will be kept as-is.
#'        Also, \code{target} is excluded from being indexed,
#'        even if it is listed in \code{vignettes}.
#' @return Nothing is returned, but the contents of \code{target} are
#'   created or altered, and a message indicates when it is complete.
#'
#' @export
#'
#' @section Index tags:
#' Indexing tags are placed within HTML comments in the vignette source
#' files, following a special format illustrated in the following example:
#' \preformatted{
#' ### Everything you want to know about owls {#owls}
#' <!-- @index **Birds**!Owls; `print.owls` -->
#' #'     ... lines of text ...
#' #### Popular owls {#pop.owls}
#' <!-- @index birds!owls!!Screech; birds!owls!Great horned
#'             Birds!Owls!Barred -->
#'     ... lines of text ...
#' <!-- @index Raptors; birds!Owls!others@zzz        -->
#' }
#' The rules are as follows:
#' \enumerate{
#'   \item The keyword \code{@index} signals there are index tags
#'   \item There may be several index items on a line, separated
#'         by semicolons (\code{;}).
#'   \item The comment may extend over more than one line -- do not
#'         repeat the \code{@index} keyword
#'   \item Entries are separated from subentries using the bang
#'         (\code{!}) character (they may be nested as much as 8 deep).
#'   \item Markdown formatting may be used in entries
#'   \item When several identical entries exist, the case and formatting
#'         is determined by its first appearance
#'   \item Entries are sorted alphabetically, at each level,
#'         based on keys derived by stripping case and formatting from
#'         the entries
#'   \item However, if there is an \code{@} character in an entry,
#'         the text that follows \code{@} will be used as the sorting key.
#' }
#' Thus, in the above example, the first main entry will be \bold{Birds}
#' (in boldface and capitalized per its first appearance),
#' with a subentry for Owls, with subentries for Barred, Great horned, Screech,
#' and others (last because of the \code{@zzz}). The next main entry will be
#' \code{print.owls}, and finally, Raptors.
#'
#' @section Links:
#' Links in the generated index will be to the latest anchor (of the
#' form \code{\{#anchor\}}) found
#' in the document, or to the top of the file if no anchors occur
#' before the index tag.
#'
#' Because Markdown only allows anchors with headings,
#' the links will be fairly crude, and users are advised to use
#' a lot of sectioning in writing vignettes.
vigindex = function(dir = "vignettes",
                    vignettes = dir(dir, pattern = "*.Rmd"),
                    target = "vignette-topics.Rmd") {
    vignettes = setdiff(vignettes, target) # exclude target
    tree = list()
    for (vig in vignettes) {
        n.ent = 0
        cat(paste(vig, "...\n"))
        buffer = scan(paste(dir, vig, sep = "/"), what = "", sep = "\n")
        entlist = find_vientries(buffer)
        ext = substring(vig, regexpr("\\.", vig))
        vig.html = sub(ext, ".html", vig)
        for (ent in entlist) {
            link = paste0(vig.html, ent$link)
            for (stg in ent$entries) {
                if (stg != "") {
                    tree = insert_entry(tree, strsplit(stg, "!")[[1]], link)
                    n.ent = n.ent + 1
                }
            }
        }
        message(n.ent, " entries processed\n")
    }
    targ.file = paste(dir, target, sep = "/")
    cat("\nPreparing index file ...\n")
    if (file.exists(targ.file))
        buffer = scan(targ.file, what = "", sep = "\n",
                      blank.lines.skip = FALSE)
    else
        buffer = default_head
    top.end = grep("^<!--", buffer) # Look for 1st comment line
    if (length(top.end) >= 1)
        buffer = c(buffer[seq_len(top.end[1])], "")
    else
        buffer = c(buffer, "<!-- end of header -->", "")
    idx = vi_list2text(tree)
    indent = as.integer(substring(idx, 1, 1)) + 1
    prefix = sapply(0:7, function(i) paste(c(rep("    ", i), "  * "), collapse = ""))
    idx = paste0(prefix[indent], substring(idx, 2))
    buffer = c(buffer, idx)
    writeLines(text = buffer, con = targ.file)
    message("Vignette index file ", targ.file," is completed.")
    invisible()
}

# Heading of vignette file if none provided
default_head = c("---", "title: \"Index of vignette topics\"",
                 "output: rmarkdown::html_vignette", "vignette: >",
                 "  %\\VignetteIndexEntry{Index of vignette topics}",
                 "  %\\VignetteEngine{knitr::rmarkdown}",
                 "  %\\VignetteEncoding{UTF-8}", "---",
                 "**Note:** Links take you to the beginnings of sections or subsections ",
                 "where these topics occur.",
                 "<!-- End of header. Do not delete this comment -->")


# Create a new vignette-index entry
#
# Create a new object of class \code{vientry}, which has three components:
# \code{text}, \code{link}, and \code{children}
#
# @param text Character value. The text to display (may include markdown formatting)
# @param link Character value. The HTML link for this entry; use \code{NULL} if
#   it is just a heading for subsequent child entries
# @param children A \code{\link{list}} of other \code{vientry} objects
vientry = function(text, link = character(0), children = list()) {
    entry = list(text = text, link = link, children = children)
    ### we have no methods, so we won't give it a class
    ### class(entry) = "vientry"
    entry
}

# Make a key for storing an index entry. Typically, we use the entry
#   itself, but strip markdown styles like `text`, *text*, and **text**
# But we can manually set a key using text@key
# Note the key is used for sorting entries
# Returns a single space if otherwise it would be character(0)
make_key = function(text) {
    text = trimws(text)
    key = if (length(grep("@", text)) > 0)
        substring(text, 1 + regexpr("@", text))
    else
        tolower(gsub("`||\\*", "", text))
    ifelse(nchar(key) > 0, key, " ")
}

# insert an index entry use this like:
# main = insert_entry(main, tl, link)
insert_entry = function(parent, text_list, link) {
    key = make_key(text_list[1])
    txt = strsplit(text_list[1], "@")[[1]][1] # strip any "@key" portion

    if(length(text_list) == 1) {
        vi = vientry(txt, link)
        if (key %in% names(parent)) { # add this link
            parent[[key]]$link = c(parent[[key]]$link, link)
        }
        else { #add this new entry
            parent[[key]] = vi
        }
    }
    else {
        if (!(key %in% names(parent))) { # create new entry if not present
            parent[[key]] = vientry(text_list[1])
        }
        # in all cases, add to children of this entry
        parent[[key]]$children = insert_entry(parent[[key]]$children, text_list[-1], link)
    }
    # return the updated tree
    parent
}

# convert a list of vientries to a string vector
# 1st character of each is number of levels of indentation
# thus text to show is in substring(result, 2)
# and indent values are as.integer(substring(result, 1, 1))
vi_list2text = function(vi, indent = 0) {
    stg = character(0)
    vi = vi[order(names(vi))]
    for (ent in vi) {
        if (length(ent$link) == 0)
            stg = c(stg, paste0(indent, ent$text))
        else if (length(ent$link) == 1)
            stg = c(stg, paste0(indent, "[", ent$text, "](", ent$link, ")"))
        else {
            nlks = paste0("[", seq_along(ent$link), "](", ent$link, ")")
            links = paste0(indent, ent$text, ": ", paste(nlks, collapse = ", "))
            stg = c(stg, links)
        }
        if(length(ent$children) > 0)
            stg = c(stg, vi_list2text(ent$children, indent + 1))
    }
    stg
}

# Find index entries in a buffer. These look like:
# <!-- @index entry1; entry2!subsentry2; ... -->
# (may continue over several lines)
find_vientries = function(buffer) {
    ancloc = grep("*\\{[ ]*#[:alnum:]*[ ]*\\}*", buffer) # anchor locations
    beg = grep("^<!-- @index", buffer) # beg of @index lines
    end = grep(" -->", buffer) # comment ends
    end = sapply(beg, function(.) end[end >= .][1]) # ends of index entries

    # extract anchors
    a = buffer[ancloc]
    a = substring(a, 1 + regexpr("*\\{", a))
    anchors = c("", trimws(gsub("\\}", "", a))) # append a blank starting value
    ancloc = c(1, ancloc)

    # extract index entries and associated anchors
    lapply(seq_along(beg), function(i) {
        ai = max(which(ancloc <= beg[i]))
        text = paste(buffer[beg[i]:end[i]], collapse = "; ")
        text = substring(text, 7 + regexpr("@index", text))
        text = gsub("-->", "", text)
        entries = trimws(strsplit(text, ";")[[1]])
        list(entries = entries, link = anchors[ai])
    })
}

