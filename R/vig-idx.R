# Code for creating an index vignette

#' Generate a vignette topical index.
#'
#' Read indexing tags from a set of R-Markdown vignettes and create
#' a new vignette with topic names and links to the tagged locations.
#'
#' The \code{vigindex} function does everything in one call; in ordinary package
#' development, using it with the default areguments will index all of the
#' package's vignettes and create an index source file named
#' \code{"vignette-topics.Rmd"}. If the user wants more control over the
#' process, they may call (in order): \code{compile_vi},
#' \code{add_navigation} (optionally), and \code{write_vi}.
#'
#' If the target file does not exist, one is created.
#' The user may edit the target file once created. The function looks
#' for a line of the form \code{<div class="vigindex" ...>}.
#' Everything after this \code{div}
#' line will be replaced by a new set of indexing information each time
#' \code{vigindex} is run. Everything before the \code{div} line is retained.
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
#' @param navigation character value determining whether links/anchors should be
#'   added to assist in jumping to other places in the index. If \code{"none"},
#'   no navigation is added; if \code{"fourths"}, navigation points are placed
#'   at four roughly equally-spaced ponts in the index. If \code{"letters"},
#'   they are placed at the beginning of each new beginning letter. If
#'   \code{"auto"}, it is determined by the number of main entries.
#' @param ... additional arguments. In particular, extra arguments in
#'   \code{vigindex} are passed to \code{write_vi}.
#' @return \code{vigindex} and \code{write_vi} return nothing, but the contents
#'   of \code{target} are created or altered, and a message indicates when it is
#'   complete.
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
#' Entries on the same line are separated by \code{;}, and subentries are delineated
#' by \code{!}. Sorting is done without regard to capitalization or formatting,
#' but a different sorting key may be appended after an \code{@} symbol. Accordingly,
#' with the above examples, the first main entry will be \bold{Birds}
#' (in boldface and capitalized per its first appearance),
#' with a subentry for Owls, in turn with subentries for Barred, Great horned, Screech,
#' and others (last because of the \code{@zzz}). The next main entry will be
#' \code{print.owls}, and finally, Raptors.
#'
#' @section Links:
#' Links in the generated index will be to the latest anchor (of the
#' form \code{\{#anchor\}}) found
#' in the document, or to the top of the file if no anchors occur
#' before the index tag.
#'
#' See \code{vignette("example", package = "vigindex")} for examples and more
#' details.
#'
vigindex = function(dir = "vignettes",
                    vignettes = dir(dir, pattern = "*.Rmd"),
                    target = "vignette-topics.Rmd",
                    navigation = c("auto", "none", "fourths", "letters"),
                    ...) {
    tree = compile_vi(dir, vignettes, ...)
    tree = add_navigation(tree, navigation, ...)
    write_vi(tree, dir, target, ...)
}

#' @rdname vigindex
#' @export
#' @return \code{compile_vi} returns a tree structure with the compiled index.
#'   It consists of a named \code{list} of nodes. The names serve as sorting
#'   keys for the nodes. Each node is in turn a named list with three elements:
#'   \code{text} is a vector of character strings for the entry and any
#'   subentries. \code{link} is the character link to associate with the last
#'   element of \code{text}. And \code{children} is a named list of nodes of the
#'   same form as \code{tree}, containing any subsentries.
#'
compile_vi = function(dir = "vignettes",
                    vignettes = dir(dir, pattern = "*.Rmd")) {
    tree = list()
    for (vig in vignettes) {
        n.ent = 0
        cat(paste(vig, "..."))
        buffer = scan(paste(dir, vig, sep = "/"), what = "", sep = "\n", quiet = TRUE)
        entlist = find_vientries(buffer)
        ext = substring(vig, regexpr("\\.", vig))
        vig.html = sub(ext, ".html", vig)
        for (ent in entlist) {
            link = paste0(vig.html, ent$link)
            for (stg in ent$entries) {
                if (stg != "") {
                    ent = strsplit(gsub("\\\\!", "~ba~ng~", stg), "!")[[1]]
                    tree = insert_entry(tree, gsub("~ba~ng~", "!", ent), link)
                    n.ent = n.ent + 1
                }
            }
        }
        message(n.ent, " entries processed\n")
    }
    tree
}

#' @rdname vigindex
#' @return \code{add_navigation} returns an amended tree with entries for
#'     navigation links.
#' @param tree An index tree such as is returned by \code{compile_vi};
#'        see details under Value.
#' @export
add_navigation = function(tree, navigation = c("auto", "none", "fourths", "letters")) {
    navigation = match.arg(navigation)
    if (navigation == "auto") {
        len = length(tree)
        navigation = ifelse(len <=25, "none",
                            ifelse(len <= 125, "fourths", "letters"))
    }
    keys = sort(tolower(names(tree)))
    ltrs = unique(substring(keys, 1, 1))
    if (navigation == "fourths") {
        findprev = function(chr)
            max(1, which(ltrs == chr) - 1)
        beg = keys[1 + as.integer(length(keys)*c(0, .25, .5, .75))]
        beg = substring(beg, 1, 1)
        end = ltrs[c(sapply(beg[-1], findprev), length(ltrs))]
        begnav = paste0("[", toupper(beg), "--", toupper(end), "](#", beg, ")")
        beghead = paste0("\n\n### ", toupper(beg), "--", toupper(end), " {#", beg, "}\n\n")
        topnav = paste0("\n\n### Jump to: (",
                        paste(begnav, collapse = ") ("), ") {#topnav}\n\n@ ")
    }
    else if (navigation == "letters") {
        beg = ltrs[ltrs %in% letters]
        begnav = paste0("[", toupper(ltrs), "](#", ltrs, ")")
        beghead = paste0("\n\n### ", toupper(ltrs), " {#", ltrs, "}\n\n")
        topnav = paste("\n\n### Jump to: ",
                       paste(begnav, collapse = " "), " {#topnav}\n\n@ ")
    }
    if (navigation != "none") {
        gototop = "\n\n[Back to top](#topnav)"
        tree = insert_entry(tree, topnav)
        tree = insert_entry(tree, paste0(beghead[1], "@", beg[1]))
        for (i in (1 + seq_along(beghead[-1]))) {
            tree = insert_entry(tree, paste0(gototop, beghead[i], "@", beg[i]))
        }
        tree = insert_entry(tree, paste0(gototop, "@zzzzz"))
    }
    tree
}

#' @rdname vigindex
#' @param taglines character vector of lines that are added to the
#'   end of the index. If not supplied, it adds a promotional link for
#'   the \pkg{vigindex}  package.
#' @param style Additional text to place at the beginning of the index.
#'   If missing, no additional visible text is added, but a standard
#'   \code{<style> ... </style>} string is added that should make
#'   the formatting more attractive (e.g., no bullets).
#' @export
write_vi = function(tree, dir = getwd("vignettes"),
                    target = "vignette-topics.Rmd", taglines, ...) {
    targ.file = paste(dir, target, sep = "/")
    cat("\nPreparing index file ...\n")
    if (file.exists(targ.file))
        buffer = scan(targ.file, what = "", sep = "\n",
                      blank.lines.skip = FALSE, quiet = TRUE)
    else {
        pkgname = rev(strsplit(getwd(), "/")[[1]])[1]
        buffer = gsub("<pkgname>", pkgname, default_head)
    }
    top.end = grep("^<div class[ =\"']+vigindex", buffer) # Look for top of index
    if (length(top.end) >= 1)
        buffer = c(buffer[seq_len(top.end[1])], "")
    else
        buffer = c(buffer, "<div class=\"vigindex\">", "")
    idx = vi_list2text(tree)
    indent = as.integer(substring(idx, 1, 1)) + 1
    prefix = sapply(0:8, function(i) paste(c(rep("    ", i), "  * "), collapse = ""))
    prefix[10] = ""
    idx = paste0(prefix[indent], substring(idx, 2))
    if (missing(taglines))
        taglines = c("","","*Index generated by the [vigindex](https://github.com/rvlenth/vigindex) package.*")
    buffer = c(buffer, idx, taglines)
    writeLines(text = buffer, con = targ.file)
    message("Vignette index file ", targ.file," is completed.")
    invisible()
}


# Heading of vignette file if none provided
default_head = c("---", "title: \"Index of vignette topics\"",
     "author: \"<pkgname> package\"",
     "output: vigindex::html_vigindex",
     "vignette: >",
     "  %\\VignetteIndexEntry{Index of vignette topics}",
     "  %\\VignetteEngine{knitr::rmarkdown}",
     "  %\\VignetteEncoding{UTF-8}", "---",
     "<div class=\"vigindex\" id=\"Index follows this line; do not edit below\">")


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
    rtn = ifelse(nchar(key) > 0, key, " ")
}

# insert an index entry use this like:
# main = insert_entry(main, tl, link)
insert_entry = function(parent, text_list, link = list()) {
    txt = gsub("\\\\@", "~at~sign~", text_list[1])
    key = make_key(txt)
    txt = strsplit(txt, "@")[[1]][1] # strip any "@key" portion
    txt = gsub("~at~sign~", "@", txt)

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
# We reserve indent level 9 for navigation (entries that start with "\n")
vi_list2text = function(vi, indent = 0) {
    stg = character(0)
    vi = vi[order(tolower(names(vi)))]
    for (ent in vi) {
        if (length(ent$link) == 0) {
            if (startsWith(ent$text, "\n"))
                stg = c(stg, paste0("9", ent$text))
            else
                stg = c(stg, paste0(indent, ent$text))
        }
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
        text = gsub("\\\\;", "~semi~colon~", text)   # handle \;
        entries = trimws(strsplit(text, ";")[[1]])
        entries = gsub("~semi~colon~", ";", entries)
        list(entries = entries, link = anchors[ai])
    })
}

