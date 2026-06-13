#!/usr/bin/env Rscript
# Build the cssr package + website from the litr source (index.Rmd).
# Run from the cssr-project root:  Rscript build.R
#
# Regenerates the cssr/ package and the docs/ pkgdown site (via the
# add_pkgdown() chunk in index.Rmd), builds the litr "create" book to a
# temporary _book/, then republishes it into docs/create/ so the published
# site (served by GitHub Pages from docs/) stays in sync, and removes _book/.
#
# The copy step is the piece litr leaves manual (see litr's own create-litr
# book); without it, docs/create/ goes stale while the package keeps changing.
# See .workflow/ for the dev workflow; litr: https://jacobbien.github.io/litr-project/

litr::render("index.Rmd", minimal_eval = FALSE)

# Guard: only touch the published book if a fresh one was actually built, so a
# failed/empty render can't destroy docs/create before the copy would error.
stopifnot("litr::render did not produce _book/" = fs::dir_exists("_book"))

if (fs::dir_exists("docs/create")) fs::dir_delete("docs/create")
fs::dir_copy("_book", "docs/create")
fs::dir_delete("_book")
