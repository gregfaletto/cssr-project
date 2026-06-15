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

# --- Canonical build & verification process (issue #22) -------------------
# build.R is the SOLE documentation generator for cssr. litr writes man pages
# in its own style ("% Please edit documentation in _main.Rmd") because the
# editable source is index.Rmd, NOT the generated cssr/R/*.R files.
#
# To verify the package after building, run:
#     devtools::check("cssr", args = "--as-cran", document = FALSE)
# Do NOT run devtools::document() or plain devtools::check() on cssr/ -- they
# rewrite every man-page header to "R/<fn>.R" and produce ~62 spurious diffs.
# --------------------------------------------------------------------------

# litr 0.9.3 aborts the render if cssr/ already exists with a stale LitrId
# (reproduces even on a clean main). cssr/ is fully generated and never
# hand-edited, so delete it first for a clean, deterministic regenerate.
if (fs::dir_exists("cssr")) fs::dir_delete("cssr")

litr::render("index.Rmd", minimal_eval = FALSE)

# Guard: only touch the published book if a fresh one was actually built, so a
# failed/empty render can't destroy docs/create before the copy would error.
stopifnot("litr::render did not produce _book/" = fs::dir_exists("_book"))

if (fs::dir_exists("docs/create")) fs::dir_delete("docs/create")
fs::dir_copy("_book", "docs/create")
fs::dir_delete("_book")
