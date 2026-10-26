library(testthat)
library(heaven)
library(data.table)

test_that("every exported function documents its return value", {
    topics <- tools::Rd_db("heaven")
    tags <- function(topic, tag) {
        nodes <- Filter(function(node) identical(attr(node, "Rd_tag"), tag), topic)
        vapply(nodes, function(node) paste(unlist(node), collapse = ""), character(1))
    }
    documented <- unlist(lapply(topics, function(topic) {
        value <- tags(topic, "\\value")
        if (any(nzchar(trimws(value)))) tags(topic, "\\alias")
    }), use.names = FALSE)
    exports <- getNamespaceExports("heaven")
    functions <- exports[vapply(exports, function(name)
        is.function(getExportedValue("heaven", name)), logical(1))]
    expect_setequal(intersect(functions, documented), functions)
})

test_that("the C++ medicine macro returns intervals and supports quiet output", {
    prescriptions <- data.frame(pnr = c(1, 1, 2), eksd = c(0, 10, 0),
                                strnum = 1, packsize = 5, apk = 1)
    admissions <- data.frame(pnr = c(1, 2), inddto = -100, uddto = -99)
    doses <- list(value = 1, min = 1, max = 1, def = 1)
    index <- data.frame(id.start = c(0, 2), id.end = c(1, 2),
                        id.start.adm = c(0, 1), id.end.adm = c(0, 1))
    run <- function(verbose) innerMedicinMacro(prescriptions, admissions, doses,
                                              c(1, 2), index, 2, 0, verbose)
    expect_output(quiet <- run(FALSE), NA)
    expect_message(run(FALSE), NA)
    expect_output(loud <- run(TRUE), "subject:")
    expect_identical(loud, quiet)
    expect_type(quiet, "list")
    expect_length(quiet, 2)
    first <- quiet[[1]][order(quiet[[1]]$B), ]
    rownames(first) <- NULL
    expect_equal(first, data.frame(pnr = c(1, 1, 1), X = c(1, 0, 1),
                                   B = c(0, 5, 10), E = c(5, 10, 15)))
    expect_equal(quiet[[2]], data.frame(pnr = 2, X = 1, B = 0, E = 5))

    # Hospital stays extend the supply while preserving the input date origin.
    admissions <- data.frame(pnr = 1, inddto = 2, uddto = 4)
    index <- data.frame(id.start = 0, id.end = 0, id.start.adm = 0, id.end.adm = 0)
    result <- innerMedicinMacro(prescriptions[1, ], admissions, doses, 1,
                               index, 2, 0, FALSE)
    expect_equal(result[[1]], data.frame(pnr = 1, X = 1, B = 0, E = 7))
})

test_that("project creation requires a destination and uses that destination", {
    expect_error(createProject(), "supply a project directory")
    project <- tempfile("heaven-project-test-")
    on.exit(unlink(project, recursive = TRUE), add = TRUE)
    value <- suppressMessages(createProject(project))
    expect_null(value)
    expect_true(file.exists(file.path(project, "README.txt")))
    expect_true(file.exists(file.path(project, "R-code", "template-R-code.R")))
    expect_true(dir.exists(file.path(project, "data")))
})

test_that("matching reports return correct case counts and can be silent", {
    matched <- data.table(case.id = c(1, 1, 1, 2, 2, 3),
                          subject = c(1, 10, 11, 2, 10, 3),
                          outcome = c(1, 0, 0, 1, 0, 1))
    attr(matched, "id") <- "subject"
    attr(matched, "event") <- "outcome"
    expect_output(report <- matchReport(matched, verbose = FALSE), NA)
    expect_identical(as.integer(report$matching.success), c(1L, 1L, 1L))
    expect_identical(dimnames(report$matching.success)[[1]], c("0", "1", "2"))
    expect_identical(as.integer(report$control.reuse), c(1L, 1L))
    expect_identical(dimnames(report$control.reuse)[[1]], c("1", "2"))
    matched[, n.controls := c(2, 2, 2, 1, 1, 0)]
    expect_identical(matchReport(matched, verbose = FALSE), report)
})

test_that("admission error diagnostics use suppressible conditions", {
    admissions <- data.table(pnr = 1:2, inddto = as.Date(c("2000-01-01", "2000-02-02")),
                             uddto = as.Date(c("2000-01-02", "2000-02-01")))
    expect_warning(getAdmLimits(admissions), "inspect the returned error column")
    expect_output(result <- suppressWarnings(getAdmLimits(admissions)), NA)
    expect_identical(result$error, c(FALSE, TRUE))
})

test_that("SAS scans return data without mandatory console output", {
    rawdir <- tempfile("heaven-scan-test-")
    dir.create(rawdir)
    on.exit(unlink(rawdir, recursive = TRUE), add = TRUE)
    expect_identical(scanRawdata(rawdir), setNames(vector("list", 0), character()))
    file.create(file.path(rawdir, "pop.sas7bdat"))
    local_mocked_bindings(importSAS = function(...) data.table(variable = "pnr"),
                          .package = "heaven")
    expect_output(result <- scanRawdata(rawdir, which = "pop"), NA)
    expect_named(result, "pop.sas7bdat")
    expect_s3_class(result[[1]], "data.table")
})

test_that("SAS import failures restore the directory and remove scratch files", {
    input <- tempfile(fileext = ".sas7bdat")
    file.create(input)
    on.exit(unlink(input), add = TRUE)
    oldwd <- getwd()
    before <- list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE)
    runner <- function(command) {
        expect_identical(dirname(getwd()), normalizePath(tempdir()))
        0L
    }
    expect_error(importSAS(input, sas.program = "unused", sas.runner = runner),
                 "did not produce the expected output")
    expect_identical(getwd(), oldwd)
    expect_identical(list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE), before)

    # An error before SAS starts must also clean up an explicitly chosen directory.
    scratch <- tempfile("heaven-sas-parent-")
    dir.create(scratch)
    on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
    occupied <- file.path(scratch, "already.csv")
    file.create(occupied)
    expect_error(importSAS(input, wd = scratch, savefile = "already.csv"), "File exists")
    expect_identical(getwd(), oldwd)
    expect_identical(list.files(scratch), "already.csv")
})

test_that("SAS macro calls use temporary storage and restore it after errors", {
    macro <- tempfile(fileext = ".sas")
    file.create(macro)
    on.exit(unlink(macro), add = TRUE)
    oldwd <- getwd()
    before <- list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE)
    runner <- function(command) {
        expect_identical(dirname(getwd()), normalizePath(tempdir()))
        0L
    }
    drugdb <- data.table(pnr = 1, eksd = as.Date("2000-01-01"))
    adm <- data.table(pnr = 1, inddto = as.Date("2000-01-01"), uddto = as.Date("2000-01-02"))
    result <- xrecepter(drugdb, adm, atc = "A", value = 1:4, min = 1:4,
                       max = 1:4, def = 1:4, maxdepot = 1, macro = macro,
                       sas.program = "unused", sas.runner = runner, verbose = FALSE)
    expect_null(result)
    expect_identical(getwd(), oldwd)
    expect_identical(list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE), before)

    # Writing a list column as CSV fails after entering the scratch directory.
    bad <- data.table(pnr = 1, bad = list(new.env()))
    expect_error(xrecepter(bad, adm, atc = "A", value = 1:4, min = 1:4,
                          max = 1:4, def = 1:4, maxdepot = 1, macro = macro,
                          sas.program = "unused", sas.runner = runner, verbose = FALSE))
    expect_identical(getwd(), oldwd)
    expect_identical(list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE), before)
})

test_that("successful SAS imports preserve session state and clean scratch files", {
    input <- tempfile(fileext = ".sas7bdat")
    file.create(input)
    on.exit(unlink(input), add = TRUE)
    oldwd <- getwd()
    oldopts <- options()
    before <- list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE)
    runner <- function(command) {
        expect_identical(dirname(getwd()), normalizePath(tempdir()))
        if (grepl("tmpproccont.sas", command, fixed = TRUE)) {
            fwrite(data.table(Variable = "pnr", Type = "Char", Format = "", Informat = ""),
                   "sasimport_internal_tmpproccontout.csv")
        } else {
            fwrite(data.table(pnr = c("001", "002")), "sasimport_internal_tmpout.csv")
        }
        0L
    }
    expect_output(result <- importSAS(input, sas.program = "unused", sas.runner = runner), NA)
    expect_s3_class(result, "data.table")
    expect_identical(result$pnr, c("001", "002"))
    expect_identical(getwd(), oldwd)
    expect_identical(options(), oldopts)
    expect_identical(list.files(tempdir(), pattern = "^heaven_tempSASfiles", full.names = TRUE), before)
})

test_that("Parquet inspection can return data without printing", {
    skip_if_not_installed("arrow")
    skip_if_not_installed("dplyr")
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    arrow::write_parquet(data.frame(id = 1:3), path)
    for (action in c("head", "str")) {
        expect_output(result <- view_parquet(path, action = action, quiet = TRUE), NA)
        expect_message(view_parquet(path, action = action, quiet = TRUE), NA)
        expect_s3_class(result, "tbl_df")
        expect_identical(result$id, 1:3)
    }
})
