# Resubmission

This resubmission addresses the six requests in Leonore Hochhauser's review.

1. DESCRIPTION now starts "Prepares and analyses large Danish administrative
   and health registry datasets." The phrase "tools for" has been removed.
2. Added return documentation to all 12 help files identified by the audit:
   createProject, innerMedicinMacro, kommuneRegion, matchReport, medicinMacro,
   poissonregression, poissonRisk, rDateToSas, simAdmissionData,
   simPrescriptionData, standardize.prodlim, and xrecepter. The documentation
   describes output classes, contents, and interpretation, or the lack of a
   return value. All 40 exported functions have a nonempty value section.
   Changes were made in the roxygen source, including the C++ source for
   innerMedicinMacro, and the Rcpp interface and Rd files were regenerated.
   A second audit corrected the numeric date origin for innerMedicinMacro:
   it preserves the input scale, which is days since 1995-01-01 when called
   by medicinMacro. It also corrected the documented return classes of
   medicinMacro, getAdmLimits, and dsr, the columns returned by averageIncome,
   and the descriptions of condition searches and standardization outputs.
   Rcpp and roxygen generation was run twice and verified to be reproducible.
3. Short executable examples are unwrapped. Examples using suggested
   packages are guarded by requireNamespace(); unnecessary optional imports
   were removed. Local-path examples for createProject, listRawdata, and
   view_parquet now use temporary fixtures. The previously disabled
   standardize.rate example now compares two groups and drops unused levels.
   Only importSAS, scanRawdata, and xrecepter retain dontrun blocks: they
   require proprietary SAS software and/or private registry data or an
   external SAS macro, as explained directly in the examples.
4. Information is returned in objects, signalled through message()/warning(),
   or printed only under a user-controlled flag. matchReport returns named
   tables and accepts verbose = FALSE; scanRawdata respects verbose;
   view_parquet(..., quiet = TRUE) suppresses both messages and data display.
   Admission errors use a warning and a returned error column.
   All active Rcout statements in innerMedicinMacro.cpp are conditional on
   verbose. Its help now documents the switch explicitly, and regression
   tests verify silence with FALSE and identical results with TRUE.
5. Project creation requires an explicitly supplied destination. SAS scratch
   files default to unique subdirectories of tempdir(), rather than getwd().
   Each call cleans up its own files, including on errors; old directories
   belonging to other calls are no longer removed. Writing examples and
   tests use temporary locations and clean up their fixtures. Vignette prose
   demonstrates explicit paths and temporary output files.
6. Working-directory restoration is registered with on.exit() immediately
   after saving the original directory, before any change. Cleanup handlers
   are additive and also restore the directory before deleting scratch files.
   No package function changes options() or par(). Regression tests cover
   restoration and cleanup on successful imports and on early and late errors.

# Validation

Tested on Ubuntu 24.04.5 LTS, x86_64, with R 4.6.1 on 2026-10-26.
All declared Imports, Suggests, and LinkingTo packages were available.

```
R CMD build /home/ctp/github/heaven
R CMD check --as-cran --run-donttest --timings heaven_2026.10.26.tar.gz
```

Result: 0 errors, 0 warnings, 3 notes.

- Examples, all four test scripts, vignette creation and rebuilding, and the
  PDF manual passed. Every runnable help-topic example took less than five
  seconds; the slowest was medicinMacro at 0.863 seconds elapsed.
- The new compliance tests cover explicit project destinations, returned
  matching tables, quiet diagnostics, SAS scans, temporary storage,
  directory restoration and cleanup, and quiet Parquet inspection. SAS
  execution is simulated through the existing sas.runner argument, so these
  tests do not require a SAS installation. Actual SAS execution and remote
  server operation were not tested.
- An archive comparison confirmed that all 105 R, C++, help, and test files
  match the final working files. The archive contains no object files or
  shared libraries. The source archive is approximately 1.05 MiB.
- git diff --check passed.

The remaining notes are:

1. "New submission": the package has not yet been accepted by CRAN.
2. The non-portable flag -mno-omit-leaf-frame-pointer is present in this
   machine's R CMD config CXXFLAGS; the package does not set this flag.
3. HTML manual validation was skipped because HTML Tidy is not installed.
   Rd validation and PDF manual creation passed.

The complete check log is saved locally as cran-check.log, which is excluded
from the source package along with this file.

# User-visible changes

- importSAS() and xrecepter() use temporary storage by default; supply wd
  explicitly to choose another scratch parent directory.
- createProject() requires directory, and xrecepter() requires macro, rather
  than choosing a directory interactively or using a developer-specific path.
- matchReport() returns tables and counts matching success once per case.
- view_parquet(..., quiet = TRUE) also suppresses the data display.

The checked archive is ready for resubmission; it has not been uploaded.
