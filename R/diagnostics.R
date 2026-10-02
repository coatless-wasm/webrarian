# Package Validation and Diagnostics

# ============================================================================
# Package Availability Checking
# ============================================================================

#' Check inventory (package availability in webR repositories)
#'
#' Checks that packages exist in the webR package repository and in the
#' collection's extra repositories (`packages.repos`), the same ones [bind()]
#' downloads from. Known limitations of some packages are added as notes, but
#' they never change whether a package is available.
#'
#' @param packages Character vector of package names. If `NULL`, uses packages
#'   from the project configuration.
#' @param path Project path. Used to read config if `packages` is `NULL`, and
#'   for the collection's extra repositories.
#'
#' @return A data frame with columns:
#'   - `package`: Package name
#'   - `available`: `TRUE` or `FALSE`, or `NA` when the package was not found and
#'     a repository could not be reached
#'   - `version`: Package version if available, `NA` otherwise
#'   - `note`: Known limitations, if any
#'   - `repository`: The repository that has the package, `NA` otherwise
#'   - `cran_version`: The version on CRAN, `NA` when CRAN lacks it or cannot be
#'     reached (the `webrarian.cran_repo` option names another repository, or
#'     `FALSE` to skip the comparison)
#'   - `local_version`: The version installed in this R session, `NA` if none
#'   - `drift`: `"behind"`, `"current"` or `"ahead"`, comparing the WebAssembly
#'     version with CRAN's
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Queries the webR package repositories; without a network connection the
#' # availability is NA. The package index is cached under tempdir() here.
#' old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
#' Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "webrarian-example-cache"))
#'
#' check_inventory(c("dplyr", "ggplot2"))
#'
#' # Check every pre-built package a collection has configured
#' check_inventory(path = collection_example("data-analysis"))
#'
#' if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old)
#' }
check_inventory <- function(packages = NULL, path = ".") {
  config <- if (!is.null(collection_root(path))) collection_settings(path) else NULL

  if (is.null(packages)) {
    packages <- unlist(config$packages$prebuilt)
    if (length(packages) == 0) {
      cli::cli_alert_info("No packages to check")
      return(data.frame(
        package = character(),
        available = logical(),
        version = character(),
        note = character(),
        repository = character(),
        cran_version = character(),
        local_version = character(),
        drift = character(),
        stringsAsFactors = FALSE
      ))
    }
  }
  packages <- as.character(packages)

  r_version <- get_r_version_for_webr(config$webr$version %||% webr_version_default())
  repos <- unique(c(default_repo_url(), as.character(unlist(config$packages$repos))))

  cli::cli_alert_info(
    "Checking {length(packages)} package{?s} against {length(repos)} repositor{?y/ies}..."
  )
  index <- fetch_webr_packages_index(repos, r_version)
  problematic <- get_problematic_packages()

  if (is.null(index)) {
    cli::cli_warn(c(
      "Could not reach any package repository, so availability is unknown ({.code NA}).",
      "i" = "Check your connection and {.field packages.repos}."
    ))
    notes <- vapply(
      packages,
      function(p) check_single_package(p, NULL, problematic)$note,
      character(1)
    )
    return(data.frame(
      package = packages,
      available = NA,
      version = NA_character_,
      note = unname(notes),
      repository = NA_character_,
      cran_version = NA_character_,
      local_version = unname(local_package_versions(packages)),
      drift = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(packages, check_single_package, index = index, problematic = problematic)
  result_df <- data.frame(
    package = vapply(results, `[[`, character(1), "package"),
    available = vapply(results, `[[`, logical(1), "available"),
    version = vapply(results, `[[`, character(1), "version"),
    note = vapply(results, `[[`, character(1), "note"),
    repository = vapply(results, `[[`, character(1), "repository"),
    stringsAsFactors = FALSE
  )
  drift <- package_drift(result_df$package, result_df$version)
  result_df$cran_version <- drift$cran_version
  result_df$local_version <- drift$local_version
  result_df$drift <- drift$drift
  behind <- result_df$package[result_df$drift %in% "behind"]
  if (length(behind) > 0L) {
    cli::cli_alert_info(
      "{length(behind)} package{?s} {?is/are} older in the WebAssembly repository than on CRAN: {.pkg {behind}} (see the {.field drift} column)"
    )
  }

  unavailable <- result_df$package[result_df$available %in% FALSE]
  if (length(unavailable) > 0) {
    cli::cli_alert_warning("{length(unavailable)} package{?s} not available: {.pkg {unavailable}}")
  }
  unknown <- result_df$package[is.na(result_df$available)]
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "Availability of {.pkg {unknown}} is unknown ({.code NA}): {.url {index$failed}} could not be reached.",
      "i" = "Check your connection and {.field packages.repos}."
    ))
  }
  n_available <- sum(result_df$available %in% TRUE)
  if (n_available > 0) {
    cli::cli_alert_success("{n_available} package{?s} available")
  }
  if (any(!is.na(result_df$note))) {
    cli::cli_alert_info("Some packages have notes - see the {.field note} column")
  }
  result_df
}

#' The merged package index of the given repositories, cached for an hour
#'
#' @return A `fetch_repo_index()` result, or NULL when no repository answered
#'   and there is no cached copy. Cached only when every repository answered.
#' @noRd
fetch_webr_packages_index <- function(repos, r_version, cache_duration = 3600) {
  cache_dir <- fs::path(webr_cache_path(), "package-index")
  key <- substr(rlang::hash(list(sort(unique(as.character(repos))), r_version)), 1L, 12L)
  cache_file <- fs::path(cache_dir, sprintf("index-%s-%s.rds", r_version, key))

  if (fs::file_exists(cache_file)) {
    age <- as.numeric(difftime(
      Sys.time(),
      fs::file_info(cache_file)$modification_time,
      units = "secs"
    ))
    if (age < cache_duration) {
      return(readRDS(cache_file))
    }
  }

  index <- fetch_repo_index(repos, r_version)
  if (!is.null(index$packages)) {
    # An index missing an unreachable repository would report that
    # repository's packages unavailable for an hour, so only cache it when
    # every repository answered.
    if (length(index$failed) == 0L) {
      ensure_dir(cache_dir)
      saveRDS(index, cache_file)
    }
    return(index)
  }
  if (fs::file_exists(cache_file)) {
    cli::cli_alert_info("Using a cached package index (may be outdated)")
    return(readRDS(cache_file))
  }
  NULL
}

#' Check a single package's availability
#'
#' The problem list only adds a note: packages it names may still install
#' (V8, processx and sys all do), so it must not decide availability.
#' @noRd
check_single_package <- function(pkg_name, index, problematic) {
  result <- list(
    package = pkg_name,
    available = FALSE,
    version = NA_character_,
    note = NA_character_,
    repository = NA_character_
  )

  if (!is.null(index$packages)) {
    hit <- match(pkg_name, index$packages[, "Package"])
    if (!is.na(hit)) {
      result$available <- TRUE
      result$version <- unname(index$packages[hit, "Version"])
      result$repository <- sub("/bin/emscripten/contrib/.*$", "", index$source[[pkg_name]])
    }
  }
  if (!isTRUE(result$available) && length(index$failed) > 0L) {
    result$available <- NA
  }

  if (!is.null(problematic)) {
    prob_idx <- which(problematic$name == pkg_name)
    if (length(prob_idx) > 0) {
      entry <- problematic[prob_idx[[1]], ]
      note <- entry$reason
      if (!is.na(entry$alternative) && nzchar(entry$alternative)) {
        note <- paste0(note, ". Alternative: ", entry$alternative)
      }
      result$note <- note
    }
  }

  result
}

#' Get problematic packages database
#' @noRd
get_problematic_packages <- function() {
  json_file <- system.file(
    "extdata",
    "problematic-packages.json",
    package = "webrarian"
  )

  if (!fs::file_exists(json_file)) {
    return(NULL)
  }

  tryCatch(
    {
      data <- jsonlite::read_json(json_file, simplifyVector = TRUE)
      data$packages
    },
    error = function(e) {
      NULL
    }
  )
}

# ============================================================================
# Comprehensive Diagnostics
# ============================================================================

#' Diagnose collection
#'
#' Checks network access, the tools webrarian uses, the collection's
#' configuration and its packages. Every problem is collected and reported, and
#' nothing stops the checks early.
#'
#' @param path A collection, or any directory inside one. Outside a collection
#'   only the network and tool checks run.
#'
#' @return Invisibly, a list with `ok` (`FALSE` when something would make
#'   [bind()] fail, such as an invalid configuration, local or GitHub packages without
#'   a running Docker, or, for a build that bundles its packages
#'   (`build.bundle-engine`, the default, or `build.offline`), a configured
#'   pre-built package that no repository has), `problems` (a data frame with
#'   one row per problem found, warnings included, where `severity` is `"error"` or
#'   `"warning"`, `area` is `"network"`, `"config"`, `"tools"` or
#'   `"packages"`, and `message` says what is wrong), `overall` (`"ok"`,
#'   `"warning"` or `"error"`), and the details of each check in `network`,
#'   `tools`, `config` and `packages`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Probes the webR repository and GitHub; without a network connection those
#' # checks report the failure and the others still run.
#' path <- collection_example("basic")
#' result <- diagnose_collection(path)
#' result$ok
#' result$problems[result$problems$severity == "error", ]
#' unlink(path, recursive = TRUE)
#' }
diagnose_collection <- function(path = ".") {
  cli::cli_h1("Webrarian Diagnostics")

  results <- list(
    ok = TRUE,
    problems = data.frame(
      severity = character(),
      area = character(),
      message = character(),
      stringsAsFactors = FALSE
    ),
    overall = "ok",
    network = list(),
    tools = list(),
    config = list(),
    packages = list()
  )
  rank <- c(ok = 0L, warning = 1L, error = 2L)
  report <- function(level, area, problem) {
    results$problems <<- rbind(
      results$problems,
      data.frame(severity = level, area = area, message = problem, stringsAsFactors = FALSE)
    )
    if (rank[[level]] > rank[[results$overall]]) results$overall <<- level
  }

  cli::cli_h2("Network Connectivity")
  results$network <- diagnose_network()
  for (name in names(results$network)) {
    if (!isTRUE(results$network[[name]]$ok)) {
      report(
        "warning",
        "network",
        sprintf(
          "%s could not be reached: %s",
          name,
          results$network[[name]]$error %||% "no answer"
        )
      )
    }
  }

  cli::cli_h2("Tool Availability")
  results$tools <- diagnose_tools()

  root <- collection_root(path)
  if (!is.null(root)) {
    cli::cli_h2("Project Configuration")
    results$config <- diagnose_config(root)
    if (!isTRUE(results$config$valid)) {
      for (issue in results$config$issues) {
        report("error", "config", issue)
      }
      cli::cli_alert_info("Skipping package checks until the configuration is fixed.")
    } else {
      for (issue in results$config$issues) {
        report("warning", "config", issue)
      }
      config <- suppressWarnings(collection_settings(root))

      if (
        (length(config$packages$github) > 0 || length(config$packages$local) > 0) &&
          !identical(results$tools$docker$status, "running")
      ) {
        problem <- "This collection compiles local/GitHub packages, which needs a running Docker."
        cli::cli_alert_danger(problem)
        report("error", "tools", problem)
      }

      cli::cli_h2("Package Availability")
      prebuilt <- unlist(config$packages$prebuilt)
      if (length(prebuilt) > 0) {
        pkg_check <- tryCatch(
          withCallingHandlers(
            check_inventory(prebuilt, root),
            warning = function(w) {
              cli::cli_alert_warning(conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) e
        )
        if (inherits(pkg_check, "error")) {
          cli::cli_alert_danger("Package check failed: {conditionMessage(pkg_check)}")
          results$packages <- list(checked = 0, error = conditionMessage(pkg_check))
          report("warning", "packages", paste("Package check failed:", conditionMessage(pkg_check)))
        } else {
          results$packages <- list(
            checked = nrow(pkg_check),
            available = sum(pkg_check$available %in% TRUE),
            unavailable = sum(pkg_check$available %in% FALSE),
            unknown = sum(is.na(pkg_check$available)),
            details = pkg_check
          )
          missing <- pkg_check$package[pkg_check$available %in% FALSE]
          unknown <- pkg_check$package[is.na(pkg_check$available)]
          if (length(missing) > 0) {
            # A build that bundles its packages (build.bundle-engine, the
            # default, or build.offline) downloads every configured prebuilt
            # package and aborts on one no repository has (bind()'s `bundle`;
            # configure_packages(): required = prebuilt). Otherwise the page
            # installs it, and only that fails.
            bundles <- isTRUE(config$build$offline) || !isFALSE(config$build$bundle_engine)
            report(
              if (bundles) "error" else "warning",
              "packages",
              paste("Not available for webR:", paste(missing, collapse = ", "))
            )
          }
          if (length(unknown) > 0) {
            report(
              "warning",
              "packages",
              paste(
                "Availability unknown (no repository answered):",
                paste(unknown, collapse = ", ")
              )
            )
          }
        }
      } else {
        cli::cli_alert_info("No pre-built packages configured")
        results$packages <- list(checked = 0, available = 0, unavailable = 0, unknown = 0)
      }
    }
  } else {
    cli::cli_alert_info("Not a webrarian collection - skipping project-specific checks")
  }

  cli::cli_h2("Cache Information")
  webr_cache_info()

  results$ok <- !identical(results$overall, "error")
  cli::cli_h2("Summary")
  switch(
    results$overall,
    ok = cli::cli_alert_success("All checks passed"),
    warning = cli::cli_alert_warning("Some checks have warnings - review above"),
    error = cli::cli_alert_danger("Some checks failed - review above")
  )

  invisible(results)
}

#' Diagnose network connectivity
#' @noRd
diagnose_network <- function(verbose = FALSE) {
  endpoints <- list(
    list(
      name = "webR Package Repository",
      url = "https://repo.r-wasm.org",
      critical = TRUE
    ),
    list(
      name = "GitHub Releases",
      url = "https://github.com",
      critical = TRUE
    ),
    list(
      name = "webR CDN",
      url = "https://webr.r-wasm.org",
      critical = FALSE
    )
  )

  results <- list()

  for (endpoint in endpoints) {
    result <- check_endpoint(endpoint$url, endpoint$name)

    if (result$ok) {
      cli::cli_alert_success("{endpoint$name}: OK")
    } else {
      if (endpoint$critical) {
        cli::cli_alert_danger("{endpoint$name}: Failed - {result$error}")
      } else {
        cli::cli_alert_warning("{endpoint$name}: Failed - {result$error}")
      }
    }

    if (verbose && result$ok) {
      cli::cli_text("  Response time: {result$time}ms")
    }

    results[[endpoint$name]] <- result
  }

  results
}

#' Check a single endpoint
#' @noRd
check_endpoint <- function(url, name) {
  start_time <- Sys.time()

  tryCatch(
    {
      req <- httr2::request(url)
      req <- httr2::req_timeout(req, 10)
      req <- httr2::req_method(req, "HEAD")
      resp <- httr2::req_perform(req)

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000

      list(
        ok = httr2::resp_status(resp) < 400,
        status = httr2::resp_status(resp),
        time = round(elapsed),
        error = NULL
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        status = NA,
        time = NA,
        error = conditionMessage(e)
      )
    }
  )
}

#' Diagnose tool availability
#' @noRd
diagnose_tools <- function(verbose = FALSE) {
  results <- list()

  # Docker (required for compiling local/GitHub packages)
  docker <- docker_status()
  switch(
    docker,
    running = cli::cli_alert_success("Docker: running (for custom package compilation)"),
    stopped = cli::cli_alert_warning(
      "Docker: installed but not running (needed for local/GitHub packages)"
    ),
    missing = cli::cli_alert_info("Docker: not installed (needed for local/GitHub packages)")
  )
  results$docker <- list(available = identical(docker, "running"), status = docker)

  # httpuv
  httpuv_ok <- requireNamespace("httpuv", quietly = TRUE)
  if (httpuv_ok) {
    cli::cli_alert_success("httpuv: Available (for preview server)")
  } else {
    cli::cli_alert_warning("httpuv: Not installed (needed for preview)")
    cli::cli_text("  Install with: {.run install.packages(\"httpuv\")}")
  }
  results$httpuv <- list(available = httpuv_ok)

  results
}

#' Diagnose project configuration
#'
#' `valid` is FALSE for anything that would make bind() fail; warnings (an
#' unknown key, an include pattern that matches nothing) are `issues`.
#' @noRd
diagnose_config <- function(path, verbose = FALSE) {
  invalid <- function(message) {
    cli::cli_alert_danger("Configuration file: Invalid")
    cli::cli_text("  Error: {message}")
    list(valid = FALSE, issues = message)
  }

  warnings <- character()
  loaded <- tryCatch(
    withCallingHandlers(
      list(config = collection_settings(path), root = collection_root(path)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(loaded, "error")) {
    return(invalid(conditionMessage(loaded)))
  }
  config <- loaded$config
  root <- loaded$root
  issues <- warnings

  engine_problem <- tryCatch(
    {
      suppressWarnings(resolve_webr_version(config$webr$version))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(engine_problem)) {
    return(invalid(engine_problem))
  }

  output_problem <- tryCatch(
    {
      check_output_dir_replaceable(resolve_output_dir(root, config$build$output_dir))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(output_problem)) {
    return(invalid(output_problem))
  }

  # bind() aborts on an include pattern that points outside the collection
  # through select_collection_files(), so it is invalid here too.
  files <- tryCatch(
    resolve_file_patterns(
      config$files$include,
      config$files$exclude,
      root,
      output_dir = config$build$output_dir
    ),
    error = function(e) e
  )
  if (inherits(files, "error")) {
    return(invalid(conditionMessage(files)))
  }
  unmatched <- attr(files, "unmatched")
  if (length(unmatched) > 0) {
    issues <- c(issues, paste0("Include pattern `", unmatched, "` matches no files."))
  }
  repl_problem <- tryCatch(
    {
      resolve_repl_files(config, as.character(files))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(repl_problem)) {
    return(invalid(repl_problem))
  }

  if (length(issues) == 0) {
    cli::cli_alert_success("Configuration file: Valid")
  } else {
    cli::cli_alert_warning("Configuration file: {length(issues)} warning{?s}")
    for (issue in issues) {
      cli::cli_bullets(c("!" = "{issue}"))
    }
  }
  cli::cli_text("  webR version: {.val {config$webr$version}}")
  cli::cli_text("  Files to include: {length(files)}")
  prebuilt <- unlist(config$packages$prebuilt)
  if (length(prebuilt) > 0) {
    cli::cli_text("  Pre-built packages: {length(prebuilt)}")
  }

  list(valid = TRUE, issues = issues)
}
