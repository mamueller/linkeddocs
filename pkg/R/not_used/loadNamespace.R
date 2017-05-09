loadNamespace<- function (package, lib.loc = NULL, keep.source = getOption("keep.source.pkgs"), 
    partial = FALSE, versionCheck = NULL) 
{
    libpath <- attr(package, "LibPath")
    package <- as.character(package)[[1L]]
    loading <- dynGet("__NameSpacesLoading__", NULL)
    if (match(package, loading, 0L)) 
        stop("cyclic namespace dependency detected when loading ", 
            sQuote(package), ", already loading ", paste(sQuote(loading), 
                collapse = ", "), domain = NA)
    "__NameSpacesLoading__" <- c(package, loading)
    ns <- .Internal(getRegisteredNamespace(package))
    if (!is.null(ns)) {
        if (!is.null(zop <- versionCheck[["op"]]) && !is.null(zversion <- versionCheck[["version"]])) {
            current <- getNamespaceVersion(ns)
            if (!do.call(zop, list(as.numeric_version(current), 
                zversion))) 
                stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
                  sQuote(package), current, zop, zversion), domain = NA)
        }
        ns
    }
    else {
        runHook <- function(hookname, env, libname, pkgname) {
            if (!is.null(fun <- env[[hookname]])) {
                res <- tryCatch(fun(libname, pkgname), error = identity)
                if (inherits(res, "error")) {
                  stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", 
                    hookname, "loadNamespace", pkgname, deparse(conditionCall(res))[1L], 
                    conditionMessage(res)), call. = FALSE, domain = NA)
                }
            }
        }
        runUserHook <- function(pkgname, pkgpath) {
            hooks <- getHook(packageEvent(pkgname, "onLoad"))
            for (fun in hooks) try(fun(pkgname, pkgpath))
        }
        makeNamespace <- function(name, version = NULL, lib = NULL) {
            impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
            attr(impenv, "name") <- paste("imports", name, sep = ":")
            env <- new.env(parent = impenv, hash = TRUE)
            name <- as.character(as.name(name))
            version <- as.character(version)
            info <- new.env(hash = TRUE, parent = baseenv())
            env$.__NAMESPACE__. <- info
            info$spec <- c(name = name, version = version)
            setNamespaceInfo(env, "exports", new.env(hash = TRUE, 
                parent = baseenv()))
            dimpenv <- new.env(parent = baseenv(), hash = TRUE)
            attr(dimpenv, "name") <- paste("lazydata", name, 
                sep = ":")
            setNamespaceInfo(env, "lazydata", dimpenv)
            setNamespaceInfo(env, "imports", list(base = TRUE))
            setNamespaceInfo(env, "path", normalizePath(file.path(lib, 
                name), "/", TRUE))
            setNamespaceInfo(env, "dynlibs", NULL)
            setNamespaceInfo(env, "S3methods", matrix(NA_character_, 
                0L, 3L))
            env$.__S3MethodsTable__. <- new.env(hash = TRUE, 
                parent = baseenv())
            .Internal(registerNamespace(name, env))
            env
        }
        sealNamespace <- function(ns) {
            namespaceIsSealed <- function(ns) environmentIsLocked(ns)
            ns <- asNamespace(ns, base.OK = FALSE)
            if (namespaceIsSealed(ns)) 
                stop(gettextf("namespace %s is already sealed in 'loadNamespace'", 
                  sQuote(getNamespaceName(ns))), call. = FALSE, 
                  domain = NA)
            lockEnvironment(ns, TRUE)
            lockEnvironment(parent.env(ns), TRUE)
        }
        addNamespaceDynLibs <- function(ns, newlibs) {
            dynlibs <- .getNamespaceInfo(ns, "dynlibs")
            setNamespaceInfo(ns, "dynlibs", c(dynlibs, newlibs))
        }
        bindTranslations <- function(pkgname, pkgpath) {
            std <- c("compiler", "foreign", "grDevices", "graphics", 
                "grid", "methods", "parallel", "splines", "stats", 
                "stats4", "tcltk", "tools", "utils")
            popath <- if (pkgname %in% std) 
                .popath
            else file.path(pkgpath, "po")
            if (!file.exists(popath)) 
                return()
            bindtextdomain(pkgname, popath)
            bindtextdomain(paste("R", pkgname, sep = "-"), popath)
        }
        assignNativeRoutines <- function(dll, lib, env, nativeRoutines) {
            if (length(nativeRoutines) == 0L) 
                return(NULL)
            if (nativeRoutines$useRegistration) {
                fixes <- nativeRoutines$registrationFixes
                routines <- getDLLRegisteredRoutines.DLLInfo(dll, 
                  addNames = FALSE)
                lapply(routines, function(type) {
                  lapply(type, function(sym) {
                    varName <- paste0(fixes[1L], sym$name, fixes[2L])
                    if (exists(varName, envir = env)) 
                      warning(gettextf("failed to assign RegisteredNativeSymbol for %s to %s since %s is already defined in the %s namespace", 
                        sym$name, varName, varName, sQuote(package)), 
                        domain = NA, call. = FALSE)
                    else env[[varName]] <- sym
                  })
                })
            }
            symNames <- nativeRoutines$symbolNames
            if (length(symNames) == 0L) 
                return(NULL)
            symbols <- getNativeSymbolInfo(symNames, dll, unlist = FALSE, 
                withRegistrationInfo = TRUE)
            lapply(seq_along(symNames), function(i) {
                varName <- names(symNames)[i]
                origVarName <- symNames[i]
                if (exists(varName, envir = env)) 
                  if (origVarName != varName) 
                    warning(gettextf("failed to assign NativeSymbolInfo for %s to %s since %s is already defined in the %s namespace", 
                      origVarName, varName, varName, sQuote(package)), 
                      domain = NA, call. = FALSE)
                  else warning(gettextf("failed to assign NativeSymbolInfo for %s since %s is already defined in the %s namespace", 
                    origVarName, varName, sQuote(package)), domain = NA, 
                    call. = FALSE)
                else assign(varName, symbols[[origVarName]], 
                  envir = env)
            })
            symbols
        }
        pkgpath <- find.package(package, c(libpath, lib.loc), 
            quiet = TRUE)
        if (length(pkgpath) == 0L) 
            stop(gettextf("there is no package called %s", sQuote(package)), 
                domain = NA)
        bindTranslations(package, pkgpath)
        package.lib <- dirname(pkgpath)
        package <- basename(pkgpath)
        if (!packageHasNamespace(package, package.lib)) {
            hasNoNamespaceError <- function(package, package.lib, 
                call = NULL) {
                class <- c("hasNoNamespaceError", "error", "condition")
                msg <- gettextf("package %s does not have a namespace", 
                  sQuote(package))
                structure(list(message = msg, package = package, 
                  package.lib = package.lib, call = call), class = class)
            }
            stop(hasNoNamespaceError(package, package.lib))
        }
        nsInfoFilePath <- file.path(pkgpath, "Meta", "nsInfo.rds")
        nsInfo <- if (file.exists(nsInfoFilePath)) 
            readRDS(nsInfoFilePath)
        else parseNamespaceFile(package, package.lib, mustExist = FALSE)
        pkgInfoFP <- file.path(pkgpath, "Meta", "package.rds")
        if (file.exists(pkgInfoFP)) {
            pkgInfo <- readRDS(pkgInfoFP)
            version <- pkgInfo$DESCRIPTION["Version"]
            vI <- pkgInfo$Imports
            if (is.null(built <- pkgInfo$Built)) 
                stop(gettextf("package %s has not been installed properly\n", 
                  sQuote(basename(pkgpath))), call. = FALSE, 
                  domain = NA)
            R_version_built_under <- as.numeric_version(built$R)
            if (R_version_built_under < "3.0.0") 
                stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
                  sQuote(basename(pkgpath))), call. = FALSE, 
                  domain = NA)
            dependsMethods <- "methods" %in% names(pkgInfo$Depends)
            if (dependsMethods) 
                loadNamespace("methods")
            if (!is.null(zop <- versionCheck[["op"]]) && !is.null(zversion <- versionCheck[["version"]]) && 
                !do.call(zop, list(as.numeric_version(version), 
                  zversion))) 
                stop(gettextf("namespace %s %s is being loaded, but %s %s is required", 
                  sQuote(package), version, zop, zversion), domain = NA)
        }
        ns <- makeNamespace(package, version = version, lib = package.lib)
        on.exit(.Internal(unregisterNamespace(package)))
        for (i in nsInfo$imports) {
            if (is.character(i)) 
                namespaceImport(ns, loadNamespace(i, c(lib.loc, 
                  .libPaths()), versionCheck = vI[[i]]), from = package)
            else if (!is.null(i$except)) 
                namespaceImport(ns, loadNamespace(j <- i[[1L]], 
                  c(lib.loc, .libPaths()), versionCheck = vI[[j]]), 
                  from = package, except = i$except)
            else namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], 
                c(lib.loc, .libPaths()), versionCheck = vI[[j]]), 
                i[[2L]], from = package)
        }
        for (imp in nsInfo$importClasses) namespaceImportClasses(ns, 
            loadNamespace(j <- imp[[1L]], c(lib.loc, .libPaths()), 
                versionCheck = vI[[j]]), imp[[2L]], from = package)
        for (imp in nsInfo$importMethods) namespaceImportMethods(ns, 
            loadNamespace(j <- imp[[1L]], c(lib.loc, .libPaths()), 
                versionCheck = vI[[j]]), imp[[2L]], from = package)
        "__LoadingNamespaceInfo__" <- list(libname = package.lib, 
            pkgname = package)
        env <- asNamespace(ns)
        env$.packageName <- package
        codename <- strsplit(package, "_", fixed = TRUE)[[1L]][1L]
        codeFile <- file.path(pkgpath, "R", codename)
        if (file.exists(codeFile)) {
            res <- try(sys.source(codeFile, env, keep.source = keep.source))
            if (inherits(res, "try-error")) 
                stop(gettextf("unable to load R code in package %s", 
                  sQuote(package)), call. = FALSE, domain = NA)
        }
        if (partial) 
            return(ns)
        dbbase <- file.path(pkgpath, "R", "sysdata")
        if (file.exists(paste0(dbbase, ".rdb"))) 
            lazyLoad(dbbase, env)
        dbbase <- file.path(pkgpath, "data", "Rdata")
        if (file.exists(paste0(dbbase, ".rdb"))) 
            lazyLoad(dbbase, .getNamespaceInfo(env, "lazydata"))
        registerS3methods(nsInfo$S3methods, package, env)
        dlls <- list()
        dynLibs <- nsInfo$dynlibs
        for (i in seq_along(dynLibs)) {
            lib <- dynLibs[i]
            dlls[[lib]] <- library.dynam(lib, package, package.lib)
            assignNativeRoutines(dlls[[lib]], lib, env, nsInfo$nativeRoutines[[lib]])
            if (!is.null(names(nsInfo$dynlibs)) && nzchar(names(nsInfo$dynlibs)[i])) 
                env[[names(nsInfo$dynlibs)[i]]] <- dlls[[lib]]
            setNamespaceInfo(env, "DLLs", dlls)
        }
        addNamespaceDynLibs(env, nsInfo$dynlibs)
        Sys.setenv(`_R_NS_LOAD_` = package)
        on.exit(Sys.unsetenv("_R_NS_LOAD_"), add = TRUE)
        runHook(".onLoad", env, package.lib, package)
        exports <- nsInfo$exports
        for (p in nsInfo$exportPatterns) exports <- c(ls(env, 
            pattern = p, all.names = TRUE), exports)
        if (.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns) && 
            !identical(package, "methods")) {
            methods::cacheMetaData(ns, TRUE, ns)
            for (p in nsInfo$exportPatterns) {
                expp <- ls(ns, pattern = p, all.names = TRUE)
                newEx <- !(expp %in% exports)
                if (any(newEx)) 
                  exports <- c(expp[newEx], exports)
            }
            expClasses <- nsInfo$exportClasses
            pClasses <- character()
            aClasses <- methods::getClasses(ns)
            classPatterns <- nsInfo$exportClassPatterns
            if (!length(classPatterns)) 
                classPatterns <- nsInfo$exportPatterns
            for (p in classPatterns) {
                pClasses <- c(aClasses[grep(p, aClasses)], pClasses)
            }
            pClasses <- unique(pClasses)
            if (length(pClasses)) {
                good <- vapply(pClasses, methods::isClass, NA, 
                  where = ns)
                if (!any(good) && length(nsInfo$exportClassPatterns)) 
                  warning(gettextf("'exportClassPattern' specified in 'NAMESPACE' but no matching classes in package %s", 
                    sQuote(package)), call. = FALSE, domain = NA)
                expClasses <- c(expClasses, pClasses[good])
            }
            if (length(expClasses)) {
                missingClasses <- !vapply(expClasses, methods::isClass, 
                  NA, where = ns)
                if (any(missingClasses)) 
                  stop(gettextf("in package %s classes %s were specified for export but not defined", 
                    sQuote(package), paste(expClasses[missingClasses], 
                      collapse = ", ")), domain = NA)
                expClasses <- paste0(methods::classMetaName(""), 
                  expClasses)
            }
            allGenerics <- unique(c(methods:::.getGenerics(ns), 
                methods:::.getGenerics(parent.env(ns))))
            expMethods <- nsInfo$exportMethods
            addGenerics <- expMethods[is.na(match(expMethods, 
                exports))]
            if (length(addGenerics)) {
                nowhere <- vapply(addGenerics, function(what) !exists(what, 
                  mode = "function", envir = ns), NA, USE.NAMES = FALSE)
                if (any(nowhere)) {
                  warning(gettextf("no function found corresponding to methods exports from %s for: %s", 
                    sQuote(package), paste(sQuote(sort(unique(addGenerics[nowhere]))), 
                      collapse = ", ")), domain = NA, call. = FALSE)
                  addGenerics <- addGenerics[!nowhere]
                }
                if (length(addGenerics)) {
                  addGenerics <- addGenerics[vapply(addGenerics, 
                    function(what) !is.primitive(get(what, mode = "function", 
                      envir = ns)), NA)]
                  ok <- vapply(addGenerics, methods:::.findsGeneric, 
                    1L, ns)
                  if (!all(ok)) {
                    bad <- sort(unique(addGenerics[!ok]))
                    msg <- ngettext(length(bad), "Function found when exporting methods from the namespace %s which is not S4 generic: %s", 
                      "Functions found when exporting methods from the namespace %s which are not S4 generic: %s")
                    stop(sprintf(msg, sQuote(package), paste(sQuote(bad), 
                      collapse = ", ")), domain = NA, call. = FALSE)
                  }
                  else if (any(ok > 1L)) 
                    addGenerics <- addGenerics[ok < 2L]
                }
                exports <- c(exports, addGenerics)
            }
            expTables <- character()
            if (length(allGenerics)) {
                expMethods <- unique(c(expMethods, exports[!is.na(match(exports, 
                  allGenerics))]))
                missingMethods <- !(expMethods %in% allGenerics)
                if (any(missingMethods)) 
                  stop(gettextf("in %s methods for export not found: %s", 
                    sQuote(package), paste(expMethods[missingMethods], 
                      collapse = ", ")), domain = NA)
                tPrefix <- methods:::.TableMetaPrefix()
                allMethodTables <- unique(c(methods:::.getGenerics(ns, 
                  tPrefix), methods:::.getGenerics(parent.env(ns), 
                  tPrefix)))
                needMethods <- (exports %in% allGenerics) & !(exports %in% 
                  expMethods)
                if (any(needMethods)) 
                  expMethods <- c(expMethods, exports[needMethods])
                pm <- allGenerics[!(allGenerics %in% expMethods)]
                if (length(pm)) {
                  prim <- vapply(pm, function(pmi) {
                    f <- methods::getFunction(pmi, FALSE, FALSE, 
                      ns)
                    is.primitive(f)
                  }, logical(1L))
                  expMethods <- c(expMethods, pm[prim])
                }
                for (i in seq_along(expMethods)) {
                  mi <- expMethods[[i]]
                  if (!(mi %in% exports) && exists(mi, envir = ns, 
                    mode = "function", inherits = FALSE)) 
                    exports <- c(exports, mi)
                  pattern <- paste0(tPrefix, mi, ":")
                  ii <- grep(pattern, allMethodTables, fixed = TRUE)
                  if (length(ii)) {
                    if (length(ii) > 1L) {
                      warning(gettextf("multiple methods tables found for %s", 
                        sQuote(mi)), call. = FALSE, domain = NA)
                      ii <- ii[1L]
                    }
                    expTables[[i]] <- allMethodTables[ii]
                  }
                  else {
                    warning(gettextf("failed to find metadata object for %s", 
                      sQuote(mi)), call. = FALSE, domain = NA)
                  }
                }
            }
            else if (length(expMethods)) 
                stop(gettextf("in package %s methods %s were specified for export but not defined", 
                  sQuote(package), paste(expMethods, collapse = ", ")), 
                  domain = NA)
            exports <- unique(c(exports, expClasses, expTables))
        }
        if (length(exports)) {
            stoplist <- c(".__NAMESPACE__.", ".__S3MethodsTable__.", 
                ".packageName", ".First.lib", ".onLoad", ".onAttach", 
                ".conflicts.OK", ".noGenerics")
            exports <- exports[!exports %in% stoplist]
        }
        namespaceExport(ns, exports)
        sealNamespace(ns)
        runUserHook(package, pkgpath)
        on.exit()
        Sys.unsetenv("_R_NS_LOAD_")
        ns
    }
}

