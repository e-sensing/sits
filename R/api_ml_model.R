#' @title Return machine learning model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model as specified by the original ML function
.ml_model <- function(ml_model) {
    env <- environment(ml_model)

    if (.ml_is_torch_model(ml_model)) {
        return(.torch_model_restore(ml_model))
    }
    if ("model" %in% ls(env, all.names = TRUE)) {
        return(env[["model"]])
    }


    stop(.conf("messages", ".ml_model"))
}
#' @title Return statistics of ML model inside a closure (new version)
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#'
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Data statistics contained in the model closure
.ml_stats <- function(ml_model) {
    # New stats variable
    environment(ml_model)[["ml_stats"]]
}
#' @title Symbols used by code
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Walks calls, pairlists, lists and the formals and body of
#' functions, and returns the names of the symbols found. Constants are
#' ignored. The right side of \code{$} and \code{@@} and the package of
#' \code{::} are names, not variables, and are not returned.
#' @param  x  A function, a call or a list of them
#' @return    Character vector with symbol names
.ml_symbols <- function(x) {
    if (is.symbol(x)) {
        return(setdiff(as.character(x), ""))
    }
    if (is.function(x)) {
        if (is.primitive(x)) {
            return(character(0L))
        }
        return(union(.ml_symbols(formals(x)), .ml_symbols(body(x))))
    }
    if (is.call(x)) {
        head <- x[[1L]]
        if (identical(head, as.name("::")) || identical(head, as.name(":::"))) {
            return(character(0L))
        }
        if (identical(head, as.name("$")) || identical(head, as.name("@"))) {
            return(.ml_symbols(x[[2L]]))
        }
    }
    if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
        x <- as.list(x)
        symbols <- character(0L)
        for (i in seq_along(x)) {
            # an argument without default is the empty symbol
            if (!identical(x[[i]], quote(expr = ))) {
                symbols <- union(symbols, .ml_symbols(x[[i]]))
            }
        }
        return(symbols)
    }
    character(0L)
}
#' @title Frames of an environment up to the namespace
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  env   Environment of a closure
#' @return       List with \code{env} and its parents below the namespace
.ml_frames <- function(env) {
    namespace <- topenv(env)
    frames <- list()
    while (!identical(env, namespace) && !identical(env, emptyenv())) {
        frames[[length(frames) + 1L]] <- env
        env <- parent.env(env)
    }
    frames
}
#' @title Is an environment a namespace or a top level environment?
#' @keywords internal
#' @noRd
#' @param  env  Environment
#' @return      TRUE or FALSE
.ml_is_stop_env <- function(env) {
    isNamespace(env) || identical(env, emptyenv()) ||
        identical(env, globalenv()) || identical(env, baseenv())
}
#' @title Is an environment created by R6 or torch for a module?
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description The enclosing environment of the methods of an R6 object,
#' or the torch environment that holds \code{inherit}: it binds only names
#' of config key \code{ml_module_env_names}, with \code{self} an
#' environment or \code{inherit} an R6 class.
#' @param  env  Environment
#' @return      TRUE or FALSE
.ml_is_module_env <- function(env) {
    if (!is.environment(env) || .ml_is_stop_env(env)) {
        return(FALSE)
    }
    names <- ls(env, all.names = TRUE)
    if (!.has(names) || !all(names %in% .conf("ml_module_env_names"))) {
        return(FALSE)
    }
    # a function frame may bind these names, but not to these values
    is.environment(get0("self", envir = env, inherits = FALSE)) ||
        inherits(
            get0("inherit", envir = env, inherits = FALSE), "R6ClassGenerator"
        )
}
#' @title Environment with the objects that code reads
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Looks up the symbols of \code{code} in \code{env} and its
#' parents, up to the namespace, and copies the objects found into a new
#' environment whose parent is the namespace. A function copied from those
#' frames is walked too and moved to the new environment.
#' @param  code    A function, a call or a list of them
#' @param  env     Environment where \code{code} was defined
#' @param  result  Environment to fill; names it binds are not copied
#' @return         The environment filled
.ml_env <- function(code, env, result = new.env(parent = topenv(env))) {
    frames <- .ml_frames(env)
    is_frame <- function(e) any(purrr::map_lgl(frames, identical, e))
    pending <- .ml_symbols(code)
    while (.has(pending)) {
        name <- pending[[1L]]
        pending <- pending[-1L]
        frame <- purrr::detect(frames, function(e) {
            exists(name, envir = e, inherits = FALSE)
        })
        if (is.null(frame) || exists(name, envir = result, inherits = FALSE)) {
            next
        }
        # a binding to a missing argument has no value
        value <- tryCatch(
            get(name, envir = frame, inherits = FALSE),
            error = function(e) NULL
        )
        if (is.function(value) && is_frame(environment(value))) {
            pending <- union(pending, .ml_symbols(value))
            environment(value) <- result
        }
        assign(name, value, envir = result)
    }
    result
}
#' @title Cut the frames that the torch modules of a model read
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Walks the objects of \code{env} through lists, attributes,
#' R6 objects and module environments, without entering function frames or
#' namespaces. A torch module reads objects from the frame where its class
#' was defined. Its module environment gets a new parent with only these
#' objects, so the model no longer holds the whole frame, which is not
#' changed.
#' @param  env    Environment of a compacted model, filled from \code{frame}
#' @param  frame  Frame where the model closure was defined
#' @return        Called for side effects
.ml_cut_modules <- function(env, frame) {
    address <- function(e) format.default(e)
    # new environment of each frame cut, and frame of each new environment
    news <- new.env(hash = TRUE, parent = emptyenv())
    origins <- new.env(hash = TRUE, parent = emptyenv())
    # environments visited and module environments cut, by address
    seen <- new.env(hash = TRUE, parent = emptyenv())
    cut_done <- new.env(hash = TRUE, parent = emptyenv())
    is_new <- function(e) exists(address(e), envir = origins, inherits = FALSE)
    mark <- function(e) assign(address(e), TRUE, envir = seen)
    is_seen <- function(e) exists(address(e), envir = seen, inherits = FALSE)
    register <- function(e, new_env) {
        assign(address(e), new_env, envir = news)
        assign(address(new_env), e, envir = origins)
        mark(new_env)
    }
    # values of the bindings; active bindings compute values on access,
    # and a binding to a missing argument has no value
    bindings <- function(e) {
        names <- purrr::discard(ls(e, all.names = TRUE), bindingIsActive, e)
        purrr::map(names, function(name) {
            tryCatch(
                get(name, envir = e, inherits = FALSE),
                error = function(err) NULL
            )
        })
    }
    enter <- function(e) purrr::walk(bindings(e), visit)
    # copies into the new environment of `e` what `code` reads there
    read_into <- function(e, code) {
        if (is_new(e)) {
            e <- get(address(e), envir = origins)
        }
        new_env <- get0(address(e), envir = news, inherits = FALSE)
        if (is.null(new_env)) {
            new_env <- new.env(parent = topenv(e))
            register(e, new_env)
        }
        before <- ls(new_env, all.names = TRUE)
        .ml_env(code, e, new_env)
        bind(new_env, setdiff(ls(new_env, all.names = TRUE), before))
        new_env
    }
    # gives the last module environment above `start` a new parent
    cut <- function(start, code) {
        owned <- list(start)
        parent <- parent.env(start)
        while (!is_new(parent) && .ml_is_module_env(parent)) {
            owned <- c(owned, parent)
            parent <- parent.env(parent)
        }
        last <- owned[[length(owned)]]
        new_env <- NULL
        if (!.ml_is_stop_env(parent)) {
            new_env <- parent
            # objects of one class share the module environments above them
            if (!exists(address(last), envir = cut_done, inherits = FALSE)) {
                assign(address(last), TRUE, envir = cut_done)
                new_env <- read_into(parent, code)
                # the training call created it, or readRDS() copied it, so
                # no object outside the model shares it
                parent.env(last) <- new_env
            }
        }
        # the torch environment binds the parent class in `inherit`
        purrr::walk(owned[-1L], visit)
        new_env
    }
    # R6 object or the `super` of one
    cut_object <- function(x) {
        enclos <- get(".__enclos_env__", envir = x, inherits = FALSE)
        private <- get0("private", envir = enclos, inherits = FALSE)
        envs <- c(list(x), if (is.environment(private)) list(private))
        values <- c(
            unlist(purrr::map(envs, bindings), recursive = FALSE),
            get0(".__active__", envir = enclos, inherits = FALSE)
        )
        methods <- purrr::keep(values, function(f) {
            is.function(f) && identical(environment(f), enclos)
        })
        cut(enclos, methods)
        purrr::walk(envs, mark)
        mark(enclos)
        purrr::walk(c(envs, enclos), enter)
    }
    # R6 class: methods are given an environment by each $new()
    cut_class <- function(gen) {
        fields <- c("public_methods", "private_methods", "active")
        lists <- purrr::map(fields, get0, envir = gen, inherits = FALSE)
        code <- c(unlist(lists, recursive = FALSE), list(gen[["inherit"]]))
        parent <- gen[["parent_env"]]
        new_env <- NULL
        if (.ml_is_module_env(parent)) {
            new_env <- cut(parent, code)
            visit(parent)
        } else if (!.ml_is_stop_env(parent) && !is_new(parent)) {
            new_env <- read_into(parent, code)
            gen[["parent_env"]] <- new_env
        }
        # the environments stored only keep the frame alive; the class is
        # created by the training call or copied by readRDS(), as above
        if (!is.null(new_env)) {
            purrr::walk2(fields, lists, function(field, funs) {
                if (.has(funs)) {
                    funs <- purrr::map(funs, function(f) {
                        environment(f) <- new_env
                        f
                    })
                    assign(field, funs, envir = gen)
                }
            })
        }
        purrr::walk(c("public_fields", "private_fields"), function(field) {
            visit(get0(field, envir = gen, inherits = FALSE))
        })
    }
    # a function defined in a frame that is cut, or a torch class generator,
    # defined in the frame of nn_module(), which binds the frame of the class
    needs_env <- function(x) {
        if (!is.function(x) || is.primitive(x)) {
            return(FALSE)
        }
        e <- environment(x)
        !.ml_is_stop_env(e) && !is_new(e) && (
            exists(address(e), envir = news, inherits = FALSE) ||
                inherits(attr(x, "module"), "R6ClassGenerator")
        )
    }
    bind <- function(new_env, names) {
        purrr::walk(names, function(name) {
            value <- get(name, envir = new_env, inherits = FALSE)
            if (needs_env(value)) {
                environment(value) <- read_into(environment(value), value)
                assign(name, value, envir = new_env)
            }
            visit(value)
        })
    }
    visit <- function(x) {
        if (is.environment(x)) {
            if (.ml_is_stop_env(x) || is_seen(x)) {
                return(invisible())
            }
            mark(x)
            if (inherits(x, "R6ClassGenerator")) {
                cut_class(x)
            } else if (is.environment(
                get0(".__enclos_env__", envir = x, inherits = FALSE)
            )) {
                cut_object(x)
            } else if (.ml_is_module_env(x)) {
                enter(x)
            }
        } else if (is.function(x) && !is.primitive(x)) {
            if (.ml_is_module_env(environment(x))) {
                visit(environment(x))
            }
        } else if (is.list(x)) {
            purrr::walk(unclass(x), visit)
        }
        purrr::walk(attributes(x), visit)
        invisible()
    }
    # the model environment holds what the training frame binds
    register(frame, env)
    bind(env, ls(env, all.names = TRUE))
    invisible(env)
}
#' @title Keep in a trained model only what it reads
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @description Gives a trained model or encoder, new or saved, a new
#' environment with only the objects that prediction and the accessors
#' read (config key \code{ml_model_names}). The samples keep one row of
#' each label. Torch modules are cut by \code{.ml_cut_modules()}. No
#' function frame is changed, since the training may still be running.
#' @param  model  Closure of a trained model or encoder
#' @return        The model, with its new environment
.ml_model_compact <- function(model) {
    frame <- environment(model)
    # a compacted model has the namespace as parent
    if (identical(parent.env(frame), topenv(frame))) {
        return(model)
    }
    code <- c(list(model), purrr::map(.conf("ml_model_names"), as.name))
    env <- .ml_env(code, frame)
    # cut before the walk, which would visit every series of every sample
    .ml_cut_samples(env)
    .ml_cut_modules(env, frame)
    environment(model) <- env
    model
}
#' @title Cut the samples kept by a compacted model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  env  Environment of a compacted model, changed in place
#' @return      Called for side effects
.ml_cut_samples <- function(env) {
    # accessors read labels and bands, and timeline and dates of row 1
    samples <- env[["samples"]]
    if (.has(samples)) {
        env[["samples"]] <- samples[!duplicated(samples[["label"]]), ]
    }
    # accessors read only the feature names
    if (.has(env[["train_samples"]])) {
        env[["train_samples"]] <- env[["train_samples"]][0L, ]
    }
    invisible(env)
}
#' @title Return samples of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Samples used for ML construction
.ml_samples <- function(ml_model) {
    environment(ml_model)[["samples"]]
}
#' @title Return class of ML model inside a closure
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           ML model class
.ml_class <- function(ml_model) {
    class(ml_model)[[1L]]
}
#' @title Return names of features used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Features used to build the model
.ml_features_name <- function(ml_model) {
    # Get feature names from variable used in training
    names(environment(ml_model)[["train_samples"]])[-2L:0L]
}
#' @title Return names of bands used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Bands used to build the model
.ml_bands <- function(ml_model) {
    .samples_bands(.ml_samples(ml_model))
}
#' @title Return labels of samples of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Sample labels used to build the model
.ml_labels <- function(ml_model) {
    .samples_labels(.ml_samples(ml_model))
}
#' @title Return codes of sample labels of used to train ML model
#' @keywords internal
#' @noRd
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Codes of sample labels used to build the model
.ml_labels_code <- function(ml_model) {
    labels <- .ml_labels(ml_model)
    names(labels) <- seq_along(labels)
    labels
}
#' @title Clean GPU memory allocation
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Called for side effects
.ml_gpu_clean <- function(ml_model) {
    # Clean torch allocations
    # the model check first: the CUDA check loads torch
    if (.ml_is_torch_model(ml_model) && .torch_cuda_enabled()) {
        torch::cuda_empty_cache()
    }
    invisible(NULL)
}

#' @title normalize the probability results
#' @keywords internal
#' @author Rolf Simoes, \email{rolfsimoes@@gmail.com}
#' @noRd
#' @param  values    Values to be normalized
#' @param  ml_model  Closure that contains ML model and its environment
#' @return           Normalized values
#'
.ml_normalize <- function(values, ml_model) {
    UseMethod(".ml_normalize", ml_model)
}
#' @export
#'
.ml_normalize.torch_model <- function(values, ml_model) {
    # Correct the default behaviour of softmax in torch models
    # Run softmax here instead of inside a torch model
    column_names <- colnames(values)
    values[is.na(values)] <- 0
    values <- softmax(values)
    colnames(values) <- column_names
    values
}
#' @export
#'
.ml_normalize.default <- function(values, ml_model) {
    values[is.na(values)] <- 0
    values
}
#' @title Update multicores for models that do internal multiprocessing
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @param  multicores Current multicores setting
#' @return            Updated multicores
#'
.ml_update_multicores <- function(ml_model, multicores) {
    # xgboost model has internal multiprocessing
    if ("xgb_model" %in% .ml_class(ml_model)) {
        multicores <- 1L
    }
    # torch in GPU has internal multiprocessing
    if (.ml_is_torch_model(ml_model) && .torch_gpu_classification()) {
        multicores <- 1L
    }

    multicores
}
#' @title Is the ML model a torch model?
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @return            TRUE/FALSE
.ml_is_torch_model <- function(ml_model) {
    inherits(ml_model, "torch_model")
}
#' @title Is the ML method a torch method, before training?
#' @keywords internal
#' @noRd
#' @param  ml_method  Training function returned by a sits ML method
#' @return            TRUE/FALSE
.ml_is_torch_method <- function(ml_method) {
    # relies on torch factories defining an `optimizer` argument
    "optimizer" %in% ls(environment(ml_method))
}

#' @title Is the ML torch model temporal?
#' @keywords internal
#' @noRd
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @param  ml_model   Closure that contains ML model and its environment
#' @return            TRUE/FALSE
.ml_torch_is_temporal <- function(ml_model) {
    .ml_is_torch_model(ml_model) && !"mlp_model" %in% ls(environment(ml_model))
}
