fit_model  <-  function (settings, extrapolation_list = NULL, Lat_i, Lon_i, t_i, b_i, a_i, c_iz = rep(0, 
                                                             length(b_i)), v_i = rep(0, length(b_i)), working_dir = paste0(getwd(), 
                                                                                                                           "/"), Xconfig_zcp = NULL, covariate_data, formula = ~0, 
          Q_ik = NULL, newtonsteps = 1, silent = TRUE, build_model = TRUE, 
          run_model = TRUE, test_fit = TRUE, ...) 
{
  extra_args = list(...)
  extra_args = c(extra_args, extra_args$extrapolation_args, 
                 extra_args$spatial_args, extra_args$optimize_args, extra_args$model_args)
  data_frame = data.frame(Lat_i = Lat_i, Lon_i = Lon_i, a_i = a_i, 
                          v_i = v_i, b_i = b_i, t_i = t_i, c_iz = c_iz)
  year_labels = seq(min(t_i), max(t_i))
  years_to_plot = which(year_labels %in% t_i)
  dir.create(working_dir, showWarnings = FALSE, recursive = TRUE)
  capture.output(settings, file = file.path(working_dir, "settings.txt"))
  message("\n### Making extrapolation-grid")
  if(is.null(extrapolation_list)){
    message("\n### This isn't working")
    
  extrapolation_args_default = list(Region = settings$Region, extrapolation_list = extrapolation_list,
                                    strata.limits = settings$strata.limits, zone = settings$zone, 
                                    max_cells = settings$max_cells)
  extrapolation_args_input = combine_lists(input = extra_args, 
                                           default = extrapolation_args_default, args_to_use = formalArgs(make_extrapolation_info))
  extrapolation_list = do.call(what = make_extrapolation_info, 
                               args = extrapolation_args_input)
  }else{
    extrapolation_args_default = list(Region = settings$Region, extrapolation_list = extrapolation_list,
                                      strata.limits = settings$strata.limits, zone = settings$zone, 
                                      max_cells = settings$max_cells)
    extrapolation_args_input = combine_lists(input = extra_args, 
                                             default = extrapolation_args_default, args_to_use = formalArgs(make_extrapolation_info))
    extrapolation_list = extrapolation_list
  }
  message("\n### Making spatial information")
  spatial_args_default = list(grid_size_km = settings$grid_size_km, 
                              n_x = settings$n_x, Method = settings$Method, Lon_i = Lon_i, 
                              Lat_i = Lat_i, Extrapolation_List = extrapolation_list, 
                              DirPath = working_dir, Save_Results = TRUE, fine_scale = settings$fine_scale, 
                              knot_method = settings$knot_method)
  spatial_args_input = combine_lists(input = extra_args, default = spatial_args_default, 
                                     args_to_use = formalArgs(make_spatial_info))
  spatial_list = do.call(what = make_spatial_info, args = spatial_args_input)
  message("\n### Making data object")
  if (missing(covariate_data)) 
    covariate_data = NULL
  data_args_default = list(Version = settings$Version, FieldConfig = settings$FieldConfig, 
                           OverdispersionConfig = settings$OverdispersionConfig, 
                           RhoConfig = settings$RhoConfig, VamConfig = settings$VamConfig, 
                           ObsModel = settings$ObsModel, c_iz = c_iz, b_i = b_i, 
                           a_i = a_i, v_i = v_i, s_i = spatial_list$knot_i - 1, 
                           t_i = t_i, spatial_list = spatial_list, Options = settings$Options, 
                           Aniso = settings$use_anisotropy, Xconfig_zcp = Xconfig_zcp, 
                           covariate_data = covariate_data, formula = formula, Q_ik = Q_ik)
  data_args_input = combine_lists(input = extra_args, default = data_args_default)
  data_list = do.call(what = make_data, args = data_args_input)
  message("\n### Making TMB object")
  model_args_default = list(TmbData = data_list, RunDir = working_dir, 
                            Version = settings$Version, RhoConfig = settings$RhoConfig, 
                            loc_x = spatial_list$loc_x, Method = spatial_list$Method, 
                            build_model = build_model)
  model_args_input = combine_lists(input = extra_args, default = model_args_default, 
                                   args_to_use = formalArgs(make_model))
  tmb_list = do.call(what = make_model, args = model_args_input)
  if (run_model == FALSE | build_model == FALSE) {
    input_args = list(extra_args = extra_args, extrapolation_args_input = extrapolation_args_input, 
                      model_args_input = model_args_input, spatial_args_input = spatial_args_input, 
                      data_args_input = data_args_input)
    Return = list(data_frame = data_frame, extrapolation_list = extrapolation_list, 
                  spatial_list = spatial_list, data_list = data_list, 
                  tmb_list = tmb_list, year_labels = year_labels, years_to_plot = years_to_plot, 
                  settings = settings, input_args = input_args)
    class(Return) = "fit_model"
    return(Return)
  }
  if (silent == TRUE) 
    tmb_list$Obj$env$beSilent()
  if (test_fit == TRUE) {
    message("\n### Testing model at initial values")
    LogLike0 = tmb_list$Obj$fn(tmb_list$Obj$par)
    Gradient0 = tmb_list$Obj$gr(tmb_list$Obj$par)
    if (any(Gradient0 == 0)) {
      message("\n")
      stop("Please check model structure; some parameter has a gradient of zero at starting values\n", 
           call. = FALSE)
    }
    else {
      message("Looks good: All fixed effects have a nonzero gradient")
    }
  }
  message("\n### Estimating parameters")
  optimize_args_default1 = combine_lists(default = list(lower = tmb_list$Lower, 
                                                        upper = tmb_list$Upper, loopnum = 2), input = extra_args, 
                                         args_to_use = formalArgs(TMBhelper::fit_tmb))
  optimize_args_input1 = list(obj = tmb_list$Obj, savedir = NULL, 
                              newtonsteps = 0, bias.correct = FALSE, control = list(eval.max = 10000, 
                                                                                    iter.max = 10000, trace = 1), quiet = TRUE, getsd = FALSE)
  optimize_args_input1 = combine_lists(default = optimize_args_default1, 
                                       input = optimize_args_input1, args_to_use = formalArgs(TMBhelper::fit_tmb))
  parameter_estimates = do.call(what = TMBhelper::fit_tmb, 
                                args = optimize_args_input1)
  if (exists("check_fit") & test_fit == TRUE) {
    problem_found = VAST::check_fit(parameter_estimates)
    if (problem_found == TRUE) {
      message("\n")
      stop("Please change model structure to avoid problems with parameter estimates and then re-try; see details in `?check_fit`\n", 
           call. = FALSE)
    }
  }
  optimize_args_default2 = list(obj = tmb_list$Obj, lower = tmb_list$Lower, 
                                upper = tmb_list$Upper, savedir = working_dir, bias.correct = settings$bias.correct, 
                                newtonsteps = newtonsteps, bias.correct.control = list(sd = FALSE, 
                                                                                       split = NULL, nsplit = 1, vars_to_correct = settings$vars_to_correct), 
                                control = list(eval.max = 10000, iter.max = 10000, trace = 1), 
                                loopnum = 1)
  optimize_args_input2 = combine_lists(input = extra_args, 
                                       default = optimize_args_default2, args_to_use = formalArgs(TMBhelper::fit_tmb))
  optimize_args_input2 = combine_lists(input = list(startpar = parameter_estimates$par), 
                                       default = optimize_args_input2)
  parameter_estimates = do.call(what = TMBhelper::fit_tmb, 
                                args = optimize_args_input2)
  Report = tmb_list$Obj$report()
  ParHat = tmb_list$Obj$env$parList(parameter_estimates$par)
  input_args = list(extra_args = extra_args, extrapolation_args_input = extrapolation_args_input, 
                    model_args_input = model_args_input, spatial_args_input = spatial_args_input, 
                    optimize_args_input1 = optimize_args_input1, optimize_args_input2 = optimize_args_input2, 
                    data_args_input = data_args_input)
  Return = list(data_frame = data_frame, extrapolation_list = extrapolation_list, 
                spatial_list = spatial_list, data_list = data_list, tmb_list = tmb_list, 
                parameter_estimates = parameter_estimates, Report = Report, 
                ParHat = ParHat, year_labels = year_labels, years_to_plot = years_to_plot, 
                settings = settings, input_args = input_args)
  class(Return) = "fit_model"
  return(Return)
}
