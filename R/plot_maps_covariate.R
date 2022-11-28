#' @title{Plot standard maps}
#'
#' @description {plots a standard set of diagnostic maps}
#'
#' @param plot_set integer-vector defining plots to create
#' \describe{
#'   \item{plot_set=1}{Probability of encounter/non-encounter}
#'   \item{plot_set=2}{Log-expected positive catch rate}
#'   \item{plot_set=3}{Log-predicted density (product of encounter probability and positive catch rates)}
#'   \item{plot_set=4}{Log-positive catch rates (rescaled)}
#'   \item{plot_set=5}{Log-predicted density (rescaled)}
#'   \item{plot_set=6}{Spatio-temporal variation in encounter probability}
#'   \item{plot_set=7}{Spatio-temporal variation in log-positive catch rates}
#'   \item{plot_set=8}{Linear predictor for encounter probability}
#'   \item{plot_set=9}{Linear predictor for positive catch rates}
#'   \item{plot_set=10}{Coefficient of variation for predicted density (available only if \code{Data_Fn(...,Options=c('SD_site_logdensity'=1,...))}}
#'   \item{plot_set=11}{Covariates that are included in the model}
#'   \item{plot_set=12}{Total biomass across all categories (only useful in a multivariate model)}
#'   \item{plot_set=13}{Covariate effects on encounter probability}
#'   \item{plot_set=14}{Covariate effects on positive catch rates}
#'   \item{plot_set=15}{Individual covariate effects on encounter probability}
#'   \item{plot_set=16}{Individual covariate effects on positive catch rates}
#' }
#' @param fit tagged list of outputs from TMB model via \code{Obj$report()}
#' @param TmbData optional tagged list of outputs from fit$data_list or VAST::make_data
#' @param spatial_list required for Method == Stream_network, optional for other spatial models, tagged list of outputs from \code{make_spatial_info}
#' @param Sdreport Standard deviation outputs from TMB model via \code{sdreport(Obj)}
#' @param Panel Whether to plot years for a given category (\code{Panel="Category"}) or categories for a given year ((\code{Panel="Year"})  in each panel figure
#' @param Ylim ylimits for each panel
#' @param Xlim xlimits for each panel
#' @param Zlim value scale limits
#' @param DirName Directory (absolute path)
#' @param PlotName plot names are automatically generated but option to add a modifier
#' @param category_names character vector specifying names for different categories (only used for R package \code{VAST})
#' @param covar_names character vector specifying covariate names for labeling figures
#' @param legend Boolean whether to plot legend or not
#' @param textmargin option to include y-axis text
#' @param option to add arrows between network nodes
#' @param cex size of points for map
#' @param ... arguments passed to \code{par}
#'
#' @return Mat_xt a matrix (rows: modeled knots; column: modeled year) for plotted output of last element of \code{plot_set}
#'

#' @export
plot_maps <-
  function(Data_Geostat = Data_Geostat, plot_set=3, fit1, fit2, fit3, plot_value = NULL, n_samples = 100,
           data = NULL,
           Sdreport1=NULL,Sdreport2=NULL, Sdreport3=NULL, years = "all", 
           Report1, Report2, Report3,
           Obj1 = NULL, Obj2 = NULL, Obj3 = NULL,
           Xlim=NULL, Ylim=NULL, Zlim = NULL, projargs = "+proj=longlat",
           TmbData=NULL, spatial_list1=NULL,spatial_list2=NULL,spatial_list3=NULL, Panel="Category",
           DirName=NULL, PlotName=NULL,sample_fixed = TRUE,
           category_names=NULL, covar_names=NULL,
           legend=TRUE, textmargin=NULL, arrows=FALSE, cex=0.5,base_size = 30,...){
    
    extract_value = function(Sdreport, Report, Obj, variable_name, 
                             plot_value = "estimate", n_samples, sample_fixed = TRUE) {
      if (missing(Report)) {
        Report = Obj$report()
      }
      if (is.function(plot_value)) {
        if (missing(Obj)) 
          stop("Must provide `Obj` for `extract_value(.)` in `plot_maps(.)` when specifying a function for argument `plot_value`")
        Var_r = sample_variable(Sdreport = Sdreport, Obj = Obj, 
                                variable_name = variable_name, n_samples = n_samples, 
                                sample_fixed = sample_fixed)
        Return = apply(Var_r, MARGIN = 1:(length(dim(Var_r)) - 
                                            1), FUN = plot_value)
        if (any(dim(Return) != dim(Report[[variable_name]]))) {
          stop("Check `extract_value(.)` in `plot_maps(.)`")
        }
      }
      else if (plot_value == "estimate") {
        Return = Report[[variable_name]]
      }
      else if (plot_value == "CV") {
        Var_r = sample_variable(Sdreport = Sdreport, Obj = Obj, 
                                variable_name = variable_name, n_samples = n_samples, 
                                sample_fixed = sample_fixed)
        Return1 = apply(Var_r, MARGIN = 1:(length(dim(Var_r)) - 
                                             1), FUN = sd)
        Return2 = Report[[variable_name]]
        Return = Return1/Return2
      }
      else stop("Check input `plot_value` in `plot_maps(.)`")
      return(Return)
    }
    
    # local functions
    logsum = function(vec){ max(vec) + log(sum(exp(vec-max(vec)))) }
    
    year_labels = fit1$year_labels
    years_to_plot = fit1$years_to_plot
    Report1 <- fit1$Report
    Report2 <- fit2$Report
    Report3 <- fit3$Report
    
    # Fill in missing inputs
    if( "D_gcy" %in% names(Report1)){
      # VAST Version >= 8.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report1$D_gcy)[2]
      Ncategories = dim(Report1$D_gcy)[2]
      Nyears = dim(Report1$D_gcy)[3]
    }
    if( "D_gcy" %in% names(Report2)){
      # VAST Version >= 8.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report2$D_gcy)[2]
      Ncategories = dim(Report2$D_gcy)[2]
      Nyears = dim(Report2$D_gcy)[3]
    }
    if( "D_gcy" %in% names(Report3)){
      # VAST Version >= 8.0.0
      if( is.null(category_names) ) category_names = 1:dim(Report3$D_gcy)[2]
      Ncategories = dim(Report3$D_gcy)[2]
      Nyears = dim(Report3$D_gcy)[3]
    }
  
    
    
    # Errors
    if( Nyears != length(year_labels) ){
      stop("Problem with `year_labels`")
    }
    if( Ncategories != length(category_names) ){
      stop("Problem with `category_names`")
    }
    
    # Extract elements
    plot_codes <- c("Pres", "Pos", "Dens", "Pos_Rescaled", "Dens_Rescaled", "Eps_Pres", "Eps_Pos", "LinPred_Pres", "LinPred_Pos", "Dens_CV", "Covariates", "Total_dens", "Cov_effects_Pres", "Cov_effects_Pos", "Indiv_cov_effects_Pres", "Indiv_cov_effects_Pos")
    plot_names <- c("presence_absence", "log-positive catch rates", "log-density", "positive catch rates", "density", "epsilon for presence_absence", "epsilon for positive catch rates", "encounter probability linear predictor", "positive catch rates linear predictor", "density CV", "covariates", "total density", "encounter probability covariate effects", "catch rates covariate effects", "encounter probability individual covariate effects", "catch rates individual covariate effects")
    if( is.null(textmargin)){
      textmargin <- c("Probability of encounter", "Density, ln(kg. per square km.)", "Density, ln(kg. per square km.)", "", "", "", "", "", "", "CV of density (dimensionless)", "Covariate value", "Density, ln(kg. per square km.)", "", "", "", "")
    }
    # Select locations to plot
    # if( Nknots<Inf ){
    #   NN_plot = stats::kmeans(x=PlotDF[,c("Lon","Lat")], centers=Nknots, iter.max=50, nstart=2, trace=0)
    #   Match = match( 1:Nknots, NN_plot$cluster)
    #   PlotDF = PlotDF[Match,]
    #   message( "Restricted plotting locations to ", Nknots, " locations" )
    # }
    
    # Loop through plots
    for(plot_num in plot_set){
      
      inp_Zlim <- Zlim
      # Extract matrix to plot
      if(plot_num==11){
        if(is.null(TmbData)) stop( "Must provide `TmbData` to plot covariates" )
        if(!("X_gtp" %in% names(TmbData))) stop( "Can only plot covariates for VAST version >= 2.0.0" )
        Array_xct = aperm( TmbData$X_gtp, perm=c(1,3,2) )
        category_names = 1:dim(Array_xct)[2]
      }
 
     
      if(plot_num==14){
        # Covariate effects for positive catch rates
        if("D_gcy"%in%names(Report1)) Array_xct1 = extract_value(Sdreport = Sdreport1,
                                                               Report = Report1, Obj = Obj1, plot_value = plot_value,
                                                               sample_fixed = sample_fixed, n_samples = n_samples,
                                                               variable_name = "Xi2_gcp")#variable_name = "eta2_gct")
                                    Array_xct2 = extract_value(Sdreport = Sdreport2,
                                                               Report = Report2, Obj = Obj2, plot_value = plot_value,
                                                               sample_fixed = sample_fixed, n_samples = n_samples,
                                                               variable_name = "Xi2_gcp")#variable_name = "eta2_gct")
                                    Array_xct3 = extract_value(Sdreport = Sdreport3,
                                                               Report = Report3, Obj = Obj3, plot_value = plot_value,
                                                               sample_fixed = sample_fixed, n_samples = n_samples,
                                                               variable_name = "Xi2_gcp")#variable_name = "eta2_gct")
        #Array_xct = Report$Xi2_gcp #Array_xct = Report$eta2_gct
        if("dhat_ktp" %in% names(Report1)) stop()
        if("dpred_ktp" %in% names(Report1)) stop()
      }
  
      
      # require(ggplot2)
      if(all(is.null(spatial_list1))) stop("add spatial_list (output from FishStatsUtils::make_spatial_info) to use ggplot2 plots.")
      years_to_plot = 1:dim(Array_xct1)[3]
      
      
      # Plot for each year
      if( tolower(Panel)=="year" ){
        plot_list = list()
        plot_list1 = list()
        plot_list2 = list()
        plot_list3 = list()
        Nplot = length(years_to_plot)
        index <- 1
        for( tI in 1:Nplot){
          if(length(dim(Array_xct1))==2) Mat_xc1 = Array_xct1[,years_to_plot[tI],drop=TRUE]
          if(length(dim(Array_xct1))==3) Mat_xc1 = Array_xct1[,,years_to_plot[tI],drop=TRUE]
          if(length(dim(Array_xct2))==2) Mat_xc2 = Array_xct2[,years_to_plot[tI],drop=TRUE]
          if(length(dim(Array_xct2))==3) Mat_xc2 = Array_xct2[,,years_to_plot[tI],drop=TRUE]
          if(length(dim(Array_xct3))==2) Mat_xc3 = Array_xct3[,years_to_plot[tI],drop=TRUE]
          if(length(dim(Array_xct3))==3) Mat_xc3 = Array_xct3[,,years_to_plot[tI],drop=TRUE]
          
          if(is.null(dim(Mat_xc1)) & is.vector(Mat_xc1)){
            Ncategories = 1
          } else { Ncategories = dim(Mat_xc1)[2] }
          Return1 = Mat_xc1 = array( as.vector(Mat_xc1), dim=c(dim(Array_xct1)[1],Ncategories)) # Reformat to make sure it has same format for everything
          Return2 = Mat_xc2 = array( as.vector(Mat_xc2), dim=c(dim(Array_xct2)[1],Ncategories)) # Reformat to make sure it has same format for everything
          Return3 = Mat_xc3 = array( as.vector(Mat_xc3), dim=c(dim(Array_xct3)[1],Ncategories)) # Reformat to make sure it has same format for everything
          
          ## matrix is number of nodes by number of years
          n_c <- dim(Mat_xc1)[2]
          xct1 <- data.frame('value'=Mat_xc1, 'year'=year_labels[years_to_plot[tI]], spatial_list1$latlon_g, "category"='total')
          xct2 <- data.frame('value'=Mat_xc2, 'year'=year_labels[years_to_plot[tI]], spatial_list2$latlon_g, "category"='total')
          xct3 <- data.frame('value'=Mat_xc3, 'year'=year_labels[years_to_plot[tI]], spatial_list3$latlon_g, "category"='total')
          
          p1 <- ggplot(xct1) 
          p2 <- ggplot(xct2) 
          p3 <- ggplot(xct3) 
          p4 <- ggplot(data) 
          
          if(is.null(Zlim)) inp_Zlim = quantile(xct1$value, prob = c(0,1), na.rm=TRUE)
          
          theme_params = list()
          #theme_params[['plot.margin']] = unit(c(-0.1,-0.1,-0.1,-0.1),"cm") #unit(c(-0.1,-0.3,-0.3,-0.3),"cm")
          #theme_params[['plot.margin']] = unit(c(1,0.1,0.1,0.1),"cm") #unit(c(-0.1,-0.3,-0.3,-0.3),"cm")
          theme_params[['plot.margin']] = unit(c(1,0.6,0.1,0.1),"cm") #unit(c(-0.1,-0.3,-0.3,-0.3),"cm") c(top,right,bottom,left)
          theme_params[['legend.position']] = "none"

          plot_theme = do.call(theme,theme_params)
          
          p1 <- p1 +
            geom_point(aes(x = ifelse(Lon> 0, Lon, Lon + 360), y = Lat, color = value), cex=cex) +
            scale_colour_viridis(option="magma", limits = c(min(xct2$value),max(xct2$value)),breaks = c(-1.2,-0.8,-0.4,0,0.2,0.4)) + 
            #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
            coord_cartesian(xlim = Xlim, ylim = Ylim) +
            scale_x_continuous(breaks=quantile(ifelse(xct1$Lon> 0, xct1$Lon, xct1$Lon + 360), prob=c(0.1,0.5,0.9)), labels=round(quantile(ifelse(xct1$Lon> 0, xct1$Lon, xct1$Lon + 360), prob=c(0.1,0.5,0.9)),0)) +
            mytheme(base_size = base_size) +
            xlab("Longitude") + ylab("Latitude")+
            ggtitle("Pollock") +
            borders(fill="darkgrey",colour="darkgrey",xlim = c(-180,180), ylim = Ylim) + 
            plot_theme #+  
            # theme(legend.justification = c("left", "top"),
            #       legend.position = c(0.01, 0.23),
            #       legend.background = element_rect(fill = "white", linetype = 1, color = "black",size = 5),
            #       legend.title=element_blank(),
            #       legend.text = element_text(size=25, face="bold"),
            #       legend.key.size = unit(1.5, "cm"))
          
          p2 <- p2 +
            geom_point(aes(x = ifelse(Lon> 0, Lon, Lon + 360), y = Lat, color = value), cex=cex) +
            scale_colour_viridis(option="magma", limits = c(min(xct2$value),max(xct2$value)),breaks = c(-1.2,-0.8,-0.4,0,0.2,0.4)) + 
            #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
            coord_cartesian(xlim = Xlim, ylim = Ylim) +
            scale_x_continuous(breaks=quantile(ifelse(xct2$Lon> 0, xct2$Lon, xct2$Lon + 360), prob=c(0.1,0.5,0.9)), labels=round(quantile(ifelse(xct2$Lon> 0, xct2$Lon, xct2$Lon + 360), prob=c(0.1,0.5,0.9)),0)) +
            mytheme(base_size = base_size) +
            xlab("Longitude") + ylab("Latitude")+
            ggtitle("Cod") +
            borders(fill="darkgrey",colour="darkgrey",xlim = c(-180,180), ylim = Ylim) + 
            plot_theme #+  
            # theme(legend.justification = c("left", "top"),
            #       legend.position = c(0.01, 0.23),
            #       legend.background = element_rect(fill = "white", linetype = 1, color = "black",size = 5),
            #       legend.title=element_blank(),
            #       legend.text = element_text(size=25, face="bold"),
            #       legend.key.size = unit(1.5, "cm"))
          
          p3 <- p3 +
            geom_point(aes(x = ifelse(Lon> 0, Lon, Lon + 360), y = Lat, color = value), cex=cex) +
            scale_colour_viridis(option="magma", limits = c(min(xct2$value),max(xct2$value)),breaks = c(-1.2,-0.8,-0.4,0,0.2,0.4), labels = c("-1.2","-0.8","-0.4","0","0.2","0.5")) + 
            #scale_color_distiller(palette = "Spectral", limits = c(min(xct3$value),max(xct3$value))) +
            coord_cartesian(xlim = Xlim, ylim = Ylim) +
            scale_x_continuous(breaks=quantile(ifelse(xct3$Lon> 0, xct3$Lon, xct3$Lon + 360), prob=c(0.1,0.5,0.9)), labels=round(quantile(ifelse(xct3$Lon> 0, xct3$Lon, xct3$Lon + 360), prob=c(0.1,0.5,0.9)),0)) +
            mytheme(base_size = base_size) +
            xlab("Longitude") + ylab("Latitude")+
            ggtitle("Plaice") +
            borders(fill="darkgrey",colour="darkgrey",xlim = c(-180,180), ylim = Ylim) + 
            plot_theme +  
            theme(legend.justification = c("left", "top"),
            legend.position = c(0.01, 0.98), #c(0.01, 0.23),
            legend.background = element_rect(fill = "white", linetype = 1, color = "black",size = 1.5),
            legend.margin =  margin(t=0, r=0.2, b=0.3, l=0.2, unit="cm"),
            legend.title=element_blank(),
            legend.text = element_text(size=22, face="bold"),
            legend.key.size = unit(1, "cm"))
          
          p4 <- ggplot() +
            geom_line(data = data, aes(x = year, y = CPE),color = "black", size = 3) +
            geom_point() +
            geom_hline(yintercept = 0,color = "grey", size = 3, linetype = "dashed") +
            mytheme(base_size = 35) +
            xlab("Year") + ylab("Normalized Cold Pool Extent")+
            ggtitle("") +
            plot_theme
          
          # p <- p +
          #   #geom_point(aes(x = E_km, y = N_km, color = value), cex=cex) +
          #   geom_point(aes(x = ifelse(Lon> 0, Lon, Lon + 360), y = Lat, color = value), cex=cex) +
          #   scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
          #   coord_cartesian(xlim = Xlim, ylim = Ylim) +
          #   #scale_x_continuous(breaks=quantile(xct$E_km, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$E_km, prob=c(0.1,0.5,0.9)),0)) +
          #   scale_x_continuous(breaks=quantile(ifelse(xct$Lon> 0, xct$Lon, xct$Lon + 360), prob=c(0.1,0.5,0.9)), labels=round(quantile(ifelse(xct$Lon> 0, xct$Lon, xct$Lon + 360), prob=c(0.1,0.5,0.9)),0)) +
          #   # guides(color=guide_legend(title=plot_codes[plot_num])) +
          #   #facet_wrap(~year) + 
          #   mytheme() +
          #   xlab(ifelse(tI == 37,"Eastings","")) + ylab(ifelse(tI == 16,"Northings",""))+
          #   borders(fill="darkgrey",colour="darkgrey",xlim = c(-180,180), ylim = Ylim) + 
          #   plot_theme #+
          #   #theme(plot.margin=unit(c(0,0,0.5,0.5),"cm"))
          #   #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")  
            
          #plot_list[[tI]] = p
          plot_list[[index]] = p1
          plot_list[[index + 1]] = p2
          plot_list[[index + 2]] = p3
          plot_list[[index + 3]] = p4
          
          
          
          # if(!is.null(DirName)){
          #   if(Nplot!=1) ggsave(file.path(DirName, paste0(plot_names[plot_num], PlotName, "_", tI, "_byYear.png")), p, width=width, height=height)
          #   if(Nplot==1) ggsave(file.path(DirName, paste0(plot_names[plot_num], PlotName, "_byYear.png")), p, width = width, height = height)
          # }       
          # if(is.null(DirName)){
          #   dev.new()
          #   print(p) 
          # }
        }
        #together <- grid::grid.draw(plot_list)
        if(years == "covariate"){
          together <- ggpubr::ggarrange(plotlist=plot_list, ncol = 2, nrow = 2 ) #for all years
        }
        if(years == "all"){
        together <- ggpubr::ggarrange(plotlist=plot_list, ncol = 5, nrow = 8 ) #for all years
        }
        if(years == "subset"){
        together <- ggpubr::ggarrange(plotlist=plot_list, ncol = 1, nrow = 6 ) #for subset years
        }
        # together2 <- gridExtra::grid.arrange(
        #   grobs = plot_list
        # )
        #ggsave(file = OutFileName, arrangeGrob(grobs = plot_list, ncol = 5))  ## save plot
        #ggsave(file.path(getwd(), paste0(PlotName, "_Total.png")), together,width = 30000, height = 30000, units = "cm",res = 600, device = png,limitsize = FALSE)
        ggsave(file.path(getwd(), paste0(PlotName, "_Total.pdf")), together, device = pdf,width = 70, height = 70, units = "cm")
      }
    }
    
    return( invisible(c(Return1, Return2, Return3)) )
  }
