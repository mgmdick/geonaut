library(shiny)
library(ggplot2)


x <- seq(0, 5000, 50)
y <- seq(0, 5000, 50)




ob_cost <- 10
co_rd <- 1.4
co_price <- 100

res <- generate_plane(x, y)
res <- margin_rank(res, co_rd = co_rd, ob_cost = ob_cost, co_price = co_price)


starting_energy_level <- 200
corer_cost <- 1
drone_cost <- 0.25

plot_themes <- theme(legend.position = "bottom",
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     plot.background = element_rect(fill = "#384779", colour = "#384779"),
                     legend.background = element_rect(fill = "#384779", colour = "#384779"),
                     legend.text = element_text(colour = "white", size = 15), 
                     strip.background = element_rect(fill = "#384779", colour = "#384779"), 
                     strip.text = element_text(colour = "white", size = 15)) 


shinyServer(function(input, output) {
    
    output$tenement_drill_plot <- renderPlot({
        
        plt <- ggplot(values$base_model %>% mutate(class = ifelse(class %in% c("Weathered", "Fresh"), "Emilarium Rich Zone", "No Detection")), aes(x,  y, fill = class)) +
            geom_raster(alpha = 0.5) +
            scale_fill_manual(name = "", values = scifi_palette) +
            xlab("") + ylab("") +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) +
            plot_themes
        if (nrow(values$drill_locations) > 0) plt <- plt + geom_point(data = values$drill_locations, colour = "red", size = 3, inherit.aes =F, aes(x = x, y = y))
        
        if (nrow(values$seismic_locations) > 0) {
          plt <- plt + geom_path(data = values$seismic_locations, size = 3, inherit.aes = F, show.legend = F, colour = "grey", aes(x = x, y = y, group = linegroup)) + 
            geom_point(data = values$seismic_locations,  size = 3, inherit.aes = F, aes(x = x, y = y), show.legend = F)
        }
        
        plt 
    })
    
    output$tenement_seismic_plot <- renderPlot({
        
        plt <-ggplot(values$base_model %>% mutate(class = ifelse(class %in% c("Weathered", "Fresh"), "Emilarium Rich Zone", "No Detection")), aes(x,  y, fill = class)) +
          geom_raster(alpha = 0.5) +
          scale_fill_manual(name = "", values = scifi_palette) +
          xlab("") + ylab("") +
          scale_x_continuous(expand = c(0,0)) +
          scale_y_continuous(expand = c(0,0)) +
          plot_themes
        if (nrow(values$seismic_locations) > 0) {
            plt <- plt + geom_path(data = values$seismic_locations, size = 3, inherit.aes = F, show.legend = F, aes(x = x, y = y, group = linegroup, colour = factor(linegroup))) + 
                geom_point(data = values$seismic_locations,  size = 3, inherit.aes = F, aes(x = x, y = y, group = linegroup, colour = factor(linegroup)), show.legend = F)
        }
        
        if (nrow(values$drill_locations) > 0) plt <- plt + geom_point(data = values$drill_locations, colour = "grey", size = 3, inherit.aes =F, aes(x = x, y = y))
        
        
        #if (nrow(values$seismic_locations)  %% 2 == 0 & nrow(values$seismic_locations)  > 0) {
        #    plt <- plt + geom_path(data = values$seismic_lines, size = 3, inherit.aes = F, show.legend = F, aes(x = x, y = y, group = linegroup, colour = factor(linegroup)))
        #}
        
        
        plt
    })
    
    output$seismic_line_plot <- renderPlot({
      
      validate(
        need(nrow(values$seismic_locations) > 0, "No scan-lines - deploy drones")
      )
      
      validate(
        need(nrow(values$seismic_locations) %% 2 == 0, "Provide drone end point")
      )
      
        if (nrow(values$seismic_locations) > 0) {
        if (nrow(values$seismic_locations) %% 2 == 0) {
            print(values$seismic_locations)
            #plt <- ggplot(values$seismic_lines, aes(x = dist, y = z, group = linegroup, colour = factor(linegroup))) + geom_path() + facet_wrap(~linegroup)+
            #    theme(legend.position = "bottom")
            
              plt <- ggplot(values$seismic_lines) + geom_ribbon(aes(x = dist, ymin = z + (tk/2), ymax = z - (tk/2)), fill = scifi_palette[1]) + facet_wrap(~linegroup, scale = "free_x") +
                geom_point(aes(x = dist, y = z, colour = cut(wline, c(-0.1, 0, 0.5, 1.1))), size = 4) + 
                geom_ribbon(aes(x = dist, ymax = 0, ymin = z + (tk/2)), fill = scifi_palette[2]) +
                scale_color_manual(values = c("red", "forestgreen")) +
                xlab("") + ylab("") +
                scale_x_continuous(expand = c(0,0)) +
                scale_y_continuous(expand = c(0,0)) +
                plot_themes
            plt
        }
        }
        
    })
    
    output$idw_z_plot <- renderPlot({
      validate(
        need(nrow(values$model_results) > 0, "No exploration has commenced - deploy nano-corers")
      )
        if (nrow(values$model_results ) > 0) {
            print(values$seismic_locations)
            plt <- ggplot(values$model_results , aes(x = x, y = y, fill = profit > 0))   +
              geom_raster(alpha = 0.8) +
              scale_fill_manual(name = "", values = colorRampPalette(scifi_palette[c(2,1)])(2)) +
              xlab("") + ylab("") +
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0)) +
              plot_themes
            plt
        }
        
    })
    
    output$idw_tk_plot <- renderPlot({
      validate(
        need(nrow(values$model_results) > 0, "No exploration has commenced - deploy nano-corers")
      )
      
        if (nrow(values$model_results ) > 0) {
            print(values$seismic_locations)
            plt <- ggplot(values$model_results , aes(x = x, y = y, fill = cut(tk, 10))) +
              geom_raster(alpha = 0.8) +
              scale_fill_manual(name = "", values = colorRampPalette(scifi_palette[c(3,1)])(10)) +
              xlab("") + ylab("") +
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0)) +
              plot_themes
            plt
        }
    })
    
    
    #  output$nvp_model <- renderDataTable({
    #      #print("INFOBOX")
    #      #res <- margin_rank(values$model_results, weathering_rl = input$weathering_rl,final_depth = input$final_depth, ob_cost = 25)
    #      #fin_data <- mine_reserve(res, excavators = input$excavator_count)
    #      #data.frame(fin_data)
    #      mtcars
    #  })
    # 
    #  output$model_npv <- renderInfoBox({
    #      print("INFOBOX")
    #      res <- margin_rank(values$model_results, weathering_rl = input$weathering_rl,final_depth = input$final_depth, ob_cost = 25)
    #      fin_data <- mine_reserve(res, excavators = input$excavator_count)
    #      infoBox("Model NPV", value = fin_data$NPV_M)
    #   
    #  })
    #  
    # 
    # 
    # output$actual <- renderInfoBox({
    #     fin_data <- mine_reserve(res, excavators = input$excavator_count, optimal = T)
    #     infoBox("Actual NPV", value = fin_data$NPV_M)
    # })
    # 
    
    
    output$idw_wline_plot <- renderPlot({
      validate(
        need(nrow(values$model_results) > 0, "No exploration has commenced - deploy nano-corers")
      )
        if (nrow(values$model_results ) > 0) {
            print(values$seismic_locations)
            plt <- ggplot(values$model_results, aes(x = x, y = y, fill = cut(z, 5)))+
              geom_raster(alpha = 0.9) +
              scale_fill_manual(name = "", values = colorRampPalette(scifi_palette[c(1,3)])(5)) +
              geom_point(data = values$model_results %>% filter(profit > 0 & z > -input$final_depth), colour = "purple", size = 6, inherit.aes = F, aes(x = x, y = y)) +
              xlab("") + ylab("") +
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0)) +
              plot_themes
            
            #plt <- plt + geom_line(data = values$model_results %>% filter(abs(z + input$weathering_rl) < 5 & !is.na(z)) %>% arrange(x) %>% group_by(x) %>% summarise(y = median(y, na.rm = T)), colour = "red", inherit.aes =F, aes(x = x, y = y))
            #plt <- plt + geom_point(data = values$model_results %>% filter(z < -input$weathering_rl & z > -input$final_depth), colour = "greenyellow", inherit.aes =F, aes(x = x, y = y))

            plt
        }
        
    })

    
    values <- reactiveValues("drill_locations" = data.frame(),
                             "seismic_locations" = data.frame(),
                             "seismic_lines" = data.frame(),
                             "model_results" = data.frame(),
                             "base_model" = res)
    
    
    observeEvent(input$drill_click, {
      if (energy_balance() >= 0) {
        values$drill_locations <- rbind(values$drill_locations, nearPoints(values$base_model, input$drill_click, threshold = 50)[1,])
      }
    })
    
    
    observeEvent(input$explore_click, {
      if (energy_balance() >= 0 & nrow(values$drill_locations) > 0) {
        #values$model_results <- idw_grid(values$base_model, values$drill_locations)
        values$model_results <- krige_grid(values$base_model, values$drill_locations)
        values$model_results <- margin_rank(values$model_results,  co_rd = co_rd, ob_cost = ob_cost, co_price = co_price)
      }
    })
    
    
    observeEvent(input$seismic_click, {
        
        if (energy_balance() >= 0) {
        values$seismic_locations <- rbind(values$seismic_locations, cbind(nearPoints(values$base_model, input$seismic_click, threshold = 50)[1,],linegroup = NA))
        #values$seismic_locations$line <- rep(1:2, nrow(seismic_locations))
        tmp <- values$seismic_locations
        linegroup <- sort(rep(1:(nrow(tmp)/2), 2))
        linegroup <- linegroup[1:nrow(tmp)]
        tmp$linegroup <- linegroup
        
        values$seismic_locations <- tmp
        print(values$seismic_locations)
        if (nrow(values$seismic_locations) %% 2 == 0) {
            values$seismic_lines <- nearPointsLine2(values$base_model, tmp)
        }
        }
    })
    
    
    output$progress_plot <- renderPlot({
      
      validate(
        need(nrow(values$model_results) > 0, "No exploration has commenced - deploy nano-corers")
      )
      
        if (nrow(values$model_results) > 0) {
        res <- margin_rank(values$model_results, final_depth = input$final_depth, ob_cost = ob_cost)
        plt <- mine_reserve_plot(res, excavators = input$excavator_count)
        plt <- plt + plot_themes +
          scale_fill_manual(values = scifi_palette[1]) +
          scale_colour_manual(values = scifi_palette[1]) +
          xlab("YEAR") + ylab("ENERGY ") +
          theme(plot.background = element_rect(fill = "#384779"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#384779")) 
        
        
        plt
        }
    })
    
    observeEvent(input$new_quadrant, {
      
      values$base_model <- generate_plane(x, y)
      values$base_model <- margin_rank(values$base_model, co_rd = co_rd, ob_cost = ob_cost, co_price = co_price)
      values$drill_locations = data.frame()
      values$seismic_locations = data.frame()
      values$seismic_lines = data.frame()
      values$model_results = data.frame()

    })
    
    
    
    energy_levels <- reactive({
       
       Type = c("Remaining Exploration Energy", "Drone Depletion", "Corer Depletion")
       
       drone_total_cost = (nrow(values$seismic_locations) * 20 * drone_cost)
       corer_total_cost = (nrow(values$drill_locations) * corer_cost)
       
       Values = c(starting_energy_level - (corer_total_cost + drone_total_cost), drone_total_cost, corer_total_cost)
     
       data.frame(Type, Values)
       
     })
    # 
    # 
     energy_balance <- reactive({
       drone_total_cost = (nrow(values$seismic_locations) * 20 * drone_cost)
       corer_total_cost = (nrow(values$drill_locations) * corer_cost)
       as.numeric(starting_energy_level - (corer_total_cost + drone_total_cost))
     })
    # 
    # 
     output$energy_plot <- renderPlot({
       
       ggplot(energy_levels(), aes(x = "Energy", y = Values, fill = Type)) + geom_bar(stat = "identity", position = "stack") + plot_themes +
         theme(plot.background = element_rect(fill = "#384779"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "#384779")) +
         scale_fill_manual(values = c(scifi_palette[4],scifi_palette[2],scifi_palette[3])) +
         coord_flip() + xlab("") + ylab("")
       
     })
    # 
    
})
