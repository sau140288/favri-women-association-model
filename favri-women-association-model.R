# FAVRI Women's association
# mobile selling of safe vegetables

library(decisionSupport)
make_variables <- function(est, n = 1)
{x <- decisionSupport::random(rho = est, n = n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]), envir = .GlobalEnv)}

make_variables(estimate_read_csv(paste("inputs_women_assoc.csv", sep="")))

source("chance_event.R")
source("vv.R")

# Model ####

women_assoc_function <- function(x, varnames){
  
  # Cost####

  one_time_establishment_costs <- cost_of_establishment_meetings + 
                                  cost_of_promotion
  
  annual_running_costs <- cost_of_space_rent + 
                          cost_of_regular_meetings +
                          cost_of_cleaning_event + 
                          cost_of_communication
  
  # costs over five years
  total_costs <-  vv(annual_running_costs, var_CV = CV_value, 
                        relative_trend = inflation_rate,
                       n = number_of_years)
  
  #assumning that in the first year we establishment costs and running costs
  total_costs[1]<-total_costs[1]+one_time_establishment_costs
  
  
  # Benefits ####
  
  # Add more information about accessibility
  # Is the organization of the intervention aligned with 
  # what people need? 
  
  if(poor_organization >= 0.3 | 
     poor_management >= 0.3 |  
     inconvenient >= 0.3 | 
     family_cannot_travel_too_far >= 0.3 |
     family_cannot_wait >= 0.3 |
     unsuitable_day_of_sale >= 0.3){
    
    if_accessible * organization_access_reduction
    
  } else {
    
    if_accessible
    
  }
  
  # Do people go and get and also eat the veggies?
  
  if(taste_reduced >= 0.3 | 
     family_does_not_like >= 0.3 |  
     family_cannot_afford >= 0.3 | 
     undesirable_change_of_veg >= 0.3 |
     family_cannot_wait >= 0.3 |
     dont_care  >= 0.3 |
     dont_know  >= 0.3 | 
     change_of_veg_source_undesirable  >= 0.3 | 
     already_have_other_source  >= 0.3 | 
     negative_rumors >= 0.3){
    
    if_accessible * organization_access_reduction
    
  } else {
    
    if_accessible
    
  }
  
  
  # here we need to think slightly differently about benefits
  # These are related to the savings in programs ... perhaps, 
  # although that is slightly low in the urban areas
  # because the people of Hanoi are relatively richer than other parts of Vietnam
  # the savings will not be much, the government is busy taking care of very poor people
  # in other areas of Vietnam
  
  healthy_veggies_benefit <- diverse_veg_benefit +  # savings on programs for fresh veggies
                            knowledge_exchange_benefit + # savings on community outreach program
                            gender_equality_benefit + # savings on the costs of gender programs
                            reputation_benefit + # added economic benefit of reputation gains
                            environmental_benefit # savings on environmental programs
  
  total_benefits <-  vv(healthy_veggies_benefit + # savings on health related expenses
                          percentage_of_sales, # revenue from farmer's sales
                                var_CV = CV_value, 
                                n = number_of_years, 
                                relative_trend = inflation_rate) * if_accessible #conditional on access

  
 sales_intervention_result <- total_benefits - total_costs
                        
# Alternative ####
# what might the Women's Assoc. do instead 
 # with the same kinds of outcomes in mind
 # what action would they take to address health, gender etc.?
 # i.e. just connect farmers and consumers and allow htem to handle logistics
 
 expected_cost_of_altervative <- vv(cost_of_establishment_meetings* 0.1, # ten percent of the meetings, but each year
                                    var_CV = CV_value, n = number_of_years, 
                                    relative_trend = inflation_rate)  
 
 expected_benefit_of_altervative <- vv(diverse_veg_benefit,
                                       var_CV = CV_value, n = number_of_years, 
                                       relative_trend = inflation_rate)  
 
 no_intervention_result <- expected_benefit_of_altervative - expected_cost_of_altervative
 
# The difference is inherent in our calculation so we do not need the 
# comparative NPV here, just discount the intervention result
NPV_interv <-
  discount(sales_intervention_result, 
           discount_rate, calculate_NPV = TRUE)

NPV_no_interv <-
  discount(no_intervention_result, 
           discount_rate, calculate_NPV = TRUE)

# Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
# the variables will be called output_1, _2, etc.

return(list(NPV_mobile_sales = NPV_interv,
            NPV_no_interv = NPV_no_interv,
            decision = NPV_interv - NPV_no_interv, #the decision is in the trade off
            Cashflow_mobile_sales = sales_intervention_result))
}

women_assoc_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("inputs_women_assoc.csv"),
  model_function = women_assoc_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

decisionSupport::plot_distributions(mcSimulation_object = women_assoc_results, 
                                    vars = c("NPV_mobile_sales","NPV_no_interv"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = women_assoc_results, 
                                    vars = c("NPV_mobile_sales","NPV_no_interv"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = women_assoc_results, 
                                    vars = "decision",
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

# Cashflow 

plot_cashflow(mcSimulation_object = women_assoc_results, 
              cashflow_var_name = "Cashflow_mobile_sales")

# PLS

pls_result <- plsr.mcSimulation(object = women_assoc_results,
                                resultName = names(women_assoc_results$y)[1], 
                                ncomp = 1)

input_table <- read.csv("inputs_women_assoc.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0)

# EVPI 

#here we subset the outputs from the mcSimulation function (y) 
# by selecting the correct variables
# be sure to run the multi_EVPI only on the variables that the we want
mcSimulation_table <- data.frame(women_assoc_results$x, 
                                 women_assoc_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_mobile_sales")

plot_evpi(evpi, decision_vars = "NPV_mobile_sales")
plot_evpi(evpi, decision_vars = "decision")
