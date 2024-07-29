
install.packages("ReinforcementLearning")
library(ReinforcementLearning)

## Create a simulated environment
## 

# Set seed for reproducibility
set.seed(123)

# Define state space (reservoir levels) and action space (release decisions)
states <- seq(0, 1, by = 0.1)  # Reservoir levels (normalized)
actions <- seq(0, 1, by = 0.2)  # Release decisions (normalized)

# Generate synthetic data
data <- data.frame(State = character(), Action = character(), Reward = numeric(), NextState = character(), stringsAsFactors = FALSE)
for (i in 1:1000) {
  state <- sample(states, 1)
  action <- sample(actions, 1)
  inflow <- runif(1, 0, 0.1)  # Random inflow
  next_state <- min(max(state - action + inflow, 0), 1)  # Ensure next state is within bounds
  reward <- action * 50  # Reward based on electricity price
  data <- rbind(data, data.frame(State = as.character(state), Action = as.character(action), Reward = reward, NextState = as.character(next_state)))
}

# Convert state and action to factors
#data$State <- as.character(data$State)
#data$Action <- as.character(data$Action)
#data$NextState <- as.character(data$NextState)

# Print the first few rows of the generated data
head(data)

# Train the reinforcement learning model
model <- ReinforcementLearning(data, s = "State", a = "Action", r = "Reward", s_new = "NextState", iter = 1000, control = list(alpha = 0.1, gamma = 0.9))

# Print the learned policy
print(model$Policy)

# Print the state-action value function
print(model$Q)

# Define a function to apply the learned policy
apply_policy <- function(state, policy) {
  action <- policy[state]
  return(as.numeric(levels(action))[action])
}

# Example usage of the learned policy
current_state <- as.character(0.5)  # Example starting state
optimal_action <- apply_policy(current_state, model$Policy)
print(optimal_action)




