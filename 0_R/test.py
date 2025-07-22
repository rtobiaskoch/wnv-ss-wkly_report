import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Distributions
x = np.linspace(-4, 8, 1000)
h0 = stats.norm.pdf(x, loc=0, scale=1)  # H₀: μ=0, σ=1
h1 = stats.norm.pdf(x, loc=3, scale=1)  # H₁: μ=3, σ=1
critical_value = stats.norm.ppf(0.95)   # α=0.05 (1-tailed)

# Observed statistic (e.g., t=2.5)
observed = 2.5
p_value = 1 - stats.norm.cdf(observed, loc=0, scale=1)

# Plot
plt.plot(x, h0, label='Null (H₀)')
plt.plot(x, h1, label='Alternative (H₁)')
plt.axvline(critical_value, linestyle='--', color='black', label=f'Critical Value (α=0.05)')
plt.axvline(observed, linestyle='-', color='purple', label=f'Observed (t={observed})')

# Shade p-value (area under H₀ beyond observed)
plt.fill_between(x, h0, where=(x >= observed), color='blue', alpha=0.5, label=f'p-value = {p_value:.3f}')

# Shade Type I/II errors
plt.fill_between(x, h0, where=(x >= critical_value), color='blue', alpha=0.2, label='Type I (α)')
plt.fill_between(x, h1, where=(x <= critical_value), color='red', alpha=0.2, label='Type II (β)')

plt.legend()
plt.xlabel('Test Statistic')
plt.ylabel('Probability Density')
plt.title('p-value, Type I/II Errors, and Power')
plt.show()