`distribute()` allows to consider multiple user cases. The default EVs have different parameters to reflect this: 

1. A user with a large battery half-full `soc = 40, vol = 75`, charges at high rate `cap = 20`. The owner is not in a hurry to charge `level = 1` (The higher *level*, the earlier the EV charges).
2. Average car almost full `soc = 40, vol = 50, cap = 12`; the user is a doctor, and must have the battery filled as soon as possible `level = 3`. 
3. Small utilitarian for daily commutes `soc = 15, vol = 30, cap = 10`; the owner pays less in exchange for lower charging priority `level = 0`.
4. Average car with the battery almost empty `soc = 5, vol = 50, cap = 12`. The user is going on a long trip soon, so has higher priority `level = 2`.
5. Taxi driver. The battery is quite full `soc = 30, vol = 40` but it is convenient to have it complete `level = 2`; the EV can charge at a faster rate `cap = 20`.

Try to change the parameters to simulate your own user scenarios. The upper graph shows how the SOC of the batteries evolve over time (once they are full, the line is finished). The lower graph shows the portion of grid capacity (that can be randomized) allocated to each EV.
