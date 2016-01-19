Z„STATS data;avg
©DATA „… vector of numbers
©Z    „… (MEAN)(VARIANCE) 
data„,data © make sure variable is a vector
:if 1=½data
    Z„((+/data)÷½data) Ð    © Pass mean only if only one datum
:else
    Z  „(+/data)÷½data      © Pass mean..
    Z  „Z,((+/((data-Z)*2))÷((½data)-1))÷½data ©.. and mean variance
:endif