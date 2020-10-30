```Python
import math
import numpy as np
from scipy.stats import poisson
import pandas as pd
import seaborn as sns
import plotly.graph_objs as go
from plotly.offline import iplot
import plotly.io as pio
import kaleido
import os
import cufflinks as cf
cf.go_offline()
cf.set_config_file(world_readable=True, theme='pearl')

os.getcwd() 

import plotly.graph_objects as go
stance=['Pro-war', 'Neutral', 'Anti-war']

fig = go.Figure(data=[
    go.Bar(name='Percent of American Sources', x=stance, y=[71, 26, 3],marker_color='indianred'),
    go.Bar(name='Percent of Total Sources', x=stance, y=[65, 26, 10],marker_color='rgb(55, 83, 109)')
])
# Change the bar mode
fig.update_layout(
  title={'text':"Percent of Iraq War Media Sources",'y':0.9,'x':0.5},
  barmode='group',
  yaxis_title="%",xaxis_title="Stance towards war",
  legend=dict(yanchor='top',y=.99,xanchor='right',x=.99))
fig.show()

fig.write_image("images/FAIRx1.png")
```
