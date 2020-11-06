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

fig = go.Figure(go.Scatter(x=[0,1,2,0,None,3,3,5,5,3], y=[0,2,0,0,None,0.5,1.5,1.5,0.5,0.5], fill="toself"))
fig.show()

fig2= go.Figure(go.Scatter(x=[0,2,1,0],y=[0,0,3,0],fill="toself"))
fig2.update_xaxes(showticklabels=False)
fig2.update_yaxes(showticklabels=False)
fig2.update_xaxes(showgrid=False,zeroline=False)
fig2.update_yaxes(showgrid=False,zeroline=False)
fig2.show()

fig.write_image("images/FAIRx1.png")
```
