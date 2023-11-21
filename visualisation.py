# Import libraries
import plotly.express as px
import plotly.graph_objects as go
import pandas as pd

# Get data 
best_lap = pd.read_csv("Best_Lap.csv")
worst_lap = pd.read_csv("Worst_Lap.csv")
filt_left = pd.read_excel("filtered_left.xlsx")
filt_right = pd.read_excel("filtered_right.xlsx")
turns = pd.read_csv("f1sim-ref-turns.csv")

turns = turns.loc[turns['TURN'].isin([1,2])]

# Plotly plot
fig1 = px.scatter(best_lap, x='WORLDPOSX', y='WORLDPOSY', color='LAP_DISTANCE', size='SPEED_KPH', 
                hover_data={'LAP_DISTANCE': True, 'LAP_TIME': True, 'THROTTLE': True, 'BRAKE': True, 'GEAR': True, 'LATERAL': True, 'SPEED_KPH': True, 'ENGINE_RPM': True, 'STEERING': True},
                title='Albert Park Turns 1 & 2 - Best Lap')

# Configurations
fig1.update_layout(
    height = 700,
    width = 1200
)

# Add track features 
track_line_left = go.Scatter(
    x=filt_left['WORLDPOSX'],
    y=filt_left['WORLDPOSY'],
    mode='lines',
    line=dict(color='black', width=2.5),
    showlegend=False
)

track_line_right = go.Scatter(
    x=filt_right['WORLDPOSX'],
    y=filt_right['WORLDPOSY'],
    mode='lines',
    line=dict(color='black', width=2.5),
    showlegend=False
)

track_turns = go.Scatter(
    x=turns['APEX_X1'],
    y=turns['APEX_Y1'],
    mode='markers',
    marker=dict(color='blue', size=10, symbol='cross'),
    name='Turns',
    showlegend=False,
    text=['Turn 1', 'Turn 2'],  
    hoverinfo='text+x+y'
)

fig1.add_trace(track_line_left)
fig1.add_trace(track_line_right)
fig1.add_trace(track_turns)

# Corner labels
labels = [
    dict(
        x=turns['APEX_X1'][0],
        y=turns['APEX_Y1'][0],
        xref="x",
        yref="y",
        text="Turn 1",
        showarrow=True,
        arrowhead=7,
        ax=140,
        ay=-30,
        font=dict(size=14)
    ),
    dict(
        x=turns['APEX_X1'][1],
        y=turns['APEX_Y1'][1],
        xref="x",
        yref="y",
        text="Turn 2",
        showarrow=True,
        arrowhead=7,
        ax=-190,
        ay=-40,
        font=dict(size=14)
    ),

]


fig1.update_layout(
    annotations=labels,
    xaxis=dict(showgrid=False),  
    yaxis=dict(showgrid=False, zeroline=False)
)

fig1.show()

# To html
fig1.write_html("index.html")

# Plotly plot
fig2 = px.scatter(worst_lap, x='WORLDPOSX', y='WORLDPOSY', color='LAP_DISTANCE', size='SPEED_KPH', 
                hover_data={'LAP_DISTANCE': True, 'LAP_TIME': True, 'THROTTLE': True, 'BRAKE': True, 'GEAR': True, 'LATERAL': True, 'SPEED_KPH': True, 'ENGINE_RPM': True, 'STEERING': True},
                title='Albert Park Turns 1 & 2 - Worst Lap')

# Configurations
fig2.update_layout(
    height = 700,
    width = 1200
)

# Add track features 
track_line_left = go.Scatter(
    x=filt_left['WORLDPOSX'],
    y=filt_left['WORLDPOSY'],
    mode='lines',
    line=dict(color='black', width=2.5),
    showlegend=False
)

track_line_right = go.Scatter(
    x=filt_right['WORLDPOSX'],
    y=filt_right['WORLDPOSY'],
    mode='lines',
    line=dict(color='black', width=2.5),
    showlegend=False
)

track_turns = go.Scatter(
    x=turns['APEX_X1'],
    y=turns['APEX_Y1'],
    mode='markers',
    marker=dict(color='blue', size=10, symbol='cross'),
    name='Turns',
    showlegend=False,
    text=['Turn 1', 'Turn 2'],  
    hoverinfo='text+x+y'
)

fig2.add_trace(track_line_left)
fig2.add_trace(track_line_right)
fig2.add_trace(track_turns)

# Corner labels
labels = [
    dict(
        x=turns['APEX_X1'][0],
        y=turns['APEX_Y1'][0],
        xref="x",
        yref="y",
        text="Turn 1",
        showarrow=True,
        arrowhead=7,
        ax=140,
        ay=-30,
        font=dict(size=14)
    ),
    dict(
        x=turns['APEX_X1'][1],
        y=turns['APEX_Y1'][1],
        xref="x",
        yref="y",
        text="Turn 2",
        showarrow=True,
        arrowhead=7,
        ax=-190,
        ay=-40,
        font=dict(size=14)
    ),

]


fig2.update_layout(
    annotations=labels,
    xaxis=dict(showgrid=False),  
    yaxis=dict(showgrid=False, zeroline=False)
)

fig2.show()

# To html
fig2.write_html("index1.html")
