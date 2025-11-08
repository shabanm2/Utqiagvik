import glob
import json
import os
import re
import time
import requests
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

import pandas as pd
import plotly.graph_objects as go
import pydeck as pdk
import streamlit as st
from plotly.subplots import make_subplots

st.set_page_config(
    page_title="PM in Barrow",
    layout="wide"
)

@st.cache_data(ttl=3600)
def get_data(path):
    data = pd.read_csv(path)
    
    data['Time'] = data['Time'].astype(str).apply(lambda x: x if re.search(r'\d{2}:\d{2}:\d{2}', x) else x + " 00:00:00")
    data['datetime'] = pd.to_datetime(data['Time'], errors='coerce')
    data = data.drop(columns=['Time'])

    node_coords = {
        'node-03': (71.29694, -156.77167),
        'node-05': (71.29024, -156.76910),
        'node-07': (71.32540, -156.68292),
        'node-08': (71.30307, -156.69313),
        'node-09': (71.29907, -156.73303),
        'node-10': (71.29691, -156.75430),
        'node-11': (71.28786, -156.77478),
        'node-12': (71.28818, -156.76040),
        'node-13': (71.33214, -156.66382),
        'node-14': (71.31150, -156.73250),
        'node-15': (71.28861, -156.76972),
        'node-16': (71.29911, -156.75789),
        'node-17': (71.30686, -156.74705),
        'node-18': (71.30215, -156.76259),
    }

    default_lat, default_lon = 71.29604, -156.77962

    data['lat'] = data['node_number'].map(lambda n: node_coords.get(n, (default_lat, default_lon))[0]).astype(float)
    data['lon'] = data['node_number'].map(lambda n: node_coords.get(n, (default_lat, default_lon))[1]).astype(float)

    data = data.dropna(subset=['value', 'lat', 'lon', 'datetime'])
    pm25 = data[data['data_type'] == 'PM2_5'].dropna(subset=['value']).copy()
    pm25 = pm25[pm25['value'] < 500]
    pm10 = data[data['data_type'] == 'PM10_0'].dropna(subset=['value']).copy()
    pm10 = pm10[pm10['value'] < 500]
    temps = data[data['data_type'] == 'Temperature'].dropna(subset=['value']).copy()
    
    merged = pd.merge(pm25, pm10, on=['datetime', 'node_number', 'lat', 'lon'], suffixes=('_pm25', '_pm10'))   

    return merged, temps

merged, temps = get_data('../PM/25/fullsummer25_PM_AND_Temp_dataset.csv')
merged['datetime'] = pd.to_datetime(merged['datetime'])
merged = merged.sort_values('datetime')

THRESH_PM25 = 15  # µg/m³
THRESH_PM10 = 45  # µg/m³

merged['exceed_pm25'] = merged['value_pm25'] > THRESH_PM25
merged['exceed_pm10'] = merged['value_pm10'] > THRESH_PM10

merged['week'] = merged['datetime'].dt.to_period('W').apply(lambda r: r.start_time)
grouped = (
    merged.groupby('week', as_index=False)[['exceed_pm25', 'exceed_pm10']]
    .sum()
)
st.title("Particulate Matter in Barrow, Alaska")

fig = go.Figure()

fig.add_trace(go.Scatter(
    x=grouped['week'],
    y=grouped['exceed_pm25'],
    mode='lines+markers',
    name='PM2.5',
    line=dict(color='blue', width=2),
    marker=dict(size=6),
))

fig.add_trace(go.Scatter(
    x=grouped['week'],
    y=grouped['exceed_pm10'],
    mode='lines+markers',
    name='PM10',
    line=dict(color='green', width=2),
    marker=dict(size=6),
))

fig.update_layout(
    title='Weekly WHO PM Limit Exceedances in Barrow, AK',
    xaxis_title='Week of Year',
    yaxis_title='Number of Exceedances Recorded',
    template='plotly_white',
    height=400,
)

st.plotly_chart(fig, use_container_width=True)


def naaqs_pm25_color(density):
    """Return RGBA color based on PM2.5 category."""
    if density <= 12:       # Low
        return [0, 255, 0, 200]      # Green
    elif density <= 25:     # Moderate
        return [255, 255, 0, 200]    # Yellow
    elif density <= 34:     # High
        return [255, 165, 0, 200]    # Orange
    else:                   # Exceedance
        return [255, 0, 0, 200]      # Red


def naaqs_pm10_color(density):
    """Return RGBA color based on PM10 category."""
    if density <= 50:       # Low
        return [0, 255, 0, 200]      # Green
    elif density <= 100:    # Moderate
        return [255, 255, 0, 200]    # Yellow
    elif density <= 149:    # High
        return [255, 165, 0, 200]    # Orange
    else:                   # Exceedance
        return [255, 0, 0, 200]      # Red

def who_pm25_color(density):
    """Return RGBA color based on WHO PM2.5 categories."""
    if density <= 5:        # Low
        return [0, 255, 0, 200]      # Green
    elif density <= 10:     # Moderate
        return [255, 255, 0, 200]    # Yellow
    elif density <= 14:     # High
        return [255, 165, 0, 200]    # Orange
    else:                   # Exceedance
        return [255, 0, 0, 200]      # Red


def who_pm10_color(density):
    """Return RGBA color based on WHO PM10 categories."""
    if density <= 15:       # Low
        return [0, 255, 0, 200]      # Green
    elif density <= 30:     # Moderate
        return [255, 255, 0, 200]    # Yellow
    elif density <= 44:     # High
        return [255, 165, 0, 200]    # Orange
    else:                   # Exceedance
        return [255, 0, 0, 200]      # Red


merged['color_pm25_who'] = merged['value_pm25'].apply(lambda x: who_pm25_color(x))
merged['color_pm10_who'] = merged['value_pm10'].apply(lambda x: who_pm10_color(x))
merged['color_pm25_naaqs'] = merged['value_pm25'].apply(lambda x: naaqs_pm25_color(x))
merged['color_pm10_naaqs'] = merged['value_pm10'].apply(lambda x: naaqs_pm10_color(x))

selected_time = st.slider(
    "Select Datetime",
    min_value=merged['datetime'].min().to_pydatetime(),
    max_value=merged['datetime'].max().to_pydatetime(),
    value=merged['datetime'].max().to_pydatetime(),
    step=timedelta(hours=1),
    format="YYYY-MM-DD HH:mm"
)

def create_legend_html(gradient: str, ticks: list[tuple[str, float]] = None) -> str:
    """
    Create HTML for color legends with optional tick labels aligned to gradient divisions.
    ticks: list of (label, position_percent) where position_percent is 0–100.
    Example: [("0", 0), ("15", 33), ("30", 66), ("44", 85), ("45+", 100)]
    """
    tick_html = ""
    if ticks:
        tick_html = "<div style='position: relative; height: 18px; margin-top: 3px;'>"
        for label, pos in ticks:
            tick_html += (
                f"<span style='position: absolute; left: {pos}%; "
                f"transform: translateX(-50%); font-size: 0.8em;'>{label}</span>"
            )
        tick_html += "</div>"

    return f"""
    <div style="display: flex; flex-direction: column; align-items: stretch; margin-top: 8px;">
        <div style="position: relative; height: 20px; background: {gradient}; border: 1px solid #ccc;"></div>
        {tick_html}
    </div>
    """

col1, col2 = st.columns([1, 1])
with col1:
    st.subheader("WHO PM Standards")

    st.markdown("**PM10 (Outer)**")
    st.markdown(
        create_legend_html(
            "linear-gradient(to right, "
            "rgb(0,255,0) 0%, rgb(0,255,0) 33%, "
            "rgb(255,255,0) 33%, rgb(255,255,0) 66%, "
            "rgb(255,165,0) 66%, rgb(255,165,0) 85%, "
            "rgb(255,0,0) 85%, rgb(255,0,0) 100%)",
            ticks=[("0", 0), ("15", 33), ("30", 66), ("44", 85), ("45+", 100)]
        ),
        unsafe_allow_html=True
    )

    st.text("PM2.5 (Inner)")
    st.markdown(
        create_legend_html(
            "linear-gradient(to right, "
            "rgb(0,255,0) 0%, rgb(0,255,0) 25%, "
            "rgb(255,255,0) 25%, rgb(255,255,0) 55%, "
            "rgb(255,165,0) 55%, rgb(255,165,0) 85%, "
            "rgb(255,0,0) 85%, rgb(255,0,0) 100%)",
            ticks=[("0", 0), ("5", 25), ("10", 55), ("14", 85), ("15+", 100)]
        ),
        unsafe_allow_html=True
    )

with col2:
    st.subheader("NAAQS PM Standards")

    st.markdown("**PM10 (Outer)**")
    st.markdown(
        create_legend_html(
            "linear-gradient(to right, "
            "rgb(0,255,0) 0%, "          # 0
            "rgb(0,255,0) 33%, "         # ~50
            "rgb(255,255,0) 33%, "       
            "rgb(255,255,0) 66%, "       # ~100
            "rgb(255,165,0) 66%, "
            "rgb(255,165,0) 90%, "       # ~149
            "rgb(255,0,0) 90%, "
            "rgb(255,0,0) 100%)",        # 150+
            ticks=[("0", 0), ("50", 33), ("100", 60), ("149", 90), ("150+", 100)]
        ),
        unsafe_allow_html=True
    )

    st.text("PM2.5 (Inner)")
    st.markdown(
        create_legend_html(
            "linear-gradient(to right, "
            "rgb(0,255,0) 0%, "          # 0
            "rgb(0,255,0) 34%, "         # 12
            "rgb(255,255,0) 34%, "
            "rgb(255,255,0) 70%, "       # 25
            "rgb(255,165,0) 70%, "
            "rgb(255,165,0) 90%, "       # 34
            "rgb(255,0,0) 90%, "
            "rgb(255,0,0) 100%)",        # 35+
            ticks=[("0", 0), ("12", 34), ("25", 70), ("34", 90), ("35+", 100)]
        ),
        unsafe_allow_html=True
    )


limit = st.selectbox("Select PM Limit: ", options=["WHO", "NAAQS"])

if limit == "WHO":
    slice = merged[merged['datetime'] == selected_time].copy()
    slice = slice[['lat', 'lon', 'value_pm25', 'value_pm10', 'color_pm25_who', 'color_pm10_who']]

    st.pydeck_chart(
        pdk.Deck(
            initial_view_state=pdk.ViewState(
                latitude = 71.314285,
                longitude = -156.710406,
                zoom = 11,
                pitch = 0,
            ),
            map_style=None,
            layers = [
                pdk.Layer(
                    "ScatterplotLayer",
                    data=slice,
                    get_position="[lon, lat]",
                    get_fill_color = "color_pm25_who",
                    get_radius=140,
                    opacity = 0.6,
                    pickable = True,
                ),
                pdk.Layer(
                    "ScatterplotLayer",
                    data=slice,
                    get_position="[lon, lat]",
                    get_fill_color = "color_pm10_who",
                    get_radius=80,
                    pickable = True,
                )
            ],
            tooltip={
                "html": "<b>PM25:</b> {value_pm25} <br/>"
                    "<b>PM10:</b> {value_pm10} <br/>",
                "style": {
                    "backgroundColor": "steelblue",
                    "color": "white",
                    "fontSize": "12px"
                    }
            }
        )
    )
else:
    slice = merged[merged['datetime'] == selected_time].copy()
    slice = slice[['lat', 'lon', 'value_pm25', 'value_pm10', 'color_pm25_naaqs', 'color_pm10_naaqs']]

    st.pydeck_chart(
        pdk.Deck(
            initial_view_state=pdk.ViewState(
                latitude = 71.314285,
                longitude = -156.710406,
                zoom = 11,
                pitch = 0,
            ),
            layers = [
                pdk.Layer(
                    "ScatterplotLayer",
                    data=slice,
                    get_position="[lon, lat]",
                    get_fill_color = "color_pm25_naaqs",
                    get_radius=140,
                    opacity = 0.6,
                    pickable = True,
                ),
                pdk.Layer(
                    "ScatterplotLayer",
                    data=slice,
                    get_position="[lon, lat]",
                    get_fill_color = "color_pm10_naaqs",
                    get_radius=80,
                    pickable = True,
                )
            ],
            tooltip={
                "html": "<b>PM25:</b> {value_pm25} <br/>"
                    "<b>PM10:</b> {value_pm10} <br/>",
                "style": {
                    "backgroundColor": "steelblue",
                    "color": "white",
                    "fontSize": "12px"
                    }
            },
            map_style="open-street-map",
        )
    )