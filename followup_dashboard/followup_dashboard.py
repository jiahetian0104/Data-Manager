"""
Responsive follow-up dashboard built with Dash.

How to run locally:
1. Install packages if needed:
   pip install dash pandas plotly
2. Update DATA_PATH below if needed.
3. Run:
   python followup_dashboard.py
4. Open the local URL shown in the terminal, usually:
   http://127.0.0.1:8050

This app reads dashboard_detail.csv and computes metrics in the same spirit as
summarise_dashboard() from the R pipeline:
- denominator = sum(eligible_flag)
- numerator   = sum(Score among eligible records)
- progress    = numerator / denominator

New in this version:
- A control to include or exclude Potential Participants.
- An Overall row in the summary table for each staff member.
- An Overall option in the Event filter.
"""

from pathlib import Path
import pandas as pd
import numpy as np

from dash import Dash, dcc, html, dash_table, Input, Output
import plotly.express as px

# =========================
# Configuration
# =========================
DATA_PATH = Path(
    "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/"
    "Data Manager/Data-Manager/Follow-up Dashboard/dashboard_detail.csv"
)
TITLE = "Follow-up Dashboard"
POTENTIAL_LABEL = "Potential Participants"

# =========================
# Load data
# =========================
df = pd.read_csv(DATA_PATH)

# Standardize types
if "eligible_flag" in df.columns:
    df["eligible_flag"] = pd.to_numeric(df["eligible_flag"], errors="coerce")
else:
    raise ValueError("dashboard_detail.csv must contain eligible_flag.")

if "Score" in df.columns:
    df["Score"] = pd.to_numeric(df["Score"], errors="coerce")
else:
    raise ValueError("dashboard_detail.csv must contain Score.")

for col in ["staff", "event_short", "statusId", "Outcome", "status_2026_std"]:
    if col in df.columns:
        df[col] = df[col].astype("string")

# Prefer the call-list status if available, because Potential Participants are
# created from the call-list status standardization in the R code.
if "status_2026_std" in df.columns:
    df["participant_status"] = df["status_2026_std"].astype("string")
elif "statusId" in df.columns:
    df["participant_status"] = df["statusId"].astype("string")
else:
    df["participant_status"] = pd.Series(pd.NA, index=df.index, dtype="string")

# Keep the original status filter behavior using statusId when available.
# If statusId is not available, fall back to participant_status.
STATUS_FILTER_COL = "statusId" if "statusId" in df.columns else "participant_status"

# =========================
# Helper functions
# =========================
def apply_filters(data, staff_value, event_value, status_value, include_potential):
    """Apply dashboard controls before calculating metrics."""
    out = data.copy()

    if not include_potential:
        out = out[out["participant_status"].ne(POTENTIAL_LABEL) | out["participant_status"].isna()]

    if staff_value and staff_value != "All":
        out = out[out["staff"] == staff_value]

    # Event == Overall means do not filter by event. The overall metric is
    # calculated across all event_short rows after the other filters are applied.
    if event_value and event_value not in ["All", "Overall"]:
        out = out[out["event_short"] == event_value]

    if status_value and status_value != "All":
        out = out[out[STATUS_FILTER_COL] == status_value]

    return out


def calculate_metrics(data):
    """Return denominator, numerator, and progress for a filtered data frame."""
    denominator = int(data["eligible_flag"].fillna(0).sum())
    numerator = float(
        np.where(
            data["eligible_flag"].fillna(0).eq(1),
            data["Score"].fillna(0),
            0,
        ).sum()
    )
    progress = np.nan if denominator == 0 else numerator / denominator
    return denominator, numerator, progress


def summarize_dashboard(data, group_cols):
    """Generic summary matching the R denominator/numerator/progress logic."""
    if data.empty:
        return pd.DataFrame(columns=group_cols + ["denominator", "numerator", "progress"])

    out = (
        data.groupby(group_cols, dropna=False)
        .apply(lambda x: pd.Series(calculate_metrics(x), index=["denominator", "numerator", "progress"]))
        .reset_index()
    )
    return out


def summarize_by_staff(data):
    out = summarize_dashboard(data, ["staff"])
    if out.empty:
        return out
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_by_event(data):
    out = summarize_dashboard(data, ["event_short"])
    if out.empty:
        return out
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_staff_event(data, add_overall=True):
    if data.empty:
        return pd.DataFrame(columns=["staff", "event_short", "denominator", "numerator", "progress"])

    summary_by_event = summarize_dashboard(data, ["staff", "event_short"])

    if not add_overall:
        return summary_by_event.sort_values(["staff", "event_short"])

    summary_overall = summarize_dashboard(data, ["staff"])
    summary_overall["event_short"] = "Overall"
    summary_overall = summary_overall[["staff", "event_short", "denominator", "numerator", "progress"]]

    out = pd.concat([summary_by_event, summary_overall], ignore_index=True)
    return out.sort_values(["staff", "event_short"])


def pct(x):
    if pd.isna(x):
        return "NA"
    return f"{x:.1%}"


def format_number(x):
    if pd.isna(x):
        return "NA"
    x = float(x)
    return f"{int(x):,}" if x.is_integer() else f"{x:,.1f}"


def make_bar_staff(data):
    sdf = summarize_by_staff(data)
    fig = px.bar(
        sdf,
        x="staff",
        y="progress",
        hover_data=["denominator", "numerator"],
        title="Overall Progress by Staff",
    )
    fig.update_layout(
        template="plotly_white",
        margin=dict(l=20, r=20, t=60, b=20),
        yaxis_tickformat=".0%",
        xaxis_title="Staff",
        yaxis_title="Progress",
        height=420,
    )
    return fig


def make_bar_event(data):
    sdf = summarize_by_event(data)
    fig = px.bar(
        sdf,
        x="event_short",
        y="progress",
        hover_data=["denominator", "numerator"],
        title="Progress by Event",
    )
    fig.update_layout(
        template="plotly_white",
        margin=dict(l=20, r=20, t=60, b=80),
        yaxis_tickformat=".0%",
        xaxis_title="Event",
        yaxis_title="Progress",
        height=420,
    )
    fig.update_xaxes(tickangle=45)
    return fig


def make_heatmap(data):
    sdf = summarize_staff_event(data, add_overall=False)
    if sdf.empty:
        return px.imshow([[None]], text_auto=False, title="Staff × Event Progress")

    pivot = sdf.pivot(index="staff", columns="event_short", values="progress")
    fig = px.imshow(
        pivot,
        aspect="auto",
        color_continuous_scale="Blues",
        text_auto=".0%",
        title="Staff × Event Progress",
    )
    fig.update_layout(
        template="plotly_white",
        margin=dict(l=20, r=20, t=60, b=40),
        height=520,
    )
    return fig


def prep_summary_table(data):
    sdf = summarize_staff_event(data, add_overall=True).copy()
    if sdf.empty:
        return sdf
    sdf["denominator"] = sdf["denominator"].astype("int64")
    sdf["numerator"] = sdf["numerator"].map(format_number)
    sdf["progress"] = sdf["progress"].map(pct)
    return sdf


def prep_detail_table(data):
    preferred_cols = [
        "staff",
        "child_echo_id",
        "PIN",
        "participant_status",
        "statusId",
        "event_short",
        "eligible_flag",
        "Outcome",
        "Score",
        "age_at_caregiver_completion",
        "source_sheet",
    ]
    cols = [c for c in preferred_cols if c in data.columns]
    remaining_cols = [c for c in data.columns if c not in cols]
    return data[cols + remaining_cols].copy()


# =========================
# App
# =========================
app = Dash(__name__)
server = app.server

staff_options = ["All"] + sorted([x for x in df["staff"].dropna().unique().tolist()])
event_options = ["All", "Overall"] + sorted([x for x in df["event_short"].dropna().unique().tolist()])
status_options = ["All"] + sorted([x for x in df[STATUS_FILTER_COL].dropna().unique().tolist()])

app.layout = html.Div(
    [
        html.Div(
            [
                html.H1(TITLE, style={"margin": "0 0 6px 0"}),
                html.Div(
                    "Responsive review version for metric validation",
                    style={"color": "#555"},
                ),
            ],
            style={"padding": "18px 20px 8px 20px"},
        ),

        html.Div(
            [
                html.Div(
                    [
                        html.Label("Staff"),
                        dcc.Dropdown(
                            id="staff-filter",
                            options=[{"label": x, "value": x} for x in staff_options],
                            value="All",
                            clearable=False,
                        ),
                    ],
                    style={"flex": "1", "minWidth": "220px"},
                ),
                html.Div(
                    [
                        html.Label("Event"),
                        dcc.Dropdown(
                            id="event-filter",
                            options=[{"label": x, "value": x} for x in event_options],
                            value="All",
                            clearable=False,
                        ),
                    ],
                    style={"flex": "1", "minWidth": "220px"},
                ),
                html.Div(
                    [
                        html.Label("Status"),
                        dcc.Dropdown(
                            id="status-filter",
                            options=[{"label": x, "value": x} for x in status_options],
                            value="All",
                            clearable=False,
                        ),
                    ],
                    style={"flex": "1", "minWidth": "220px"},
                ),
                html.Div(
                    [
                        html.Label("Potential Participants"),
                        dcc.RadioItems(
                            id="potential-toggle",
                            options=[
                                {"label": "Include", "value": "include"},
                                {"label": "Exclude", "value": "exclude"},
                            ],
                            value="include",
                            inline=True,
                            inputStyle={"marginRight": "6px", "marginLeft": "10px"},
                            style={"paddingTop": "9px"},
                        ),
                    ],
                    style={"flex": "1", "minWidth": "260px"},
                ),
            ],
            style={
                "display": "flex",
                "gap": "14px",
                "flexWrap": "wrap",
                "padding": "8px 20px 10px 20px",
            },
        ),

        html.Div(
            id="filter-note",
            style={"padding": "0 20px 8px 20px", "color": "#666", "fontSize": "13px"},
        ),

        html.Div(
            [
                html.Div(id="denominator-card", className="kpi-card"),
                html.Div(id="numerator-card", className="kpi-card"),
                html.Div(id="progress-card", className="kpi-card"),
            ],
            style={
                "display": "grid",
                "gridTemplateColumns": "repeat(auto-fit, minmax(220px, 1fr))",
                "gap": "14px",
                "padding": "8px 20px 10px 20px",
            },
        ),

        html.Div(
            [
                html.Div(dcc.Graph(id="staff-chart", config={"displayModeBar": False}), className="panel"),
                html.Div(dcc.Graph(id="event-chart", config={"displayModeBar": False}), className="panel"),
            ],
            style={
                "display": "grid",
                "gridTemplateColumns": "repeat(auto-fit, minmax(420px, 1fr))",
                "gap": "14px",
                "padding": "8px 20px 10px 20px",
            },
        ),

        html.Div(
            [
                html.Div(dcc.Graph(id="heatmap-chart", config={"displayModeBar": False}), className="panel"),
            ],
            style={"padding": "8px 20px 10px 20px"},
        ),

        html.Div(
            [
                html.Div(
                    [
                        html.H3("Summary Table", style={"marginTop": "0"}),
                        dash_table.DataTable(
                            id="summary-table",
                            page_size=15,
                            style_table={"overflowX": "auto"},
                            style_cell={
                                "textAlign": "left",
                                "padding": "8px",
                                "fontFamily": "Arial",
                                "fontSize": "13px",
                            },
                            style_header={"fontWeight": "bold"},
                            sort_action="native",
                            filter_action="native",
                        ),
                    ],
                    className="panel",
                )
            ],
            style={"padding": "8px 20px 10px 20px"},
        ),

        html.Div(
            [
                html.Div(
                    [
                        html.H3("Detail Data", style={"marginTop": "0"}),
                        dash_table.DataTable(
                            id="detail-table",
                            page_size=15,
                            style_table={"overflowX": "auto"},
                            style_cell={
                                "textAlign": "left",
                                "padding": "8px",
                                "fontFamily": "Arial",
                                "fontSize": "12px",
                            },
                            style_header={"fontWeight": "bold"},
                            sort_action="native",
                            filter_action="native",
                        ),
                    ],
                    className="panel",
                )
            ],
            style={"padding": "8px 20px 20px 20px"},
        ),
    ],
    style={"fontFamily": "Arial, sans-serif", "maxWidth": "1600px", "margin": "0 auto"},
)

app.index_string = """
<!DOCTYPE html>
<html>
    <head>
        {%metas%}
        <title>Follow-up Dashboard</title>
        {%favicon%}
        {%css%}
        <style>
            body { background: #f7f8fa; margin: 0; }
            .kpi-card {
                background: white;
                border-radius: 12px;
                padding: 18px 20px;
                box-shadow: 0 1px 6px rgba(0,0,0,0.08);
                min-height: 88px;
            }
            .panel {
                background: white;
                border-radius: 12px;
                padding: 16px 16px 8px 16px;
                box-shadow: 0 1px 6px rgba(0,0,0,0.08);
            }
            .kpi-label {
                color: #666;
                font-size: 14px;
                margin-bottom: 6px;
            }
            .kpi-value {
                font-size: 30px;
                font-weight: 700;
            }
        </style>
    </head>
    <body>
        {%app_entry%}
        <footer>
            {%config%}
            {%scripts%}
            {%renderer%}
        </footer>
    </body>
</html>
"""


@app.callback(
    Output("filter-note", "children"),
    Output("denominator-card", "children"),
    Output("numerator-card", "children"),
    Output("progress-card", "children"),
    Output("staff-chart", "figure"),
    Output("event-chart", "figure"),
    Output("heatmap-chart", "figure"),
    Output("summary-table", "data"),
    Output("summary-table", "columns"),
    Output("detail-table", "data"),
    Output("detail-table", "columns"),
    Input("staff-filter", "value"),
    Input("event-filter", "value"),
    Input("status-filter", "value"),
    Input("potential-toggle", "value"),
)
def update_dashboard(staff_value, event_value, status_value, potential_value):
    include_potential = potential_value == "include"
    fdf = apply_filters(df, staff_value, event_value, status_value, include_potential)
    denominator, numerator, progress = calculate_metrics(fdf)

    summary_tbl = prep_summary_table(fdf)
    detail_tbl = prep_detail_table(fdf)

    potential_text = "included" if include_potential else "excluded"
    filter_note = (
        f"Potential Participants are {potential_text}. "
        "The Event = Overall option calculates metrics across all event rows after other filters are applied."
    )

    denominator_card = [
        html.Div("Denominator", className="kpi-label"),
        html.Div(f"{denominator:,}", className="kpi-value"),
    ]
    numerator_card = [
        html.Div("Numerator", className="kpi-label"),
        html.Div(format_number(numerator), className="kpi-value"),
    ]
    progress_card = [
        html.Div("Progress", className="kpi-label"),
        html.Div(pct(progress), className="kpi-value"),
    ]

    return (
        filter_note,
        denominator_card,
        numerator_card,
        progress_card,
        make_bar_staff(fdf),
        make_bar_event(fdf),
        make_heatmap(fdf),
        summary_tbl.to_dict("records"),
        [{"name": c, "id": c} for c in summary_tbl.columns],
        detail_tbl.to_dict("records"),
        [{"name": c, "id": c} for c in detail_tbl.columns],
    )


if __name__ == "__main__":
    app.run(debug=True)
