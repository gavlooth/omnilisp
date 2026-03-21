# Examples

## Canonical Product Example

- `examples/finwatch/main.omni`
- Domain: financial-service webserver

Future feature demonstrations should use `finwatch` by default.

### Finwatch Frontend Dashboard

- `examples/finwatch/value_dashboard.html`
- `examples/finwatch/value_dashboard_sample.csv`
- Purpose: practical value screener UI for filtering stocks by valuation, quality,
  leverage, and margin of safety criteria.

### Finwatch Tutorial Module

- `examples/finwatch/alerts.omni`
- `examples/finwatch/alerts_tutorial_smoke.omni`
- Purpose: minimal effect-handler + dispatch tutorial for price and analytics
  alerts, with a collector/log handler pair.

## Legacy Compatibility Example

- `examples/deduce_crud_server.omni`

Keep this for regression coverage and compatibility checks, not as the primary
product example.
