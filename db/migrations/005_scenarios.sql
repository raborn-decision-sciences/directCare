-- Saved scenario slots: 3 fixed, user-labeled slots per practice, per
-- feature area (not "3 most recent" LRU) -- see SAVED_SCENARIOS.md for the
-- full design. Separate tables/caps per feature area, all keyed by
-- practice_id (the practices.id FK, same convention as auth_events/
-- password_resets), not email.
--
-- plan_scenarios and dca_calculator_scenarios are pure scalar/small-list
-- inputs -- JSONB round-trips those cleanly. dca_forecast_scenarios is
-- different: it has to carry row-level transaction data (an 11-column
-- tibble with Date columns), not just settings, so it uses a BYTEA column
-- holding an R-serialized (saveRDS, xz-compressed) bundle instead -- see
-- SAVED_SCENARIOS.md's "Why BYTEA (not JSONB)" section. Both dca_* tables
-- snapshot computed results alongside inputs, not just inputs to
-- recompute-on-load, so a saved comparison never silently drifts if the
-- forecasting logic changes later; plan_scenarios saves inputs only, since
-- Launch Planner's own computation is cheap and deterministic to re-run.
--
-- No separate idx_..._practice_id index needed on any of the three: the
-- UNIQUE (practice_id, label) constraint's backing index already has
-- practice_id as its leading column, covering "all scenarios for this
-- practice" lookups (the picker UI's main query) for free.
--
-- ON DELETE CASCADE (unlike auth_events' ON DELETE SET NULL): a saved
-- scenario is meaningless without its practice, so there's no reason to
-- keep orphaned rows around.

CREATE TABLE plan_scenarios (
    id          SERIAL PRIMARY KEY,
    practice_id INTEGER NOT NULL REFERENCES practices(id) ON DELETE CASCADE,
    label       TEXT NOT NULL,
    inputs      JSONB NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    UNIQUE (practice_id, label)
);

CREATE TABLE dca_calculator_scenarios (
    id          SERIAL PRIMARY KEY,
    practice_id INTEGER NOT NULL REFERENCES practices(id) ON DELETE CASCADE,
    label       TEXT NOT NULL,
    inputs      JSONB NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    UNIQUE (practice_id, label)
);

CREATE TABLE dca_forecast_scenarios (
    id              SERIAL PRIMARY KEY,
    practice_id     INTEGER NOT NULL REFERENCES practices(id) ON DELETE CASCADE,
    label           TEXT NOT NULL,
    source_filename TEXT,           -- NULL for manual-entry-only sessions
    payload         BYTEA NOT NULL, -- saveRDS(..., compress = "xz") of a bundled list
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    UNIQUE (practice_id, label)
);

-- Generic (not practices-specific like migration 002's
-- set_practices_updated_at()) since three unrelated tables share it here.
CREATE OR REPLACE FUNCTION set_scenario_updated_at() RETURNS trigger AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_plan_scenarios_updated_at
    BEFORE UPDATE ON plan_scenarios
    FOR EACH ROW
    EXECUTE FUNCTION set_scenario_updated_at();

CREATE TRIGGER trg_dca_calculator_scenarios_updated_at
    BEFORE UPDATE ON dca_calculator_scenarios
    FOR EACH ROW
    EXECUTE FUNCTION set_scenario_updated_at();

CREATE TRIGGER trg_dca_forecast_scenarios_updated_at
    BEFORE UPDATE ON dca_forecast_scenarios
    FOR EACH ROW
    EXECUTE FUNCTION set_scenario_updated_at();
