-- Adds a practice mailing/physical address field and an updated_at
-- timestamp (auto-maintained via trigger) to the practices table.
--
-- Named `address`, not `location`: directCarePlanner has an unrelated
-- ephemeral "location" concept (a ZIP/county market-lookup input, never
-- persisted to any table -- see directCarePlanner/R/mod_plan_inputs.R).
-- Column names should say what they are and shouldn't coincide with an
-- unrelated concept elsewhere in the system unless the two are meant to
-- be joined as the same key.

ALTER TABLE practices
    ADD COLUMN address     TEXT,
    ADD COLUMN updated_at  TIMESTAMPTZ NOT NULL DEFAULT now();

CREATE OR REPLACE FUNCTION set_practices_updated_at() RETURNS trigger AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_practices_updated_at
    BEFORE UPDATE ON practices
    FOR EACH ROW
    EXECUTE FUNCTION set_practices_updated_at();
