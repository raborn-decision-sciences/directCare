-- Adds the client IP to auth_events so login lockout can be gated by IP
-- as well as by email (see IP_LOGIN_LOCKOUT.md) -- closes both a targeted
-- DoS (failing one known email 5x from anywhere) and a credential-spray
-- attack (many emails from one source).
--
-- Nullable: existing rows predate this column, and any edge case where
-- the IP can't be determined should degrade to NULL, not error.

ALTER TABLE auth_events ADD COLUMN ip_address TEXT;

CREATE INDEX idx_auth_events_ip_created_at ON auth_events (ip_address, created_at);
