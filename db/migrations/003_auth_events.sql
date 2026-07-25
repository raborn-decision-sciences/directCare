-- A lightweight audit log of security-relevant auth events (login
-- success/failure/lockout, password changes, profile edits). Also backs
-- login-attempt lockout: directCareAuth::auth_is_locked_out() counts
-- recent login_failure rows for an email rather than needing a separate
-- lockout table.
--
-- practice_id is nullable: a failed login against an unrecognized email
-- has no practice to attach to. ON DELETE SET NULL rather than CASCADE --
-- the audit trail should survive a practice row being removed.

CREATE TABLE auth_events (
    id           SERIAL PRIMARY KEY,
    practice_id  INTEGER REFERENCES practices(id) ON DELETE SET NULL,
    email        TEXT,
    event_type   TEXT NOT NULL,
    detail       TEXT,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Supports both the lockout count-query (email + event_type + recency)
-- and any future "show recent activity for this account" lookup.
CREATE INDEX idx_auth_events_email_created_at ON auth_events (email, created_at);
