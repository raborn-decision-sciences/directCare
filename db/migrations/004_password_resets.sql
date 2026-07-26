-- Backs the email-based "forgot password" flow. Only a hash of the reset
-- token is stored (see directCareAuth::password_reset_request()) -- the
-- raw token exists only in the emailed link and the moment it's
-- generated, same principle as never storing a plaintext password.
--
-- ON DELETE CASCADE (unlike auth_events' ON DELETE SET NULL): a reset
-- token is meaningless without its practice, so there's no reason to
-- keep orphaned rows around.

CREATE TABLE password_resets (
    id           SERIAL PRIMARY KEY,
    practice_id  INTEGER NOT NULL REFERENCES practices(id) ON DELETE CASCADE,
    token_hash   TEXT NOT NULL,
    expires_at   TIMESTAMPTZ NOT NULL,
    used_at      TIMESTAMPTZ,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_password_resets_token_hash ON password_resets (token_hash);
