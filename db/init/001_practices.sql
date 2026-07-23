CREATE TABLE practices (
    id                  SERIAL PRIMARY KEY,
    practice_name       TEXT NOT NULL,
    email               TEXT UNIQUE NOT NULL,
    password_hash       TEXT NOT NULL,
    subscription_status TEXT NOT NULL DEFAULT 'trial', -- trial / active / cancelled / none
    stripe_customer_id  TEXT,
    created_at          TIMESTAMPTZ NOT NULL DEFAULT now()
);
