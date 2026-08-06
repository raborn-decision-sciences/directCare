-- Stripe subscription billing (see STRIPE_BILLING.md). One Stripe
-- subscription per practice -- DCA and Planner already share one login
-- (the practices table), so a practice is one account, not two, and
-- tier-gated features in either app read from these same columns.
--
-- Design decisions locked in 2026-08-03 (see STRIPE_BILLING.md's "Not yet
-- decided" section, now resolved): single shared plan_tier (not the
-- per-app-tiers-with-bundling alternative also documented there), no free
-- trial (plan_tier defaults to 'free' indefinitely), monthly billing only
-- for now. Exact tier names/prices are still not finalized -- 'free' is
-- the only plan_tier value this migration itself depends on; real tier
-- values (e.g. 'starter'/'pro') are assigned by the webhook handler at
-- runtime, resolved from Stripe Price lookup_keys, never hardcoded here.

-- The internal source of truth the apps actually gate features on.
-- Free-text rather than an enum: Stripe's own Price lookup_keys are
-- already free-text and stable, and an enum would need a migration every
-- time a tier is added/renamed -- not worth the extra rigor for a
-- handful of internally-controlled values.
ALTER TABLE practices ADD COLUMN plan_tier TEXT NOT NULL DEFAULT 'free';

-- The Subscription id (distinct from the Customer id already on this
-- table) -- needed any time the app itself needs to reference *this
-- specific* subscription (e.g. an admin support action), not just "this
-- customer's most recent one."
ALTER TABLE practices ADD COLUMN stripe_subscription_id TEXT;

-- Lets the app show "renews on <date>" / "access ends <date>" without a
-- live Stripe API call on every page load -- refreshed by the webhook on
-- every relevant subscription event.
ALTER TABLE practices ADD COLUMN current_period_end TIMESTAMPTZ;

-- Webhook idempotency: Stripe retries delivery for up to 72 hours on
-- anything other than a 2xx response, and can also just send the same
-- event twice in normal operation (at-least-once delivery, not
-- exactly-once, by Stripe's own design). The billing webhook route
-- inserts the event id here BEFORE doing any state-changing work; a
-- unique-violation on the insert means "already processed," and the
-- route returns 200 immediately without re-applying the event.
CREATE TABLE stripe_webhook_events (
    event_id    TEXT PRIMARY KEY,
    event_type  TEXT NOT NULL,
    received_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- subscription_status's original convention (trial / active / cancelled /
-- none, set when this column was first added) is superseded as of this
-- migration: the webhook handler is now the only writer of this column
-- going forward, and stores Stripe's own Subscription.status values
-- verbatim (trialing/active/past_due/canceled/unpaid/incomplete/
-- incomplete_expired/paused) rather than translating through a second,
-- lossier vocabulary -- strictly less code, one less place for a mapping
-- bug to hide. 'none' replaces 'trial' as the pre-Stripe-customer
-- default: a practice created via signup has no stripe_customer_id yet
-- and should read as un-subscribed, not as any real Stripe status.
-- 'trial' was never a real Stripe status to begin with (billing didn't
-- exist before this migration), so every existing row carrying it is
-- unconditionally reinterpreted as 'none', not selectively migrated.
UPDATE practices SET subscription_status = 'none' WHERE subscription_status = 'trial';
ALTER TABLE practices ALTER COLUMN subscription_status SET DEFAULT 'none';
COMMENT ON COLUMN practices.subscription_status IS
    'Stripe Subscription.status verbatim (trialing/active/past_due/canceled/unpaid/incomplete/incomplete_expired/paused), or ''none'' before any Stripe customer exists. Written only by the billing webhook handler (directCareBilling::stripe_handle_webhook_event()).';
