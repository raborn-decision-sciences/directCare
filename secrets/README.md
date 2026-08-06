# Secrets

`docker-compose.yml` reads the Postgres password from `secrets/db_password.txt`
via Docker Compose secrets. This directory is git-ignored except for this
file and `.gitkeep` — generate the password file on the host before running
`make deploy`, it is never committed.

**The `chown 1000:1000` commands below are for the production VPS only**
(DEPLOY.md Step 4, where you're `root` over SSH). `dca`/`planner`/`billing`
run as an unprivileged `appuser` inside their containers (UID `1000`,
pinned in `docker/base.Dockerfile`) rather than root, and Compose's
file-based secrets mount preserves the *host* file's own owner/permissions
as-is (no per-secret mode/uid override outside Swarm mode) — so on that
Linux server, a root-owned file isn't readable by the non-root UID no
matter what `chmod` mode you set, and `chown` fixes it. **On a local Mac
(Docker Desktop), skip the `chown` line entirely** — you're not root and
`chown` to an arbitrary UID will fail with "Operation not permitted";
Docker Desktop's VM doesn't enforce the same host-UID matching, so
`chmod 600`/`400` alone (or even the file's default permissions) is
enough for local dev/testing. `chown` each file to UID `1000` on the
server (no matching named user needs to exist there):

```bash
openssl rand -base64 32 | tr -d '\n' > secrets/db_password.txt
chown 1000:1000 secrets/db_password.txt
chmod 400 secrets/db_password.txt
```

`secrets/zeptomail_token.txt` holds the ZeptoMail API "Send Mail" token
(from the ZeptoMail dashboard -> Mail Agents -> your agent -> API tokens),
used by both apps' password-reset email flow (`directCareAuth::send_password_reset_email()`).
Same convention -- generate it on the host, never commit it:

```bash
echo -n "your-zeptomail-api-token" > secrets/zeptomail_token.txt
chown 1000:1000 secrets/zeptomail_token.txt
chmod 400 secrets/zeptomail_token.txt
```

`secrets/stripe_secret_key.txt` holds a Stripe **restricted key** (`rk_...`,
Dashboard -> Developers -> API keys -> Create restricted key -- scope:
Checkout Sessions Write, Billing Portal Sessions Write, Customers Read, see
STRIPE_BILLING.md Part 1 #5). `dca`, `planner`, and `billing` all read it
(`STRIPE_SECRET_KEY_FILE`) -- `dca`/`planner` to create Checkout/Portal
Sessions, `billing` to resolve a completed Checkout Session's line items
(`stripe_handle_webhook_event()`). Use the `sk_test_.../rk_test_...` key
until Live mode is actually being launched, same as every other Stripe
credential in this repo:

```bash
echo -n "sk_test_... or rk_test_..." > secrets/stripe_secret_key.txt
chown 1000:1000 secrets/stripe_secret_key.txt
chmod 400 secrets/stripe_secret_key.txt
```

`secrets/stripe_webhook_secret.txt` holds the signing secret (`whsec_...`)
for the `billing.directcareanalytics.com/webhook` endpoint, generated when
that endpoint is registered in the Stripe Dashboard (Developers -> Webhooks
-> Add endpoint) -- only `billing` reads it
(`stripe_verify_webhook_signature()`). Same convention:

```bash
echo -n "whsec_..." > secrets/stripe_webhook_secret.txt
chown 1000:1000 secrets/stripe_webhook_secret.txt
chmod 400 secrets/stripe_webhook_secret.txt
```
