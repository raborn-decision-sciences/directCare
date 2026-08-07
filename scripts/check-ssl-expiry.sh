#!/bin/bash
# Daily SSL certificate expiry check for the production directCare domains.
#
# Why this exists: Caddy renews Let's Encrypt certs automatically, but not
# infallibly -- a DNS change, an ACME rate limit, or a Let's Encrypt outage
# could all cause a silent renewal failure with nothing to notice it (see
# DEPLOY.md's "Uptime and health monitoring" section). This is a cheap,
# dependency-free early-warning check: no external monitoring service, just
# openssl (already on the box) and the ZeptoMail credentials this stack
# already has configured for transactional email.
#
# Caddy's own renewal kicks in with plenty of runway before expiry (roughly
# a third of a Let's Encrypt cert's ~90-day lifetime, so around the 30-day
# mark) -- ALERT_DAYS defaults well inside that window specifically so this
# should never fire in normal operation. If it does, renewal is stuck and
# needs a human, not a "just wait" response.
#
# Usage (from crontab, once a day is plenty given the alert window):
#   0 4 * * * /path/to/directCare/scripts/check-ssl-expiry.sh >> /var/log/ssl-check.log 2>&1
#
# DOMAINS, ALERT_DAYS, ALERT_EMAIL, FROM_EMAIL, ZEPTOMAIL_TOKEN_FILE are all
# overridable via environment for local testing without touching the
# production defaults.
#
# Requires GNU date (`date -d "..." +%s`) to parse openssl's notAfter
# output -- the production server's Ubuntu ships this natively as `date`.
# On macOS, BSD date doesn't support -d the same way; DATE_BIN lets local
# testing point at `gdate` (Homebrew coreutils) instead -- not needed for
# the real cron job, which always uses plain `date`.
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DATE_BIN="${DATE_BIN:-date}"

# Every TLS-terminated public domain in the Caddyfile.
DOMAINS="${DOMAINS:-app.directcareanalytics.com planner.directcareanalytics.com billing.directcareanalytics.com}"
ALERT_DAYS="${ALERT_DAYS:-14}"
ALERT_EMAIL="${ALERT_EMAIL:-anthony@raborndecisionsciences.com}"
FROM_EMAIL="${FROM_EMAIL:-noreply@directcareanalytics.com}"
ZEPTOMAIL_TOKEN_FILE="${ZEPTOMAIL_TOKEN_FILE:-${REPO_DIR}/secrets/zeptomail_token.txt}"

problems=""

for domain in $DOMAINS; do
  end_date=$(echo | openssl s_client -connect "${domain}:443" -servername "$domain" 2>/dev/null \
    | openssl x509 -noout -enddate 2>/dev/null | sed 's/^notAfter=//')

  if [ -z "$end_date" ]; then
    echo "$("$DATE_BIN" -u +%Y-%m-%dT%H:%M:%SZ) ${domain}: could not retrieve certificate at all"
    problems="${problems}<li><strong>${domain}</strong>: could not retrieve a certificate at all -- site may be down or DNS/TLS is broken, not just an expiring cert.</li>"
    continue
  fi

  end_epoch=$("$DATE_BIN" -d "$end_date" +%s)
  now_epoch=$("$DATE_BIN" +%s)
  days_left=$(( (end_epoch - now_epoch) / 86400 ))

  echo "$("$DATE_BIN" -u +%Y-%m-%dT%H:%M:%SZ) ${domain}: ${days_left} days remaining (expires ${end_date})"

  if [ "$days_left" -le "$ALERT_DAYS" ]; then
    problems="${problems}<li><strong>${domain}</strong>: only ${days_left} days left (expires ${end_date}). Caddy's automatic renewal should have already kicked in well before this point -- check 'docker compose logs caddy' for a renewal error.</li>"
  fi
done

if [ -z "$problems" ]; then
  exit 0
fi

echo "ALERT: sending expiry warning to ${ALERT_EMAIL}"

if [ ! -f "$ZEPTOMAIL_TOKEN_FILE" ]; then
  echo "ERROR: ZeptoMail token file not found at ${ZEPTOMAIL_TOKEN_FILE} -- cannot send alert email." >&2
  exit 1
fi
token=$(tr -d '[:space:]' < "$ZEPTOMAIL_TOKEN_FILE")

# Same API call directCareAuth's send_password_reset_email() makes
# (directCareAuth/R/email.R) -- POST to ZeptoMail's transactional send
# endpoint with a Zoho-enczapikey bearer token.
curl -sS -X POST "https://api.zeptomail.com/v1.1/email" \
  -H "Authorization: Zoho-enczapikey ${token}" \
  -H "Content-Type: application/json" \
  -d @- <<JSON
{
  "from": {"address": "${FROM_EMAIL}"},
  "to": [{"email_address": {"address": "${ALERT_EMAIL}"}}],
  "subject": "directCare: SSL certificate expiry warning",
  "htmlbody": "<p>One or more directCare domains need attention:</p><ul>${problems}</ul>"
}
JSON

echo
