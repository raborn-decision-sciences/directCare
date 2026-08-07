#!/bin/bash
# Daily Postgres backup for the production directCare stack.
#
# Why this exists: the DigitalOcean Droplet-level snapshot (DEPLOY.md
# Step 1) covers the whole VM weekly -- real disaster recovery if the box
# itself is lost, but coarse (restores app code/OS too, not just the
# database) and "weekly" means up to 6 days of data loss in the worst
# case. This is a much finer-grained, database-only backup meant to run
# daily via cron, kept separately from the Droplet it's protecting
# against losing (a local-only backup doesn't survive losing the box --
# see the sync step in DEPLOY.md's "Database backups beyond the Droplet
# snapshot" section for where these files need to end up).
#
# Usage (from crontab, as root, since docker compose needs the daemon):
#   0 3 * * * /path/to/directCare/scripts/backup-db.sh >> /var/log/db-backup.log 2>&1
#
# BACKUP_DIR and RETENTION_DAYS are overridable via environment for local
# testing without touching the production defaults.
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BACKUP_DIR="${BACKUP_DIR:-/root/db-backups}"
RETENTION_DAYS="${RETENTION_DAYS:-14}"

mkdir -p "$BACKUP_DIR"

cd "$REPO_DIR"

STAMP=$(date +%Y%m%d)
OUT_FILE="${BACKUP_DIR}/directcare-${STAMP}.sql.gz"

docker compose exec -T db pg_dump -U directcare -d directcare | gzip > "$OUT_FILE"

# Sanity check: a truncated/empty dump is worse than no dump at all --
# fail loudly (non-zero exit, visible in the cron log) rather than
# silently leaving a corrupt file that looks like a successful backup.
if [ ! -s "$OUT_FILE" ]; then
  echo "ERROR: ${OUT_FILE} is empty -- pg_dump likely failed." >&2
  rm -f "$OUT_FILE"
  exit 1
fi

echo "Backed up to ${OUT_FILE} ($(du -h "$OUT_FILE" | cut -f1))"

# Prune anything older than RETENTION_DAYS -- these are local copies only;
# the off-box sync (see DEPLOY.md) is what actually protects against
# losing the server, not this retention window.
find "$BACKUP_DIR" -name '*.sql.gz' -mtime "+${RETENTION_DAYS}" -delete
