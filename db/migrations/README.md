# Database migrations

`db/init/001_practices.sql` only runs automatically, and only once: Postgres'
official image executes everything in `docker-entrypoint-initdb.d/` the
first time it starts against an **empty** data directory (see
`docker-compose.yml`'s `db` service). Once the `pgdata` volume has been
initialized, `001_practices.sql` never runs again -- it must not be edited
to reflect schema changes for a live deployment.

Files in this directory are later, additive schema changes. There is no
migration-runner tooling in this repo; apply them by hand, in order,
against the live database:

```bash
docker compose exec -T db psql -U directcare -d directcare < db/migrations/002_practice_profile_fields.sql
```

Numbering is sequential and never reused. Each file should be safe to run
against the current production schema at the time it's added (additive
`ALTER TABLE`/`CREATE TABLE`, not destructive), and idempotent where cheap
to make so (e.g. `CREATE OR REPLACE FUNCTION`).
