# Secrets

`docker-compose.yml` reads the Postgres password from `secrets/db_password.txt`
via Docker Compose secrets. This directory is git-ignored except for this
file and `.gitkeep` — generate the password file on the host before running
`make deploy`, it is never committed.

`dca`/`planner` run as an unprivileged `appuser` inside their containers
(UID `1000`, pinned in `docker/base.Dockerfile`) rather than root. Compose's
file-based secrets mount preserves the *host* file's own owner/permissions
as-is — it does not support a per-secret mode/uid override outside Swarm
mode — so a root-owned file (the default if you create these as root) isn't
readable by that non-root UID regardless of `chmod` mode. `chown` each file
to UID `1000` (no matching named user needs to exist on the host):

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
