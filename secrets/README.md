# Secrets

`docker-compose.yml` reads the Postgres password from `secrets/db_password.txt`
via Docker Compose secrets. This directory is git-ignored except for this
file and `.gitkeep` — generate the password file on the host before running
`make deploy`, it is never committed:

```bash
openssl rand -base64 32 | tr -d '\n' > secrets/db_password.txt
chmod 600 secrets/db_password.txt
```
