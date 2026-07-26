# Secrets

`docker-compose.yml` reads the Postgres password from `secrets/db_password.txt`
via Docker Compose secrets. This directory is git-ignored except for this
file and `.gitkeep` — generate the password file on the host before running
`make deploy`, it is never committed:

```bash
openssl rand -base64 32 | tr -d '\n' > secrets/db_password.txt
chmod 600 secrets/db_password.txt
```

`secrets/zeptomail_token.txt` holds the ZeptoMail API "Send Mail" token
(from the ZeptoMail dashboard -> Mail Agents -> your agent -> API tokens),
used by both apps' password-reset email flow (`directCareAuth::send_password_reset_email()`).
Same convention -- generate it on the host, never commit it:

```bash
echo -n "your-zeptomail-api-token" > secrets/zeptomail_token.txt
chmod 600 secrets/zeptomail_token.txt
```
