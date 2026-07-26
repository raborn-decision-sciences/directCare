build-base:
	docker build -f docker/base.Dockerfile -t directcare-base:latest .

deploy: build-base
	docker compose build
	docker compose up -d

# Local/CI testing only -- publishes dca/planner ports directly to the host
# (see docker-compose.ci.yml) since Caddy needs real DNS/TLS it won't have
# here. Never use this compose combination in production.
dev: build-base
	docker compose -f docker-compose.yml -f docker-compose.ci.yml build
	docker compose -f docker-compose.yml -f docker-compose.ci.yml up -d db dca planner
