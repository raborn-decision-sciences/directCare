build-base:
	docker build -f docker/base.Dockerfile -t directcare-base:latest .

deploy: build-base
	docker compose build
	docker compose up -d
