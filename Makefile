all: targets docker

targets: _targets.R src/helpers.R
	@echo Running targets...
	@Rscript -e "targets::tar_make()"
	@echo Cleaning repository...
	@Rscript -e "source('src/helpers.R'); clean_repo()"
	@echo Rendering website...
	@quarto publish gh-pages

delete-nul:
	Del \\?\C:\Users\gonza\Documents\cognate-priming\deletefile.txt
	
docker-build: Dockerfile .dockerignore
	@echo "Building Docker image..."
	@docker build -t cognate-beginnings .
	@echo "Pushing image to Dockerhub"
	@docker tag cognate-beginnings gongcastro/cognate-beginnings:latest
	@docker push cognate-beginnings:latest

docker-actions: Dockerfile .dockerignore
	@echo "Pushing Dockerfile to GitHub Actions..."
	@git add Dockerfile
	@git diff --quiet && git diff --staged --quiet || git commit -am "Update Dockerfile"
	@git push
	@timeout 5
	@gh run watch -i 60 --repo gongcastro/cognate-beginnings

docker-run:
	@echo "Running Docker container at http://localhost:8787"
	@docker run --rm -ti \
		-e ROOT=true \
		-e PASSWORD=rstudio \
 		-p 8787:8787 \
		--name rstudio cognate-beginnings:latest
	