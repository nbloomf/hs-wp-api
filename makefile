docs:
	pandoc --mathjax -s -c style.css --to html -o docs/index.html index.md
	pandoc --mathjax -s -c style.css --to html -o docs/overview.html src/Web/Api/WordPress.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/monad.html src/Web/Api/WordPress/Monad.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/types.html src/Web/Api/WordPress/Types.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/endpoints.html src/Web/Api/WordPress/Endpoints.lhs


.PHONY: docs
