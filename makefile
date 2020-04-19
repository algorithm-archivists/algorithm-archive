default: main

# This file will expand to cover more cases, like initial install of pipenv and initial pip packages.

main: build/main.py
	cd build; pipenv run python3 main.py ; cd ../website ; python3 -m http.server 8080 ; cd ..
