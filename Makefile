build: uberjar

uberjar:
		clojure -A:uberjar -m uberdeps.uberjar --target target/somerville.jar

clean:
		rm -rf target/

fig:
		clojure -A:fig -m figwheel.main --build somerville --repl

test:
		clojure -A:test -m kaocha.runner

test-watch:
		clojure -A:test -m kaocha.runner --watch

repl:
		clojure -A:repl -m repl

outdated:
		clojure -A:outdated
