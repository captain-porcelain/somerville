build: fig

package:
		clojure -A:cambada -m cambada.jar --app-version $(VERSION)

clean:
		rm -rf target/

fig:
		clojure -A:fig -m figwheel.main --build somerville --repl

tests:
		clojure -A:test -m kaocha.runner

test-watch:
		clojure -A:test -m kaocha.runner --watch

repl:
		clojure -A:repl -m repl 7878

outdated:
		clojure -A:outdated

install:
		clojure -A:publish install target/somerville-$(VERSION).jar

publish:
		echo ${CLOJARS_USERNAME}
		clojure -A:publish deploy target/somerville-$(VERSION).jar

sidenotes:
		clojure -A:sidenotes
