ifdef STRICT
TEX=xelatex -halt-on-error
else
TEX=xelatex -interaction=nonstopmode
endif

Yesod.pdf: tex/D*.tex tex/*.tex tmp-dir
	cd tmp && $(TEX) "Developing Web Applications with Haskell and Yesod.tex" && $(TEX) "Developing Web Applications with Haskell and Yesod.tex" && $(TEX) "Developing Web Applications with Haskell and Yesod.tex" && cp "Developing Web Applications with Haskell and Yesod.pdf" ../

tmp-dir:
	rm -rf tmp
	cp -R tex tmp

clean:
	rm -rf tmp
