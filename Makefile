# Вспомогательные переменные для обработки пробелов в именах
e :=
space := $(e) $(e)

#-
# Исходные файлы
#-
MASTERTEX := Developing\ Web\ Applications\ with\ Haskell\ and\ Yesod.tex

# Имена исходных файлов TeX содержат пробелы. Поэтому для корректной работы make
# сначала экранируем все пробелы без разбора, а затем снимаем экранирование
# с полезных пробелов между файлами
TEXSRCS := $(shell find tex -type f -name "*tex")
TEXSRCS := $(subst $(space),\$(space),$(TEXSRCS))
TEXSRCS := $(subst .tex\,.tex,$(TEXSRCS))
LHSSRCS := $(shell find hs -name "*.lhs")

# Здесь имена без пробелов - сойдёт и так
HSSRCS = $(shell find hs -name "*.hs")
IMAGES := $(wildcard img/*.png)

#-
# Итоговый файл
#-
YESODBOOK := $(MASTERTEX:.tex=.pdf)

#-
# Строка для запуска TeX
#-
TEX := xelatex
STRICT ?= 1
ifdef STRICT
	TEXOPTS := -halt-on-error
else
	TEXOPTS := -interaction=nonstopmode
endif

BUILDDIR := tmp
BINDIR := $(BUILDDIR)/bin
TEXDIR := $(BUILDDIR)/tex
TEXOPTS += -output-directory=$(abspath $(TEXDIR))

#-
# Цели для сборки
#-
.PHONY: all clean examples

all: $(YESODBOOK)

$(YESODBOOK): tex/$(MASTERTEX) $(TEXSRCS) $(LHSSRCS) $(HSSRCS) $(IMAGES)
	rm -f $(YESODBOOK)
	mkdir -p $(TEXDIR)
	cd tex && \
	$(TEX) $(TEXOPTS) $(MASTERTEX) && \
	$(TEX) $(TEXOPTS) $(MASTERTEX) && \
	$(TEX) $(TEXOPTS) $(MASTERTEX) && \
	cd .. && \
	cp $(TEXDIR)/$(YESODBOOK) .

clean:
	-rm -rf $(BUILDDIR)
	-rm -f $(YESODBOOK)

dirs:
	@mkdir -p $(BINDIR)

# Успешная сборка примеров требует наличия следующих пакетов:
YESOD_VERSION = $(shell grep '^version:' examples.cabal | egrep -o '[0-9\.]+$$')
HACKAGES = \
	"yesod==$(YESOD_VERSION)" \
	persistent-sqlite \
	persistent-postgresql \
	sphinx \
	wai-conduit \
	wai-extra \
	markdown \
	monadcryptorandom \
	pool-conduit \
	xml2html \
	xml-hamlet

.PHONY: install-packages sandbox examples

sandbox:
	cabal sandbox init

install-packages:
	cabal install -j $(HACKAGES)

examples:
	@mkdir -p $(BINDIR)
	@cabal configure --builddir=$(BUILDDIR) && \
		cabal build -j --builddir=$(BUILDDIR) && \
		find $(BUILDDIR)/build -type f -executable -exec cp {} $(BINDIR) \;

%:
	@echo "Building '$@'"
	@mkdir -p $(BINDIR)
	@cabal configure --builddir=$(BUILDDIR) && \
		cabal build -j --builddir=$(BUILDDIR) $@ && \
		cp $(BUILDDIR)/build/$@/$@ $(BINDIR)/$@
