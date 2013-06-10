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
TEXSRCS := $(wildcard tex/[S012]*.tex)
TEXSRCS := $(subst $(space),\$(space),$(TEXSRCS))
TEXSRCS := $(subst .tex\,.tex,$(TEXSRCS))

# Здесь имена без пробелов - сойдёт и так
HSSRCS = $(addprefix hs/, \
	02/type-families.hs \
	03/hello-world.hs \
	03/routes.hs \
	04/i18n.hs \
	04/quasiquoter.hs \
	04/query-string.hs \
	04/shakespeare-text.hs \
	04/url-interpolation.hs \
	04/var-interpolation.hs \
	06/slashes.hs \
	08/input-forms.hs \
	08/monadic-forms.hs \
	08/synopsis.hs \
	09/messages.hs \
	09/session-example.hs \
	09/ultimate-destination.hs \
	12/RepHtmlJson.hs \
	13/navbar.hs \
	13/request-information.hs \
	14/authentication.hs \
	14/email-authentication.hs \
	14/authorization.hs \
	17/hellosub.hs \
	20/client.hs \
	20/server.hs \
	21/source.hs \
	24/hello-world.hs \
	24/hello-world-gzip.hs \
	26/synopsis.hs \
	26/response-body.hs \
	26/response-head.hs \
	27/synopsis.hs \
	27/cursor.hs \
	27/cursor-operator.hs \
	27/cursor-h1.hs \
	27/xml.hs \
	27/xml-hamlet.hs \
	27/xml-hamlet-vars.hs \
	27/xml2html.hs \
)
IMAGES := $(wildcard img/*.png)
BLOGLHS := 18/blog.lhs
WIKISRC = $(addprefix hs/19/,Chat.hs Wiki.hs)

#-
# Итоговый файл
#-
YESODBOOK := $(MASTERTEX:.tex=.pdf)

#-
# Строка для запуска TeX
#-
TEX := xelatex
ifdef STRICT
	TEXOPTS := -halt-on-error
else
	TEXOPTS := -interaction=nonstopmode
endif

BUILDDIR := tmp
BINDIR := $(BUILDDIR)/bin
OBJDIR := $(BUILDDIR)/obj
TEXDIR := $(BUILDDIR)/tex
TEXOPTS += -output-directory=$(abspath $(TEXDIR))

#-
# Цели для сборки
#-
.PHONY: all clean examples

all: $(YESODBOOK)

$(YESODBOOK): tex/$(MASTERTEX) $(TEXSRCS) $(HSSRCS) $(IMAGES) hs/$(BLOGLHS) $(WIKISRC)
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
	@mkdir -p $(OBJDIR)

# Успешная сборка примеров требует наличия следующих пакетов:
HACKAGES = \
	'yesod-platform<1.2' \
	persistent-sqlite \
	sphinx \
	wai-eventsource \
	markdown \
	xml2html \
	xml-hamlet

.PHONY: install-packages
install-packages:
	cabal install $(HACKAGES)

examples: simple-examples blog wiki i18n

simple-examples: $(HSSRCS) | dirs
	@$(foreach f, $(subst hs/,,$(HSSRCS)), \
		echo $f && \
		mkdir -p $(OBJDIR)/$(dir $f) && \
		mkdir -p $(BINDIR)/$(dir $f) && \
		cp -p hs/$f $(OBJDIR)/$(dir $f) && \
		ghc --make -o $(BINDIR)/$(f:.hs=) $(OBJDIR)/$f > $(OBJDIR)/$(f:.hs=.log);)

blog: hs/$(BLOGLHS) | dirs
	@echo $(BLOGLHS)
	@mkdir -p $(OBJDIR)/$(dir $(BLOGLHS))
	@mkdir -p $(BINDIR)/$(dir $(BLOGLHS))
	@cp -p hs/$(BLOGLHS) $(OBJDIR)/$(dir $(BLOGLHS))
	@cp -rp hs/18/messages-blog $(BUILDDIR)
	@cd $(OBJDIR) && ghc --make -o $(realpath $(BINDIR))/$(BLOGLHS:.lhs=) $(BLOGLHS) > $(BLOGLHS:.lhs=.log)

wiki: $(WIKISRC) | dirs
	@echo 19/wiki
	@mkdir -p $(OBJDIR)/19
	@mkdir -p $(BINDIR)/19
	@cp -p $(WIKISRC) $(OBJDIR)/19
	@cd $(OBJDIR) && ghc --make -o $(realpath $(BINDIR))/19/wiki $(subst hs/,,$(WIKISRC)) > 19/wiki.log

I18NSRC = hs/16/i18n-synopsis.hs
i18n: $(I18NSRC)
	@echo 16/i18n
	@mkdir -p $(OBJDIR)/16
	@mkdir -p $(BINDIR)/16
	@cp -p $(I18NSRC) $(OBJDIR)/16
	@cp -rp hs/16/messages $(OBJDIR)/16
	@cd $(OBJDIR)/16 && ghc --make -o $(realpath $(BINDIR))/16/i18n-synopsis i18n-synopsis.hs > i18n-synopsis.log 
