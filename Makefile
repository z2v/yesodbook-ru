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
HSSRCS := $(wildcard hs/*.hs)
IMAGES := $(wildcard img/*.png)
BLOGLHS := 18-blog.lhs

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

$(YESODBOOK): tex/$(MASTERTEX) $(TEXSRCS) $(HSSRCS) $(IMAGES) hs/$(BLOGLHS)
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

examples: blog | dirs
	@cp -pu $(HSSRCS) $(OBJDIR)
	$(foreach f, $(subst hs/,$(OBJDIR)/,$(HSSRCS)), ghc $(f) -o $(subst $(OBJDIR),$(BINDIR),$(f:.hs=)) > $(f:.hs=.log);)

blog: hs/$(BLOGLHS) | dirs
	@cp -pu hs/$(BLOGLHS) $(OBJDIR)
	@cp -rpu hs/messages-blog $(BUILDDIR)
	cd $(OBJDIR) && \
	ghc $(BLOGLHS) -o ../bin/$(BLOGLHS:.lhs=) > $(BLOGLHS:.lhs=.log)
