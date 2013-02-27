# Вспомогательные переменные для обработки пробелов в именах
e :=
space := $(e) $(e)

#-
# Исходные файлы
#-
MASTER_TEX := Developing\ Web\ Applications\ with\ Haskell\ and\ Yesod.tex

# Имена исходных файлов TeX содержат пробелы. Поэтому для корректной работы make
# сначала экранируем все пробелы без разбора, а затем снимаем экранирование
# с полезных пробелов между файлами
TEXSRCS := $(wildcard tex/[S012]*.tex)
TEXSRCS := $(subst $(space),\$(space),$(TEXSRCS))
TEXSRCS := $(subst .tex\,.tex,$(TEXSRCS))

# Здесь имена без пробелов - сойдёт и так
HSSRCS := $(wildcard hs/*.hs)
IMAGES := $(wildcard img/*.png)

#-
# Итоговый файл
#-
YESODBOOK := $(MASTER_TEX:.tex=.pdf)

#-
# Строка для запуска TeX
#-
TEX := xelatex
ifdef STRICT
	TEXOPTS := -halt-on-error
else
	TEXOPTS := -interaction=nonstopmode
endif

OUTPUT_DIR ?= tmp
TEXOPTS += -output-directory=$(abspath $(OUTPUT_DIR))

#-
# Цели для сборки
#-
.PHONY: all tmp-dir clean build-examples clean-examples

all: tmp-dir $(YESODBOOK)

$(YESODBOOK): tex/$(MASTER_TEX) $(TEXSRCS) $(HSSRCS) $(IMAGES)
	rm -f $(YESODBOOK)
	cd tex && \
	$(TEX) $(TEXOPTS) $(MASTER_TEX) && \
	$(TEX) $(TEXOPTS) $(MASTER_TEX) && \
	$(TEX) $(TEXOPTS) $(MASTER_TEX) && \
	cd .. && \
	cp $(OUTPUT_DIR)/$(YESODBOOK) .

tmp-dir:
	@mkdir -p $(OUTPUT_DIR)

clean:
	-rm -rf $(OUTPUT_DIR)
	-rm -f $(YESODBOOK)

BINDIR ?= $(OUTPUT_DIR)/bin
OBJDIR ?= $(OUTPUT_DIR)/obj

build-examples:
	@mkdir -p $(BINDIR)
	@mkdir -p $(OBJDIR)
	@cp $(HSSRCS) $(OBJDIR)
	$(foreach f, $(subst hs/,$(OBJDIR)/,$(HSSRCS)), ghc $(f) -o $(subst $(OBJDIR),$(BINDIR),$(f:.hs=)) > $(f:.hs=.log);)

clean-examples:
	-rm -rf $(BINDIR)
	-rm -rf $(OBJDIR)
