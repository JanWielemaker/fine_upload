FILES=	web/css/fine-uploader-new.css \
	web/js/fine-uploader.js \
	web/icons/placeholders/waiting-generic.png \
	web/icons/placeholders/not_available-generic.png \
	web/css/loading.gif \
	web/css/edit.gif
DIRS=	web/css web/js web/icons/placeholders
SRC=	node_modules/fine-uploader/fine-uploader

all::

src: $(FILES)

web/css/%: $(SRC)/% $(DIRS)
	cp -p $< $@
web/js/%: $(SRC)/% $(DIRS)
	cp -p $< $@
web/icons/%: $(SRC)/% $(DIRS)
	cp -p $< $@

$(DIRS):
	mkdir -p $@

check::

clean::
distclean:
	rm -f $(FILES)
