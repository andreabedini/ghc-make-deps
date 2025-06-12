MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

PKG_NAME        := make-deps
PKG_VERSION     := 0.0.0.1
PKG_ID          := $(PKG_NAME)-$(PKG_VERSION)
DEPS            := base-4.21.0.0-5b43 containers-0.7-40d4 directory-1.3.9.0-cbf1 filepath-1.5.4.0-04d4 ghc-9.12.2-c687 os-string-2.0.7-6e72

HI_SUFFIX			  := dyn_hi

OUTDIR          := out

SRCS 						:= MakeDeps.hs MakeDeps/MakeFile.hs MakeDeps/MakeFile/JSON.hs
OBJS            := $(SRCS:%.hs=%.o)

EXPOSED_MODULES := $(foreach src,$(SRCS),$(subst /,.,$(patsubst %.hs,%,$(src))))

UNIT_ID         := $(PKG_ID)

GHC_VERSION     := $(shell ghc --numeric-version)

HS_LIBNAME      := HS$(UNIT_ID)
LIBNAME				  := lib$(HS_LIBNAME)-ghc$(GHC_VERSION).so

OPTS := -hide-all-packages -clear-package-db -global-package-db $(addprefix -package-id=,$(DEPS))
OPTS += -this-unit-id $(UNIT_ID) -outputdir $(OUTDIR)
OPTS += -fPIC -hisuf $(HI_SUFFIX)

.PHONY: all
all : $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(PKG_ID).conf $(OUTDIR)/package.cache

vpath %.hs src

$(OUTDIR)/$(LIBNAME): $(SRCS)
	@mkdir -p $(@D)
	ghc -shared -dynamic -dynload deploy $(OPTS) -o $@ --make $^

define package_conf
name:                 $(PKG_NAME)
version:              $(PKG_VERSION)
visibility:           public
id:                   $(UNIT_ID)
key:                  $(UNIT_ID)
exposed:              True
exposed-modules:      $(EXPOSED_MODULES)
import-dirs:          $${pkgroot}/$(OUTDIR)
library-dirs:         $${pkgroot}/$(OUTDIR)
library-dirs-static:  $${pkgroot}/$(OUTDIR)
dynamic-library-dirs: $${pkgroot}/$(OUTDIR)
hs-libraries:         $(HS_LIBNAME)
depends:              $(DEPS)
endef

$(OUTDIR)/$(PKG_ID).conf:
	$(file >$@,$(package_conf))

$(OUTDIR)/package.cache: $(OUTDIR)/$(PKG_ID).conf
	ghc-pkg --package-db $(OUTDIR) recache
	ghc-pkg --package-db $(OUTDIR) check

# test: all
# 	ghc -dynamic -package-db $(OUTDIR) --frontend MakeDeps ~/code/haskell/cabal/Cabal-syntax/src/**/*.hs

clean:
	rm -rf $(OUTDIR)
