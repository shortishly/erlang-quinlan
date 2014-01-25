#-*- mode: makefile-gmake -*-
PROJECT = quinlan

CT_SUITES = example golfing entropy wind baseball sunburn ipcam

ERLC_OPTS = -DDEBUG +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +bin_opt_info #+warn_missing_spec

include erlang.mk
