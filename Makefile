#-*- mode: makefile-gmake -*-
# Copyright (c) 2013-2014 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

PROJECT = quinlan

PLT_APPS = asn1 mnesia syntax_tools observer wx et erts kernel stdlib compiler tools hipe runtime_tools inets webtool crypto public_key ssl

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
        +warn_shadow_vars +warn_obsolete_guard +warn_missing_spec

include erlang.mk
