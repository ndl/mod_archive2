#!/bin/sh

mkdir -p ebin ebin/test/unit ebin/test/regression ebin/tools
erl -pa ../ejabberd/src -pa ebin/tools -s make all -s init stop -noinput
