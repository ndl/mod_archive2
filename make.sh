#!/bin/sh

erl -pa ../ejabberd/src -pa ebin/tools -s build all -s init stop -noinput
