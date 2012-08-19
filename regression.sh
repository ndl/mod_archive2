#!/bin/sh

if ./make.sh; then
  if [ "$1" == "xml" ]; then
    erl -pa ebin/test/regression -noinput -sname regression@`hostname -s` -s client eunit_xml_report "." -s init stop
  else
    erl -pa ebin/test/regression -noinput -sname regression@`hostname -s` -s client test -s init stop
  fi
else
  echo "Make failed!"
fi
