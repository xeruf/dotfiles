#!/bin/sh -e
for arg; do
	(
	cd $arg
	source ./PKGBUILD
	cpan -D "$(echo "$_pkgname" | sed "s/-/::/g")" | grep --color=none --only-matching "\(^\w\+::\|Installed\|CPAN\).*"
	)
done
